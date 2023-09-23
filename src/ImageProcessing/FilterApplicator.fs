module ImageProcessing.FilterApplicator

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open ImageProcessing.Image

let applyFilter (filter: float32[][]) imgW imgH (img: byte[]) =
    let filterD = (Array.length filter) / 2
    let filter = Array.concat filter

    let processPixel px py =
        let dataToHandle = [|
            for i in px - filterD .. px + filterD do
                for j in py - filterD .. py + filterD do
                    if i < 0 || i >= imgH || j < 0 || j >= imgW then
                        float32 img.[px * imgW + py]
                    else
                        float32 img.[i * imgW + j]
        |]

        Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

    Array.mapi (fun i _ -> byte (processPixel (i / imgW) (i % imgW))) img

let applyFilters (filters: list<float32[][]>) (img: Image) =
    Image(
        List.fold (fun data filter -> applyFilter filter img.Width img.Height data) img.Data filters,
        img.Width,
        img.Height,
        img.Name
    )

let applyFilterGPUKernel (clContext: ClContext) localWorkSize =
    let kernel =
        <@
            fun (r: Range1D) (img: ClArray<_>) imgW imgH (filter: ClArray<_>) filterD (result: ClArray<_>) ->
                let p = r.GlobalID0
                let pw = p % imgW
                let ph = p / imgW
                let mutable res = 0.0f

                for i in ph - filterD .. ph + filterD do
                    for j in pw - filterD .. pw + filterD do
                        let mutable d = 0uy

                        if i < 0 || i >= imgH || j < 0 || j >= imgW then
                            d <- img.[p]
                        else
                            d <- img.[i * imgW + j]

                        let f = filter.[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                        res <- res + (float32 d) * f

                result.[p] <- byte (int res)
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<_>) (filter: ClArray<float32>) filterD (img: ClArray<byte>) imgH imgW (result: ClArray<_>) ->
        let ndRange = Range1D.CreateValid(imgH * imgW, localWorkSize)
        let kernel = kernel.GetKernel()
        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img imgW imgH filter filterD result))
        commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
        result

let applyFiltersGPU (clContext: ClContext) localWorkSize =
    let kernel = applyFilterGPUKernel clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (filters: list<float32[][]>) (img: Image) ->
        let mutable input =
            clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

        let mutable output =
            clContext.CreateClArray(
                img.Data.Length,
                HostAccessMode.NotAccessible,
                allocationMode = AllocationMode.Default
            )

        for filter in filters do
            let filter = Array.concat filter
            let filterD = (Array.length filter) / 2

            let clFilter =
                clContext.CreateClArray<_>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

            let oldInput = input
            input <- kernel queue clFilter filterD input img.Height img.Width output
            output <- oldInput
            queue.Post(Msg.CreateFreeMsg clFilter)

        let result = Array.zeroCreate (img.Height * img.Width)
        let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(input, result, ch))
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)
        Image(result, img.Width, img.Height, img.Name)
