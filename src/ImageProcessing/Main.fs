namespace ImageProcessing

open Brahma.FSharp

module Main =
    let pathToExamples = __SOURCE_DIRECTORY__
    let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

    [<EntryPoint>]
    let main (argv: string array) =
        //let nvidiaDevice =
        //    ClDevice.GetAvailableDevices(platform = Platform.Nvidia) |> Seq.head
        //let intelDevice =
        //    ClDevice.GetAvailableDevices(platform = Platform.Intel) |> Seq.head
        let device = ClDevice.GetFirstAppropriateDevice()
        printfn $"Device: %A{device.Name}"
        let context = ClContext(device)
        let applyFiltersOnAnyDevice = ImageProcessing.applyFiltersGPU context 64
        //let nvContext = ClContext(nvidiaDevice)
        //let applyFiltersOnNvGPU = ImageProcessing.applyFiltersGPU nvContext 64
        //let intelContext = ClContext(intelDevice)
        //let applyFiltersOnIntelGPU = ImageProcessing.applyFiltersGPU intelContext 64
        let filters = [
            ImageProcessing.gaussianBlurKernel
            ImageProcessing.gaussianBlurKernel
            ImageProcessing.edgesKernel
        ]
        //let grayscaleImage = ImageProcessing.loadAs2DArray demoFile
        //let blur = ImageProcessing.applyFilter ImageProcessing.gaussianBlurKernel grayscaleImage
        //let edges = ImageProcessing.applyFilter ImageProcessing.edgesKernel blur
        //let edges =  applyFiltersGPU [ImageProcessing.gaussianBlurKernel; ImageProcessing.edgesKernel] grayscaleImage
        //ImageProcessing.save2DByteArrayAsImage edges "../out/demo_grayscale.jpg"
        printfn $"Files from %s{inputFolder} will be processed"
        let start = System.DateTime.Now

        Streaming.processAllFiles inputFolder "/out/" [
            applyFiltersOnAnyDevice filters (*; applyFiltersOnNvGPU filters; applyFiltersOnIntelGPU filters*)
        ]

        printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
        0
