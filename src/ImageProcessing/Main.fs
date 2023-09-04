namespace ImageProcessing

open Brahma.FSharp
open System.Reflection

module AssemblyInfo =
    let metaDataValue (mda: AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly: Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find (fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate" |> getMetaDataAttribute assembly |> metaDataValue

    let getGitHash assembly =
        "GitHash" |> getMetaDataAttribute assembly |> metaDataValue

    let getVersion assembly =
        "AssemblyVersion" |> getMetaDataAttribute assembly |> metaDataValue

    let assembly = lazy (Assembly.GetEntryAssembly())

    let printVersion () =
        let version = assembly.Force().GetName().Version
        printfn "%A" version

    let printInfo () =
        let assembly = assembly.Force()
        let name = assembly.GetName()
        let version = assembly.GetName().Version
        let releaseDate = getReleaseDate assembly
        let githash = getGitHash assembly
        printfn "%s - %A - %s - %s" name.Name version releaseDate githash

module Main =
    open Argu

    type CLIArguments =
        | Info
        | Version
        | FavoriteColor of string // Look in App.config
        | Filter of string * string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "More detailed information"
                | Version -> "Version of application"
                | FavoriteColor _ -> "Favorite color"
                | Filter (a, b) -> "Apply filter %s{a} on images in folder %s{b}/input/"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "ImageProcessing")
        let results = parser.Parse(argv)

        if results.Contains Version then
            AssemblyInfo.printVersion ()
        elif results.Contains Info then
            AssemblyInfo.printInfo ()
        elif results.Contains Filter then
            match results.TryGetResult Filter with
            | Some ((filterName, pathToExamples)) ->
                let inputFolder = System.IO.Path.Combine(pathToExamples, "input")
                let outputFolder = System.IO.Path.Combine(pathToExamples, "out")
                //let nvidiaDevice =
                //    ClDevice.GetAvailableDevices(platform = Platform.Nvidia) |> Seq.head
                //let intelDevice =
                //    ClDevice.GetAvailableDevices(platform = Platform.Intel) |> Seq.head
                (*let device = ClDevice.GetFirstAppropriateDevice()
                printfn $"Device: %A{device.Name}"
                let context = ClContext(device)
                let applyFiltersOnAnyDevice = ImageProcessing.applyFiltersGPU context 64*)
                //let nvContext = ClContext(nvidiaDevice)
                //let applyFiltersOnNvGPU = ImageProcessing.applyFiltersGPU nvContext 64
                //let intelContext = ClContext(intelDevice)
                //let applyFiltersOnIntelGPU = ImageProcessing.applyFiltersGPU intelContext 64
                let filters = [ Kernel.gaussianBlurKernel; Kernel.gaussianBlurKernel; Kernel.edgesKernel ]
                //let blur = ImageProcessing.applyFilter ImageProcessing.gaussianBlurKernel grayscaleImage
                //let edges = ImageProcessing.applyFilter ImageProcessing.edgesKernel blur
                //let edges =  applyFiltersGPU [ImageProcessing.gaussianBlurKernel; ImageProcessing.edgesKernel] grayscaleImage
                printfn $"Files from %s{inputFolder} will be processed"
                let start = System.DateTime.Now

                Streaming.processAllFiles inputFolder outputFolder [
                    ImageProcessing.applyFilters
                        filters (*applyFiltersOnAnyDevice filters (*; applyFiltersOnNvGPU filters; applyFiltersOnIntelGPU filters*)*)
                ]

                printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
            | None -> parser.PrintUsage() |> printfn "%s"
        else
            parser.PrintUsage() |> printfn "%s"

        0
