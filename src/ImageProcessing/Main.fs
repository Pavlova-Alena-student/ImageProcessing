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
        | Filter of FilterApplicator.Filter * string
        | GPUFilter of FilterApplicator.Filter * string
        | Rotation of RotationApplicator.Rotation * string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "Get information about this application"
                | Version -> "Version of application"
                | Filter (a, b) ->
                    "Apply filter <filterName> on images in folder <folderName>/input/ and save result to folder <folderName>/out/"
                | GPUFilter (a, b) -> "Same as previous, but process takes place on GPU"
                | Rotation (a, b) ->
                    "Apply rotation/flip/reflection on images in folder <folderName>/input/ and save result to folder <folderName>/out/"

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
                System.IO.Directory.CreateDirectory(outputFolder) |> ignore
                printfn $"Files from %s{inputFolder} will be processed"
                let start = System.DateTime.Now
                Streaming.processAllFiles inputFolder outputFolder [ FilterApplicator.applyFilter filterName ]
                printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
            | None -> parser.PrintUsage() |> printfn "%s"
        elif results.Contains GPUFilter then
            match results.TryGetResult GPUFilter with
            | Some ((filterName, pathToExamples)) ->
                let inputFolder = System.IO.Path.Combine(pathToExamples, "input")
                let outputFolder = System.IO.Path.Combine(pathToExamples, "out")
                System.IO.Directory.CreateDirectory(outputFolder) |> ignore
                let filters = [ filterName.getKernel ]
                let allDevices = ClDevice.GetAvailableDevices()

                for device in allDevices do
                    let outputFolder = System.IO.Path.Combine(outputFolder, string device)
                    System.IO.Directory.CreateDirectory(outputFolder) |> ignore
                    let context = ClContext(device)
                    let applyFiltersOnGPU = FilterApplicator.applyFiltersGPU context 64
                    printfn $"Files from %s{inputFolder} will be processed by %A{device}"
                    let start = System.DateTime.Now
                    Streaming.processAllFiles inputFolder outputFolder [ applyFiltersOnGPU filters ]
                    printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
            | None -> parser.PrintUsage() |> printfn "%s"
        elif results.Contains Rotation then
            match results.TryGetResult Rotation with
            | Some ((rotationName, pathToExamples)) ->
                let inputFolder = System.IO.Path.Combine(pathToExamples, "input")
                let outputFolder = System.IO.Path.Combine(pathToExamples, "out")
                System.IO.Directory.CreateDirectory(outputFolder) |> ignore
                printfn $"Files from %s{inputFolder} will be processed"
                let start = System.DateTime.Now
                Streaming.processAllFiles inputFolder outputFolder [ RotationApplicator.applyRotation rotationName ]
                printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
            | None -> parser.PrintUsage() |> printfn "%s"
        else
            parser.PrintUsage() |> printfn "%s"

        0
