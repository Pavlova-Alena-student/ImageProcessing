namespace ImageProcessing.Tests

open Expecto
open ImageProcessing

module ImageProcessingTests =
    open ImageProcessing.ImageProcessing
    open FSharp.Collections

    let blurFilter = [ Kernel.gaussianBlurKernel ]
    let edgesFilter = [ Kernel.edgesKernel ]
    let sharpenFilter = [ Kernel.sharpenKernel ]
    let edgeDetectFilter = [ Kernel.edgeDetectKernel ]
    let embrossFilter = [ Kernel.embrossEffectKernel ]
    let idFilter = [ Kernel.idKernel ]
    let imgOnePix = Image([| 1uy |], 1, 1, "pix")
    let smallOnesImg = Image(Array.create 36 1uy, 6, 6, "small")

    [<Tests>]
    let tests =
        testList "samples" [
            testList "CPU" [
                testCase "One-pix image"
                <| fun _ ->
                    let one = Image([| 1uy |], 1, 1, "pix")
                    let zero = Image([| 0uy |], 1, 1, "pix")
                    Expect.equal (applyFilters idFilter imgOnePix) one "id filter failed on small image"
                    Expect.equal (applyFilters embrossFilter imgOnePix) one "embross filter failed on small image"

                    Expect.equal
                        (applyFilters edgeDetectFilter imgOnePix)
                        zero
                        "edges (3*3) filter failed on small image"

                    Expect.equal (applyFilters sharpenFilter imgOnePix) one "sharpen filter failed on small image"
                    Expect.equal (applyFilters edgesFilter imgOnePix) zero "edges (5*5) filter failed on small image"
                    Expect.equal (applyFilters blurFilter imgOnePix) one "blur filter failed on small image"
                testCase "Small image"
                <| fun _ ->
                    let one = Image(Array.create 36 1uy, 6, 6, "small")
                    let zero = Image(Array.zeroCreate 36, 6, 6, "small")
                    Expect.equal (applyFilters idFilter smallOnesImg) one "id filter failed on small image"
                    Expect.equal (applyFilters embrossFilter smallOnesImg) one "embross filter failed on small image"

                    Expect.equal
                        (applyFilters edgeDetectFilter smallOnesImg)
                        zero
                        "edges (3*3) filter failed on small image"

                    Expect.equal (applyFilters sharpenFilter smallOnesImg) one "sharpen filter failed on small image"
                    Expect.equal (applyFilters edgesFilter smallOnesImg) zero "edges (5*5) filter failed on small image"
                    Expect.equal (applyFilters blurFilter smallOnesImg) one "blur filter failed on small image"
                testProperty "Small random image"
                <| fun (dataPlus: byte array) ->
                    // creating square image
                    let size: int = int (sqrt (double dataPlus.Length))
                    let data: byte array = Array.take (size * size) dataPlus
                    let img = Image(data, size, size, "random")
                    // filtered image data
                    let id = applyFilters idFilter img
                    let embross = (applyFilters embrossFilter img).Data
                    let edgeDetect = (applyFilters edgeDetectFilter img).Data
                    let sharpen = (applyFilters sharpenFilter img).Data
                    let edges = (applyFilters edgesFilter img).Data
                    let blur = (applyFilters blurFilter img).Data
                    // some filters leaves sum of bytes ALMOST same as original
                    // this applies to filters whose kernels have a sum of elements equal to 1
                    // exception are border elements
                    let accuracy = {
                        absolute = (float size) * 4. + 1.
                        relative = 1
                    }

                    let converter = fun (el: byte) -> int el
                    let originalSum = Array.sumBy converter data |> float
                    Expect.equal id img "id filter failed on random image: it is not the same as original"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter embross |> float)
                        originalSum
                        "embross filter failed on random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter edgeDetect |> float)
                        0
                        "edges (3*3) filter failed on random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter sharpen |> float)
                        originalSum
                        "sharpen filter failed on random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter edges |> float)
                        0
                        "edges (5*5) filter failed on random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter blur |> float)
                        originalSum
                        "blur filter failed on random image"
            ]
            testList "GPU (if available)" [] // TODO: HW2
        ]
