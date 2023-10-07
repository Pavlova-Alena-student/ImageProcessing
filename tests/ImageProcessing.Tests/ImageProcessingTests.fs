namespace ImageProcessing.Tests

open Expecto
open ImageProcessing
open ImageProcessing.Image
open FilterApplicator

module ImageProcessingTests =
    open ImageProcessing.ImageProcessing
    open FSharp.Collections

    let blurFilter = [ Kernel.gaussianBlurKernel ]
    let upperEdgeFilter = [ Kernel.upperEdgeKernel ]
    let sharpenFilter = [ Kernel.sharpenKernel ]
    let allEdgesFilter = [ Kernel.allEdgesKernel ]
    let embrossFilter = [ Kernel.embrossEffectKernel ]
    let idFilter = [ Kernel.idKernel ]
    let imgPixOfOne = Image([| 1uy |], 1, 1, "pix of 1")
    let smallImgOfOnes = Image(Array.create 36 1uy, 6, 6, "small of 1s")

    let smallImg =
        Image(
            [|
                0uy
                0uy
                0uy
                1uy
                0uy
                0uy
                2uy
                0uy
                0uy
                2uy
                0uy
                0uy
                1uy
                0uy
                0uy
                0uy
            |],
            4,
            4,
            "small diag img for test"
        )

    [<Tests>]
    let tests =
        testList "samples" [
            testList "CPU filters" [
                testCase "One-pix image"
                <| fun _ ->
                    let one = Image([| 1uy |], 1, 1, "pix of 1")
                    let zero = Image([| 0uy |], 1, 1, "pix of 1")
                    Expect.equal (applyFilters idFilter imgPixOfOne) one "id filter failed on a small image"
                    Expect.equal (applyFilters embrossFilter imgPixOfOne) one "embross filter failed on a small image"

                    Expect.equal
                        (applyFilters allEdgesFilter imgPixOfOne)
                        zero
                        "edges (3*3) filter failed on a small image"

                    Expect.equal (applyFilters sharpenFilter imgPixOfOne) one "sharpen filter failed on a small image"

                    Expect.equal
                        (applyFilters upperEdgeFilter imgPixOfOne)
                        zero
                        "edges (5*5) filter failed on a small image"

                    Expect.equal (applyFilters blurFilter imgPixOfOne) one "blur filter failed on a small image"
                testCase "Small smooth image"
                <| fun _ ->
                    let one = Image(Array.create 36 1uy, 6, 6, "small of 1s")
                    let zero = Image(Array.zeroCreate 36, 6, 6, "small of 1s")
                    Expect.equal (applyFilters idFilter smallImgOfOnes) one "id filter failed on a small smooth image"

                    Expect.equal
                        (applyFilters embrossFilter smallImgOfOnes)
                        one
                        "embross filter failed on a small smooth image"

                    Expect.equal
                        (applyFilters allEdgesFilter smallImgOfOnes)
                        zero
                        "edges filter failed on a small smooth image"

                    Expect.equal
                        (applyFilters sharpenFilter smallImgOfOnes)
                        one
                        "sharpen filter failed on a small smooth image"

                    Expect.equal
                        (applyFilters upperEdgeFilter smallImgOfOnes)
                        zero
                        "upper edge filter failed on a small smooth image"

                    Expect.equal
                        (applyFilters blurFilter smallImgOfOnes)
                        one
                        "blur filter failed on a small smooth image"
                testCase "Small diag image"
                <| fun _ ->
                    let embrossEffect = [
                        [ 0uy; 4uy; 3uy; 1uy ]
                        [ 4uy; 4uy; 2uy; 0uy ]
                        [ 3uy; 2uy; 0uy; 0uy ]
                        [ 1uy; 0uy; 0uy; 0uy ]
                    ]

                    let allEdges = [
                        [ 0uy; 0uy; 3uy; 0uy ]
                        [ 0uy; 4uy; 0uy; 3uy ]
                        [ 3uy; 0uy; 4uy; 0uy ]
                        [ 0uy; 3uy; 0uy; 0uy ]
                    ]

                    let sharpen = [
                        [ 0uy; 0uy; 0uy; 5uy - 1uy - 1uy ]
                        [ 0uy; 0uy; 10uy; 0uy ]
                        [ 0uy; 10uy; 0uy; 0uy ]
                        [ 5uy - 1uy - 1uy; 0uy; 0uy; 0uy ]
                    ]

                    let upperEdge = [
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 2uy; 0uy ]
                        [ 0uy; 4uy; 0uy; 0uy ]
                        [ 2uy; 0uy; 0uy; 0uy ]
                    ]

                    let blur = [ // 1uy is too small for it to make impact here
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                    ]

                    let imgDataOf (lst: byte list list) = List.concat lst |> Array.ofList
                    Expect.equal (applyFilters idFilter smallImg).Data smallImg.Data "id filter failed on a small image"

                    Expect.equal
                        (applyFilters embrossFilter smallImg).Data
                        (imgDataOf embrossEffect)
                        "embross filter failed on a small image"

                    Expect.equal
                        (applyFilters allEdgesFilter smallImg).Data
                        (imgDataOf allEdges)
                        "allEdges filter failed on a small image"

                    Expect.equal
                        (applyFilters sharpenFilter smallImg).Data
                        (imgDataOf sharpen)
                        "sharpen filter failed on a small image"

                    Expect.equal
                        (applyFilters upperEdgeFilter smallImg).Data
                        (imgDataOf upperEdge)
                        "upperEdge filter failed on a small image"

                    Expect.equal
                        (applyFilters blurFilter smallImg).Data
                        (imgDataOf blur)
                        "blur filter failed on a small image"
                testProperty "Small random image properties"
                <| fun (dataPlus: byte array) ->
                    // creating square image
                    let size: int = int (sqrt (double dataPlus.Length))
                    let data: byte array = Array.take (size * size) dataPlus
                    let img = Image(data, size, size, "random")
                    // filtered image data
                    let id = applyFilters idFilter img
                    let embross = (applyFilters embrossFilter img).Data
                    let allEdges = (applyFilters allEdgesFilter img).Data
                    let sharpen = (applyFilters sharpenFilter img).Data
                    let upperEdge = (applyFilters upperEdgeFilter img).Data
                    let blur = (applyFilters blurFilter img).Data
                    // some filters leaves sum of bytes ALMOST same as original
                    // this applies to filters whose kernels have a sum of elements equal to 1
                    // exception are border elements
                    let accuracy = {
                        absolute = (float size) * 4. + 1.
                        relative = 1 //if size > 2 then 4. / (float size) else 1
                    }

                    let converter = fun (el: byte) -> int el
                    let originalSum = Array.sumBy converter data |> float
                    Expect.equal id img "id filter failed on a random image: it is not the same as original"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter embross |> float)
                        originalSum
                        "embross filter failed on a random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter allEdges |> float)
                        0
                        "edges (3*3) filter failed on a random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter sharpen |> float)
                        originalSum
                        "sharpen filter failed on a random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter upperEdge |> float)
                        0
                        "edges (5*5) filter failed on a random image"

                    Expect.floatClose
                        accuracy
                        (Array.sumBy converter blur |> float)
                        originalSum
                        "blur filter failed on a random image"
            ]
            testList "GPU (if available) filters" [] // TODO: HW2
            testList "CPU rotations" []
            testList "GPU (if available) rotations" [] // TODO: HW2
        ]
