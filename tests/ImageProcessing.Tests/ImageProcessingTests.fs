namespace ImageProcessing.Tests

open Expecto
open ImageProcessing
open ImageProcessing.Image
open FilterApplicator
open RotationApplicator

module ImageProcessingTests =
    open ImageProcessing.ImageProcessing
    open FSharp.Collections
    // Generates square image
    let imgGen: FsCheck.Gen<Image> =
        let rec imgGen' mSize =
            let contentGen: FsCheck.Gen<byte array> =
                FsCheck.Gen.elements [ 0uy .. 127uy ]
                |> FsCheck.Gen.arrayOfLength (mSize * mSize)

            FsCheck.Gen.map (fun content -> Image(content, mSize, mSize, "square generated image")) contentGen

        FsCheck.Gen.sized imgGen'

    type imgGenerator =
        static member IList() = FsCheck.Arb.fromGen imgGen

        static member Register() =
            FsCheck.Arb.register<imgGenerator> () |> ignore

    let imgGenConfig =
        { FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<imgGenerator> ]
        }
    // test "images" for tests
    let imgPixOfOne = Image([| 1uy |], 1, 1, "pix of 1")
    let smallImgOfOnes = Image(Array.create 36 1uy, 6, 6, "small of 1s")

    let smallImg =
        Image(
            Array.ofList (
                List.concat [
                    [ 0uy; 0uy; 0uy; 1uy ]
                    [ 0uy; 0uy; 2uy; 0uy ]
                    [ 0uy; 2uy; 0uy; 1uy ]
                    [ 1uy; 0uy; 0uy; 0uy ]
                ]
            ),
            4,
            4,
            "small diag img for testing"
        )

    [<Tests>]
    let tests =
        testList "samples" [
            testList "CPU filters" [
                testCase "One-pix image"
                <| fun _ ->
                    let one = Image([| 1uy |], 1, 1, "pix of 1")
                    let zero = Image([| 0uy |], 1, 1, "pix of 1")
                    Expect.equal (applyFilter Filter.Id imgPixOfOne) one "id filter failed on a 1x1 image"

                    Expect.equal
                        (applyFilter Filter.EmbrossEffect imgPixOfOne)
                        one
                        "embross filter failed on a 1x1 image"

                    Expect.equal
                        (applyFilter Filter.AllEdges imgPixOfOne)
                        zero
                        "edges (3*3) filter failed on a 1x1 image"

                    Expect.equal (applyFilter Filter.Sharpen imgPixOfOne) one "sharpen filter failed on a 1x1 image"

                    Expect.equal
                        (applyFilter Filter.UpperEdge imgPixOfOne)
                        zero
                        "edges (5*5) filter failed on a 1x1 image"

                    Expect.equal (applyFilter Filter.GaussianBlur imgPixOfOne) one "blur filter failed on a 1x1 image"
                testCase "Small smooth image"
                <| fun _ ->
                    let one = Image(Array.create 36 1uy, 6, 6, "small of 1s")
                    let zero = Image(Array.zeroCreate 36, 6, 6, "small of 1s")
                    Expect.equal (applyFilter Filter.Id smallImgOfOnes) one "id filter failed on a small smooth image"

                    Expect.equal
                        (applyFilter Filter.EmbrossEffect smallImgOfOnes)
                        one
                        "embross filter failed on a small smooth image"

                    Expect.equal
                        (applyFilter Filter.AllEdges smallImgOfOnes)
                        zero
                        "edges filter failed on a small smooth image"

                    Expect.equal
                        (applyFilter Filter.Sharpen smallImgOfOnes)
                        one
                        "sharpen filter failed on a small smooth image"

                    Expect.equal
                        (applyFilter Filter.UpperEdge smallImgOfOnes)
                        zero
                        "upper edge filter failed on a small smooth image"

                    Expect.equal
                        (applyFilter Filter.GaussianBlur smallImgOfOnes)
                        one
                        "blur filter failed on a small smooth image"
                testCase "Small diag image"
                <| fun _ ->
                    let embrossEffect = [
                        [ 0uy; 4uy; 3uy; 1uy ]
                        [ 4uy; 4uy; 4uy; 0uy ]
                        [ 3uy; 2uy; 0uy; 0uy ]
                        [ 1uy; 0uy; 0uy; 0uy ]
                    ]

                    let allEdges = [
                        [ 0uy; 0uy; 3uy; 0uy ]
                        [ 0uy; 4uy; 0uy; 4uy ]
                        [ 3uy; 0uy; 5uy; 0uy ]
                        [ 0uy; 3uy; 0uy; 1uy ]
                    ]

                    let sharpen = [
                        [ 0uy; 0uy; 0uy; 5uy - 1uy - 1uy ]
                        [ 0uy; 0uy; 10uy; 0uy ]
                        [ 0uy; 10uy; 0uy; 5uy - 1uy ]
                        [ 5uy - 1uy - 1uy; 0uy; 0uy; 0uy ]
                    ]

                    let upperEdge = [
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 2uy; 0uy ]
                        [ 0uy; 4uy; 0uy; 1uy ]
                        [ 2uy; 0uy; 0uy; 0uy ]
                    ]

                    let blur = [ // 1uy and 2uy are too small for it to make impact here
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                        [ 0uy; 0uy; 0uy; 0uy ]
                    ]

                    let imgDataOf (lst: byte list list) = List.concat lst |> Array.ofList
                    Expect.equal (applyFilter Filter.Id smallImg).Data smallImg.Data "id filter failed on a small image"

                    Expect.equal
                        (applyFilter Filter.EmbrossEffect smallImg).Data
                        (imgDataOf embrossEffect)
                        "embross filter failed on a small image"

                    Expect.equal
                        (applyFilter Filter.AllEdges smallImg).Data
                        (imgDataOf allEdges)
                        "allEdges filter failed on a small image"

                    Expect.equal
                        (applyFilter Filter.Sharpen smallImg).Data
                        (imgDataOf sharpen)
                        "sharpen filter failed on a small image"

                    Expect.equal
                        (applyFilter Filter.UpperEdge smallImg).Data
                        (imgDataOf upperEdge)
                        "upperEdge filter failed on a small image"

                    Expect.equal
                        (applyFilter Filter.GaussianBlur smallImg).Data
                        (imgDataOf blur)
                        "blur filter failed on a small image"
            ]
            testList "GPU (if available) filters" [] // TODO: HW2
            testList "CPU rotations" [
                testCase "Small image rotations"
                <| fun _ ->
                    let rightRotated =
                        Image(
                            Array.ofList (
                                List.concat [
                                    [ 1uy; 0uy; 0uy; 0uy ]
                                    [ 0uy; 2uy; 0uy; 0uy ]
                                    [ 0uy; 0uy; 2uy; 0uy ]
                                    [ 0uy; 1uy; 0uy; 1uy ]
                                ]
                            ),
                            4,
                            4,
                            "small diag img for testing"
                        )

                    let leftRotated =
                        Image(
                            Array.ofList (
                                List.concat [
                                    [ 1uy; 0uy; 1uy; 0uy ]
                                    [ 0uy; 2uy; 0uy; 0uy ]
                                    [ 0uy; 0uy; 2uy; 0uy ]
                                    [ 0uy; 0uy; 0uy; 1uy ]
                                ]
                            ),
                            4,
                            4,
                            "small diag img for testing"
                        )

                    let flipRotated =
                        Image(
                            Array.ofList (
                                List.concat [
                                    [ 0uy; 0uy; 0uy; 1uy ]
                                    [ 1uy; 0uy; 2uy; 0uy ]
                                    [ 0uy; 2uy; 0uy; 0uy ]
                                    [ 1uy; 0uy; 0uy; 0uy ]
                                ]
                            ),
                            4,
                            4,
                            "small diag img for testing"
                        )

                    let horizontallyReflected =
                        Image(
                            Array.ofList (
                                List.concat [
                                    [ 1uy; 0uy; 0uy; 0uy ]
                                    [ 0uy; 2uy; 0uy; 1uy ]
                                    [ 0uy; 0uy; 2uy; 0uy ]
                                    [ 0uy; 0uy; 0uy; 1uy ]
                                ]
                            ),
                            4,
                            4,
                            "small diag img for testing"
                        )

                    let verticallyReflected =
                        Image(
                            Array.ofList (
                                List.concat [
                                    [ 1uy; 0uy; 0uy; 0uy ]
                                    [ 0uy; 2uy; 0uy; 0uy ]
                                    [ 1uy; 0uy; 2uy; 0uy ]
                                    [ 0uy; 0uy; 0uy; 1uy ]
                                ]
                            ),
                            4,
                            4,
                            "small diag img for testing"
                        )

                    Expect.equal
                        (applyRotation Rotation.RightRotation smallImg)
                        rightRotated
                        "rotating clockwise failed on small test image"

                    Expect.equal
                        (applyRotation Rotation.LeftRotation smallImg)
                        leftRotated
                        "rotating anticlockwise failed on small test image"

                    Expect.equal
                        (applyRotation Rotation.FlipRotation smallImg)
                        flipRotated
                        "flipping failed on small test image"

                    Expect.equal
                        (applyRotation Rotation.ReflectHorizontally smallImg)
                        horizontallyReflected
                        "reflecting horizontally failed on small test image"

                    Expect.equal
                        (applyRotation Rotation.ReflectVertically smallImg)
                        verticallyReflected
                        "reflecting vertically failed on small test image"
                testPropertyWithConfig imgGenConfig "Small random image properties"
                <| fun (img: Image) ->
                    // rotated images
                    let toRight i = applyRotation Rotation.RightRotation i
                    let toLeft i = applyRotation Rotation.LeftRotation i
                    let flip i = applyRotation Rotation.FlipRotation i

                    let refH i =
                        applyRotation Rotation.ReflectHorizontally i

                    let refV i =
                        applyRotation Rotation.ReflectVertically i
                    // “Different paths, same destination” method
                    Expect.equal
                        (toLeft img |> toLeft)
                        (flip img)
                        "rotating to left twice is not the same as flipping original image"

                    Expect.equal
                        (toRight img |> toRight)
                        (flip img)
                        "rotating to right twice is not the same as flipping original image"

                    Expect.equal
                        (refH img |> refV)
                        (flip img)
                        "reflecting horizontally&vertically is not the same as flipping original image"
                    // “There and back again” method
                    Expect.equal
                        (toLeft img |> toRight)
                        img
                        "rotating to left, then to right is not the same as taking original image"

                    Expect.equal
                        (toRight img |> toLeft)
                        img
                        "rotating to right, then to left is not the same as taking original image"

                    Expect.equal (flip img |> flip) img "flipping twice is not the same as taking original image"

                    Expect.equal
                        (refH img |> refH)
                        img
                        "reflecting horizontally twice is not the same as taking original image"

                    Expect.equal
                        (refV img |> refV)
                        img
                        "reflecting vertically twice is not the same as taking original image"
                    // “Some things never change” method
                    let sum (img: Image) =
                        Array.fold (fun partSum pix -> partSum + int pix) 0 img.Data

                    for r in
                        [
                            Rotation.LeftRotation
                            Rotation.RightRotation
                            Rotation.FlipRotation
                            Rotation.ReflectHorizontally
                            Rotation.ReflectVertically
                        ] do
                        Expect.equal (applyRotation r img |> sum) (sum img) "rotation changed sum of all pixels"
                    // “Hard to prove, easy to verify” method
                    if img.Width > 1 then
                        Expect.equal
                            (toRight img).Data.[2 * img.Width - 1]
                            img.Data.[1]
                            "right rotation didn't rotate pix #[0][1]"

                        Expect.equal
                            (toLeft img).Data.[(img.Height - 2) * img.Width]
                            img.Data.[1]
                            "left rotation didn't rotate pix #[0][1]"

                        Expect.equal
                            (flip img).Data.[img.Height * img.Width - 2]
                            img.Data.[1]
                            "flip rotation didn't rotate pix #[0][1]"

                        Expect.equal
                            (refH img).Data.[(img.Height - 1) * img.Width + 1]
                            img.Data.[1]
                            "horizontal reflection didn't rotate pix #[0][1]"

                        Expect.equal
                            (refV img).Data.[img.Width - 2]
                            img.Data.[1]
                            "vertical reflection didn't rotate pix #[0][1]"
            ]
            testList "GPU (if available) rotations" [] // TODO: HW2
        ]
