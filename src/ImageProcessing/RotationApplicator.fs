module ImageProcessing.RotationApplicator

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open ImageProcessing.Image

type RotationType =
    | SizeChangingRotation
    | SizeSavingRotation

type Rotation =
    | LeftRotation
    | RightRotation
    | FlipRotation
    | ReflectHorizontally
    | ReflectVertically

    member this.getType =
        match this with
        | LeftRotation -> SizeChangingRotation
        | RightRotation -> SizeChangingRotation
        | FlipRotation -> SizeSavingRotation
        | ReflectHorizontally -> SizeSavingRotation
        | ReflectVertically -> SizeSavingRotation

///  imgH & imgW is new image's height & width
let applyRotation rotation imgW imgH (img: byte[]) =
    let rotatePixel i j =
        let oldI, oldJ =
            match rotation with
            | LeftRotation -> j, imgH - i - 1
            | RightRotation -> imgW - j - 1, i
            | FlipRotation -> imgH - i - 1, imgW - j - 1
            | ReflectHorizontally -> imgH - i - 1, j
            | ReflectVertically -> i, imgW - j - 1

        match rotation.getType with
        | SizeSavingRotation -> img.[oldI * imgW + oldJ]
        | SizeChangingRotation -> img.[oldI * imgH + oldJ]

    Array.mapi (fun i _ -> byte (rotatePixel (i / imgW) (i % imgW))) img

let applyRotations (rotations: Rotation list) (img: Image) =
    let data, width, height =
        List.fold
            (fun (data, w, h) (rotation: Rotation) ->
                if rotation.getType = SizeSavingRotation then
                    let rotated = applyRotation rotation w h data
                    (rotated, w, h)
                else
                    let rotated = applyRotation rotation h w data
                    (rotated, h, w)
            )
            (img.Data, img.Width, img.Height)
            rotations

    Image(data, width, height, img.Name)
