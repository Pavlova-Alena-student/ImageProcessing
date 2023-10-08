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
let applyRotationOnByteArray rotation imgW imgH (img: byte[]) =
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

    Array.mapi (fun i _ -> rotatePixel (i / imgW) (i % imgW)) img

let applyRotation (rotation: Rotation) (img: Image) =
    let data, width, height =
        if rotation.getType = SizeSavingRotation then
            let rotated = applyRotationOnByteArray rotation img.Width img.Height img.Data
            (rotated, img.Width, img.Height)
        else
            let rotated = applyRotationOnByteArray rotation img.Height img.Width img.Data
            (rotated, img.Height, img.Width)

    Image(data, width, height, img.Name)
