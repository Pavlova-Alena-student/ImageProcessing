module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open ImageProcessing.Image
open ImageProcessing.FilterApplicator
open ImageProcessing.RotationApplicator

type ImageTransformation =
    | Rotation of Rotation
    | Filter of Filter

let applyTransformations (transformations: ImageTransformation list) (img: Image) =
    List.fold
        (fun img (transformation: ImageTransformation) ->
            match transformation with
            | Rotation r -> applyRotation r img
            | Filter f -> applyFilter f img
        )
        img
        transformations
