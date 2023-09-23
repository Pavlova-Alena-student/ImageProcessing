module ImageProcessing.Image

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

[<Struct>]
type Image =
    val Data: array<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        {
            Data = data
            Width = width
            Height = height
            Name = name
        }

let loadAsImage (file: string) =
    let img = Image.Load<L8> file
    let buf = Array.zeroCreate<byte> (img.Width * img.Height)
    img.CopyPixelDataTo(Span<byte> buf)
    Image(buf, img.Width, img.Height, System.IO.Path.GetFileName file)

let saveImage (image: Image) file =
    printfn "saving img pix with data = %A, width = %d, height = %d" image.Data image.Width image.Height
    let img = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)
    printfn "saving img = %A to file %s" img file
    img.Save file
    printfn "saved"
