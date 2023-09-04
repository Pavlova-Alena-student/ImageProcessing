module Kernel

let gaussianBlurKernel =
    [|
        [| 1; 4; 6; 4; 1 |]
        [| 4; 16; 24; 16; 4 |]
        [| 6; 24; 36; 24; 6 |]
        [| 4; 16; 24; 16; 4 |]
        [| 1; 4; 6; 4; 1 |]
    |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))

let edgesKernel =
    [|
        [| 0; 0; -1; 0; 0 |]
        [| 0; 0; -1; 0; 0 |]
        [| 0; 0; 2; 0; 0 |]
        [| 0; 0; 0; 0; 0 |]
        [| 0; 0; 0; 0; 0 |]
    |]
    |> Array.map (Array.map float32)
