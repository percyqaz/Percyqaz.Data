open Percyqaz.Json

let JSON_Manager = Json(Json.Settings.Default).WithDefaults()

do

let cdc = JSON_Manager.GetCodec<(byte option * int) array>()
printfn "%A" (cdc.To [|Some 7uy, 5; None, System.Int32.MinValue|] |> cdc.From)