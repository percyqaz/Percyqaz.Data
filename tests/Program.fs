open Percyqaz.Json

let JSON_Manager = Json(Json.Settings.Default).WithDefaults()

[<Json.AutoCodec(true)>]
type Record =
    {
        X: int
        Y: string list
    }
    static member Default = { X = -1; Y = ["5"] }

do

let cdc = JSON_Manager.GetCodec<Record array>()
printfn "%A" (cdc.To [|Record.Default; { Record.Default with X = 5 }|] |> cdc.FromDefault)