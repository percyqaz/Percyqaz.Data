open Percyqaz.Json

let JSON_Manager = Json(Json.Settings.Default).WithDefaults()

[<Json.AutoCodec(true)>]
type Record =
    {
        X: int
        Y: string list
    }
    static member Default = { X = -1; Y = ["5"] }

[<Json.AutoCodec(true)>]
type Union =
    | A
    | B of Record
    | C of struct (int * int * byte)

do

let cdc = JSON_Manager.GetCodec<Union array>()
printfn "%A" (cdc.To [|B Record.Default; B { Record.Default with X = 5 }; A; C (struct (5, 5, 7uy))|] |> cdc.FromDefault)