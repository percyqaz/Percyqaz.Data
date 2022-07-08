open Percyqaz.Json

let JSON_Manager = Json(Json.Settings.Default).WithDefaults()

type Enum =
    | A = 0uy
    | B = 1uy
    | C = 2uy

[<Json.AutoCodec(true)>]
[<Struct>]
type Record =
    {
        X: Enum
        Y: string list
    }
    static member Default = { X = LanguagePrimitives.EnumOfValue 8uy; Y = ["5"] }

[<Json.AutoCodec>]
type Union =
    | A
    | B of Record
    | C of struct (int * int * byte)

do

let cdc = JSON_Manager.GetCodec<Union array>()
printfn "%A" (cdc.To [|B Record.Default; A; C (struct (5, 5, 7uy))|])