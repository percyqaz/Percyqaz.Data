open Percyqaz.Json

module Program = 

    type EnumTest =
    | One = 1
    | Two = 2
    | Three = 4
    | Four = 8

    type TupleTest = EnumTest array * (int * string) * string list * float array

    type RecordTest = {
        A: TupleTest
        [<Json.Required>]
        B: Map<string, UnionTest>
        C: UnionTest
    } with
        static member Default = {
            A = ([|EnumTest.One; EnumTest.Two ||| EnumTest.Three|], (0, "EEEE"), [], [||])
            B = Map.ofList [("Hello", One); ("World", Two (5, true))]
            C =
                let tup = ([||], (1, "A"), [], [||])
                Four ({A = tup; B = Map.empty; C = Three "AAAA"}, {A = tup; B = Map.empty; C = Three "DDDD"})
        }

    and UnionTest =
    | One
    | Two of (int * bool)
    | Three of string
    | Four of RecordTest * RecordTest

    type POCOTest<'T>(defaultValue: 'T, value: 'T) =
        member this.Value = value
        member this.DefaultValue = defaultValue
        static member Pickler: Json.Mapping.JsonPickler<POCOTest<'T>> =
            let tP = Json.Mapping.getPickler<'T>()
            Json.Mapping.mkPickler
                (fun (o: POCOTest<'T>) -> tP.Encode(o.Value))
                (fun (o: POCOTest<'T>) json -> tP.Decode(o.DefaultValue)(json) |> Json.JsonResult.map (fun v -> POCOTest(o.DefaultValue, v)))

    let idPrint x =
        printfn "%A" x
        id x

    let [<EntryPoint>] main _ =
        printfn "%A" (RecordTest.Default |> Json.toJson |> Json.Formatting.formatJson |> idPrint |> Json.fromString<RecordTest>)
        printfn "%A" ("""{"Four": [{}, {}]}""" |> Json.fromString<UnionTest>)
        printfn "%A" (POCOTest(3,5) |> Json.toString)
        0
