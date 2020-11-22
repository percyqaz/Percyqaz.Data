module Program = 

    type EnumTest =
    | One = 1
    | Two = 2
    | Three = 4
    | Four = 8

    type TestRec2 = {
        A: string
        B: int
    } with
        static member Default = {
            A = ""
            B = 0
        }

    type TestRec = {
        Hello2: string
        world: double
        Rec: TestRec2
        Field: EnumTest
    } with
        static member Default = {
            Hello2 = "hello"
            world = 1.0
            Field = EnumTest.Three ||| EnumTest.Four
            Rec = { A = "h"; B = 1 }
        }

    type UnionTest =
    | One
    | Two of (int * bool)
    | Three of string
    | Four of TestRec * TestRec

    let idPrint x =
        printfn "%A" x
        id x

    let [<EntryPoint>] main _ =
        printfn "%A" (Four ({Hello2 = "wtf"; world = 1.0; Field = EnumTest.Two; Rec = TestRec2.Default}, TestRec.Default) |> Percyqaz.Json.toJson |> Percyqaz.Json.Formatting.formatJson |> idPrint |> Percyqaz.Json.fromString<UnionTest>)
        printfn "%A" ("""{"Four": [{}, {}]}""" |> Percyqaz.Json.fromString<UnionTest>)
        0
