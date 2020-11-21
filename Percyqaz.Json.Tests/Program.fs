module Program = 
    type TestRec = {
        Hello2: string
        world: double
    } with
        static member Default = {
            Hello2 = "hello"
            world = 1.0
        }

    type UnionTest =
    | One
    | Two of (int * bool)
    | Three of string
    | Four of TestRec * TestRec

    let [<EntryPoint>] main _ =
        printfn "%A" (Four ({Hello2 = "wtf"; world = 1.0}, TestRec.Default) |> Percyqaz.Json.toJson |> Percyqaz.Json.Formatting.formatJson |> Percyqaz.Json.fromString<UnionTest>)
        0
