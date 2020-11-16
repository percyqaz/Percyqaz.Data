module Program = 
    let [<EntryPoint>] main _ =
        printfn "%A" (("Hello", (), false) |> Percyqaz.Json.Mapping.toJson |> Percyqaz.Json.Mapping.fromJson<string * unit * bool>)
        0
