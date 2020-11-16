module Program = 
    let [<EntryPoint>] main _ =
        printfn "%A" (("Hello", 0.0, [1; 2; 3; 4]) |> Percyqaz.Json.Mapping.toJson)
        0
