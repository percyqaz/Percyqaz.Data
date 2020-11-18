module Program = 
    let [<EntryPoint>] main _ =
        printfn "%s" ({|hello = "\n\t\u000E"; num = 5;  unitMember = ()|} |> Percyqaz.Json.toJson |> Percyqaz.Json.Formatting.formatJson)
        0
