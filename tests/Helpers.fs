namespace Percyqaz.Json.Tests

open Percyqaz.Json
open NUnit.Framework

module Helpers =

    let inline round_trip<'T> (env: Json) (x: 'T) =
        match env.ToString x |> fun json -> printfn "%s" json; json |> env.FromString<'T> with
        | Ok(y) -> Assert.AreEqual(x, y)
        | Error(err) -> Assert.Fail("Failure during round trip", err)