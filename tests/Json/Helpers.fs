namespace Percyqaz.Data.Tests.Json

open Percyqaz.Data
open NUnit.Framework

module Helpers =

    let inline round_trip<'T> (env: Json) (x: 'T) =
        match
            env.ToString x
            |> fun json ->
                printfn "%s" json
                json |> env.FromString<'T>
        with
        | Ok(y) -> Assert.AreEqual(x, y)
        | Error(err) -> Assert.Fail("Failure during round trip", err)

    let inline round_trip_reftype<'T, 'U> (env: Json) (comparison: 'T -> 'U) (x: 'T) =
        match
            env.ToString x
            |> fun json ->
                printfn "%s" json
                json |> env.FromString<'T>
        with
        | Ok(y) -> Assert.AreEqual(comparison x, comparison y)
        | Error(err) -> Assert.Fail("Failure during round trip", err)
