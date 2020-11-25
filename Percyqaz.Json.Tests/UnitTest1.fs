module Percyqaz.Json.Tests

open Percyqaz.Json
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

type TestType = string * float * int list

[<Test>]
let Test1 () =
    let v = ("Hello", (), true)
    sprintf "%A" (v |> Json.toJson)
    |> Assert.Fail
    //Assert.AreEqual(v, Percyqaz.Json.Mapping.fromJson(Unchecked.defaultof<TestType>, v |> Percyqaz.Json.Mapping.toJson))
