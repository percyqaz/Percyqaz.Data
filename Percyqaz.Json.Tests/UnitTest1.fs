module Percyqaz.Json.Tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

type TestType = string * float * int list

[<Test>]
let Test1 () =
    let v = ("Hello", 0.0, [1; 2; 3; 4])
    sprintf "%A" (v |> Percyqaz.Json.Mapping.toJson)
    |> Assert.Fail
    //Assert.AreEqual(v, Percyqaz.Json.Mapping.fromJson(Unchecked.defaultof<TestType>, v |> Percyqaz.Json.Mapping.toJson))
