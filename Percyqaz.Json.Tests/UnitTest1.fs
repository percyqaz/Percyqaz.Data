module Percyqaz.Json.Tests

open NUnit.Framework

type Test<'A>() =
    member this.x: 'A option = None

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreNotEqual(typeof<Test<int>>, typeof<Test<string>>)
