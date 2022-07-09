namespace Percyqaz.Json.Tests

open System
open NUnit.Framework
open Percyqaz.Json

open Helpers

[<TestFixture>]
type ``4: Special Deserialisation Scenarios``() =
        
    let env = Json({ Json.Settings.Default with FormatExpandObjects = false }).WithDefaults()
    
    [<Test>] member this.Record_Defaults() =
                match env.FromString<Record> "{}" with
                | Ok v -> Assert.AreEqual(env.Default<Record>(), v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)
                
                match env.FromString<RecordWithDefault> "{}" with
                | Ok v -> Assert.AreEqual(env.Default<RecordWithDefault>(), v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)

    [<Test>] member this.Record_Partial() =
                match env.FromString<RecordWithDefault> "{\"X\": 99}" with
                | Ok v -> Assert.AreEqual({ RecordWithDefault.Default with X = 99L }, v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)

    [<Test>] member this.Primitive_Defaults() =
                match env.FromString<RecordPrimitives> "{}" with
                | Ok v -> Assert.AreEqual(env.Default<RecordPrimitives>(), v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)
    
    [<Test>] member this.Record_RequiresMembers() =
                match env.FromString<RecordNoDefaults> "{}" with
                | Ok v -> Assert.Fail("Expected fail due to required members not being provided")
                | Error err -> printfn "%O" err

    [<Test>] member this.Enum_NamedValue() =
                match env.FromString<Tests.Enum> "1" with
                | Ok v -> Assert.AreEqual(Enum.One, v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)
    
    [<Test>] member this.Char_FirstLetterOfString() =
                match env.FromString<char> "\"Hello world\"" with
                | Ok v -> Assert.AreEqual('H', v)
                | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)