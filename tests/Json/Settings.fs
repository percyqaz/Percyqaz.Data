namespace Percyqaz.Data.Tests.Json

open System
open NUnit.Framework
open Percyqaz.Data

open Helpers

[<TestFixture>]
type ``3: Settings tests``() =

    [<Test>] member this.AllowNullArrays() =
                let allow = Json({ Json.Settings.Default with AllowNullArrays = true }).WithDefaults()
                let disallow = Json({ Json.Settings.Default with AllowNullArrays = false }).WithDefaults()

                round_trip allow (null : string array)
                try 
                    round_trip disallow (null : string array)
                    Assert.Fail()
                with err -> printfn "%s" err.Message

    [<Test>] member this.AllowNullStrings() =
                let allow = Json({ Json.Settings.Default with AllowNullStrings = true }).WithDefaults()
                let disallow = Json({ Json.Settings.Default with AllowNullStrings = false }).WithDefaults()
    
                round_trip allow (null : string)
                try 
                    round_trip disallow (null : string)
                    Assert.Fail()
                with err -> printfn "%s" err.Message
        
    [<Test>] member this.EnumsAsStrings() =
                let strings = Json({ Json.Settings.Default with EncodeEnumsAsStrings = true }).WithDefaults()
                let values = Json({ Json.Settings.Default with EncodeEnumsAsStrings = false }).WithDefaults()

                Assert.AreEqual("1", values.ToString Enum.One)
                Assert.AreEqual("\"One\"", strings.ToString Enum.One)

    [<Test>] member this.FormatExpandArrays() =
                let expand = Json({ Json.Settings.Default with FormatExpandArrays = true }).WithDefaults()
                let no_expand = Json({ Json.Settings.Default with FormatExpandArrays = false }).WithDefaults()

                Assert.AreEqual("[2, 4, 8]", no_expand.ToString [2; 4; 8])
                Assert.AreEqual("[\n    2, \n    4, \n    8\n]", expand.ToString [2; 4; 8])

    [<Test>] member this.FormatExpandObjects() =
                let expand = Json({ Json.Settings.Default with FormatExpandObjects = true }).WithDefaults()
                let no_expand = Json({ Json.Settings.Default with FormatExpandObjects = false }).WithDefaults()
    
                Assert.AreEqual("""{"X": 2, "Y": 4}""", no_expand.ToString { Record.X = 2L; Y = 4.0f })
                Assert.AreEqual("{\n    \"X\": 2, \n    \"Y\": 4\n}", expand.ToString { Record.X = 2L; Y = 4.0f })