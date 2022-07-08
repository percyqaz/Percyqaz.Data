namespace Percyqaz.Json.Tests

open System
open NUnit.Framework
open Percyqaz.Json

open Helpers

[<TestFixture>]
type ``1: Primitive Round Trips``() =
    
    let env = Json(Json.Settings.Default).WithDefaults()

    let (~&) (xs: 'T list) = List.iter (round_trip env) xs

    [<Test>] member this.String() = &[""; "Hello"; "\n§\r\t\\😋"]
    [<Test>] member this.Char() = &[' '; '\n'; '§'; '\r']

    [<Test>] member this.UInt8() = &[0uy; 65uy; 255uy]
    [<Test>] member this.Int8() = &[0y; 65y; -127y]
    [<Test>] member this.UInt16() = &[0us; 65us; 65535us]
    [<Test>] member this.Int16() = &[0s; -32767s; 32767s]
    [<Test>] member this.UInt32() = &[0u; 2378622171u]
    [<Test>] member this.Int32() = &[0; Int32.MinValue; Int32.MaxValue]
    [<Test>] member this.UInt64() = &[0UL; UInt64.MaxValue]
    [<Test>] member this.Int64() = &[0L; Int64.MinValue; Int64.MaxValue]