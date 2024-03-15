namespace Percyqaz.Data.Tests.Json

open System
open NUnit.Framework
open Percyqaz.Data

open Helpers

[<TestFixture>]
type ``1: Primitive Round Trips``() =

    let env =
        Json(
            { Json.Settings.Default with
                FormatExpandObjects = false
            }
        )
            .WithDefaults()

    let (~&) (xs: 'T list) = List.iter (round_trip env) xs

    [<Test>]
    member this.Unit() = &[ () ]

    [<Test>]
    member this.Bool() = &[ true; false ]

    [<Test>]
    member this.String() = &[ ""; "Hello"; "\n§\r\t\\😋" ]

    [<Test>]
    member this.Char() = &[ ' '; '\n'; '§'; '\r' ]

    [<Test>]
    member this.UInt8() = &[ 0uy; 65uy; 255uy ]

    [<Test>]
    member this.Int8() = &[ 0y; 65y; -127y ]

    [<Test>]
    member this.UInt16() = &[ 0us; 65us; 65535us ]

    [<Test>]
    member this.Int16() = &[ 0s; -32767s; 32767s ]

    [<Test>]
    member this.UInt32() = &[ 0u; 2378622171u ]

    [<Test>]
    member this.Int32() = &[ 0; Int32.MinValue; Int32.MaxValue ]

    [<Test>]
    member this.UInt64() = &[ 0UL; UInt64.MaxValue ]

    [<Test>]
    member this.Int64() = &[ 0L; Int64.MinValue; Int64.MaxValue ]

    [<Test>]
    member this.BigInt() =
        &[ Numerics.BigInteger.MinusOne, 348957248912748912398729877918I ]

    [<Test>]
    member this.Float32() =
        &
            [
                2.5f
                Single.Epsilon
                Single.MinValue
                Single.MaxValue
                Single.NaN
                Single.PositiveInfinity
                Single.NegativeInfinity
            ]

    [<Test>]
    member this.Float64() =
        &
            [
                -2.5
                Double.Epsilon
                Double.MinValue
                Double.MaxValue
                Double.NaN
                Double.PositiveInfinity
                Double.NegativeInfinity
            ]

    [<Test>]
    member this.Decimal() =
        &[ 6.9m; Decimal.MinValue; Decimal.MaxValue ]

    [<Test>]
    member this.DateTime() =
        &
            [
                DateTime.Now
                DateTime.Today
                DateTime.UnixEpoch
                DateTime.MinValue
                DateTime.MaxValue
            ]

    [<Test>]
    member this.DateTimeOffset() =
        &
            [
                DateTimeOffset.Now
                DateTimeOffset.FromUnixTimeMilliseconds(5000L)
                DateTimeOffset.UnixEpoch
                DateTimeOffset.MinValue
                DateTimeOffset.MaxValue
            ]

    [<Test>]
    member this.TimeSpan() =
        &
            [
                TimeSpan.MaxValue
                TimeSpan.MinValue
                DateTime.Today - DateTime.Now
                TimeSpan.Zero
            ]

    [<Test>]
    member this.Guid() =
        &[ Guid.NewGuid(); Guid.NewGuid(); Guid.NewGuid(); Guid.Empty ]

    [<Test>]
    member this.Json() =
        &[ JSON.String "Hello"; JSON.Null; JSON.Bool true ]

[<TestFixture>]
type ``2: Compound Round Trips``() =

    let env =
        Json(
            { Json.Settings.Default with
                FormatExpandObjects = false
            }
        )
            .WithDefaults()

    let (~&) (xs: 'T list) = List.iter (round_trip env) xs

    let (^&) (comp: 'T -> 'U) (xs: 'T list) =
        List.iter (round_trip_reftype env comp) xs

    [<Test>]
    member this.Option() = &[ Some true; None ]

    [<Test>]
    member this.VOption() = &[ ValueSome "Hello"; ValueNone ]

    [<Test>]
    member this.Union() =
        &[ Nil; One "Mind"; Many(Nil, [ 0.5; 0.4 ]); Many(Many(Nil, []), []) ]

    [<Test>]
    member this.StUnion() =
        &[ StNil; StOne "Mind"; StMany([ 6.0 ], '@') ]

    [<Test>]
    member this.Record() = &[ { Record.X = 5L; Y = 56.2f } ]

    [<Test>]
    member this.Record_Struct() = &[ { StRecord.X = 5L; Y = 56.2f } ]

    [<Test>]
    member this.Enum() =
        &
            [
                Enum.One
                Enum.Two
                LanguagePrimitives.EnumOfValue -127L
                LanguagePrimitives.EnumOfValue 1000L
            ]

    [<Test>]
    member this.Tuple2() = &[ ("Hello", [ 6; 7; 8 ]) ]

    [<Test>]
    member this.Tuple3_Struct() = &[ struct ("Hello", (), [ 6, 7, 8 ]) ]

    [<Test>]
    member this.List() = &[ [ 1; 2 ]; [] ]

    [<Test>]
    member this.Array() = &[ [| 1; 2 |]; [||] ]

    [<Test>]
    member this.Set() = &[ Set.empty; set [ 1; 5; 8; 10 ] ]

    [<Test>]
    member this.Map_String_Keys() =
        &[ Map.empty; Map.ofSeq [ "Hello", 5; "World", -25 ] ]

    [<Test>]
    member this.Map_Object_Keys() =
        &[ Map.ofSeq [ 2, 5; 1, -25 ]; Map.empty ]

    [<Test>]
    member this.ResizeArray() =
        (List.ofSeq) ^& [ ResizeArray([ 1; 2; 5; 25 ]); ResizeArray() ]

    [<Test>]
    member this.Dictionary_String_Keys() =
        let d = Collections.Generic.Dictionary()
        d.Add("Key1", 100.0)
        d.Add("Key2", -57453.2789)

        (Seq.map (|KeyValue|) >> List.ofSeq) ^& [ d; Collections.Generic.Dictionary() ]

    [<Test>]
    member this.Dictionary_Object_Keys() =
        let d = Collections.Generic.Dictionary()
        d.Add([], 100.0)
        d.Add([ 1 ], -57453.2789)

        (Seq.map (|KeyValue|) >> List.ofSeq) ^& [ d; Collections.Generic.Dictionary() ]

    [<Test>]
    member this.Record_Anonymous() = &[ {| X = 5L; Y = 56.2f; Z = 32 |} ]
