module Percyqaz.Json.Tests

open System
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Json.Json
open NUnit.Framework

type Union =
    | Nullary
    | Unary of int
    | Binary of Union * string

[<Struct>]
type StUnion =
    | Nullary
    | Unary of int
    | Binary of byte * string

type GenericUnion<'T> =
    | Nullary
    | Unary of 'T
    | Binary of GenericUnion<'T> * string

[<Struct>]
type GenericStUnion<'T> =
    | Nullary
    | Unary of 'T
    | Binary of byte * string

type EnumInt8 =
    | A = 1y
    | B = 2y
    | C = -4y

type EnumInt32 =
    | A = 1
    | B = 2
    | C = -4

type EnumInt64 =
    | A = 1l
    | B = 2l
    | C = -4l

type SimpleRecord =
    {
        int: int
        long: int64
        single: single
        bool: bool
        float: float
        string: string
        byte: byte
        time: DateTime
        time2: DateTimeOffset
        time3: TimeSpan
    }

type SimpleRecordWithDefault =
    {
        int: int
        long: int64
        single: single
        bool: bool
        float: float
        string: string
        byte: byte
        time: DateTime
        time2: DateTimeOffset
        time3: TimeSpan
    }
    static member Default = { int = 2; long = -2177777288888L; single = 0.5f; bool = true; float = -3.14159265358979; string = "Hello world"; byte = 127uy; time = DateTime.Now; time2 = DateTimeOffset.MaxValue; time3 = DateTime.Now.TimeOfDay }

[<Struct>]
type SimpleStRecord =
    {
        int: int
        long: int64
        single: single
        bool: bool
        float: float
        string: string
        byte: byte
        time: DateTime
        time2: DateTimeOffset
        time3: TimeSpan
    }

[<Struct>]
type SimpleStRecordWithDefault =
    {
        int: int
        long: int64
        single: single
        bool: bool
        float: float
        string: string
        byte: byte
        time: DateTime
        time2: DateTimeOffset
        time3: TimeSpan
    }
    static member Default = { int = 2; long = -2177777288888L; single = 0.5f; bool = true; float = -3.14159265358979; string = "Hello world"; byte = 127uy; time = DateTime.Now; time2 = DateTimeOffset.MaxValue; time3 = DateTime.Now.TimeOfDay }

type POCO<'T when 'T : equality>(defaultValue: 'T, value: 'T) =
    member this.Value = value
    member this.DefaultValue = defaultValue
    override this.Equals(other) =
        match other with
        | :? POCO<'T> as other -> other.Value = this.Value && other.DefaultValue = this.DefaultValue
        | _ -> false
    override this.GetHashCode() = 0 //shuts up compiler :)
    static member JsonCodec(cache, settings, rules): Json.Mapping.JsonCodec<POCO<'T>> =
        let tP = Mapping.getCodec<'T>(cache, settings, rules)
        {
            Encode = fun (o: POCO<'T>) -> tP.Encode o.Value
            Decode = fun (instance: POCO<'T>) json -> POCO(instance.DefaultValue, tP.Decode instance.Value json)
            Default = fun _ -> let d = tP.Default() in POCO(d, d)
        }

type ComplexRecord =
    {
        union: GenericUnion<int>
        stunion: GenericStUnion<byte>
        enum: EnumInt8
        stuple: (struct (int * byte * bool))
        tuple: (int * byte * bool)
        record: SimpleRecord
        strecord: SimpleStRecord
        option: int option
        too: ComplexRecordToo
    }
and ComplexRecordToo =
    {
        one: ComplexRecord option
        arr: string array
        list: int list
        map: Map<string, int>
        poco: POCO<float>
        anonymous: {|string: string; int: int|}
    }
    static member Default =
        {
            one = None
            arr = [|"Hello"; "World"|]
            map = Map.ofList [("", 1); ("a", 2)]
            list = [0; 1]
            poco = POCO(5.0, 10.0)
            anonymous = {|string = ""; int = 0|}
        } 

let expect (res: JsonResult<'t>) =
    match res with
    | Ok r -> r
    | Error e -> raise e

//let ExpectFailure = function JsonResult.Ok _ -> Assert.Fail("Mapping was expected to fail here but succeeded"); failwith "impossible" | o -> o
//let ExpectSuccess = function JsonResult.Error v -> Assert.Pass(sprintf "Mapping failed unexpectedly: %O" v); failwith "impossible" | o -> o

let RoundTrip (json: JsonEncoder) (x: 'T) = Assert.AreEqual(x, x |> json.ToString |> (fun j -> printfn "%s" j; j) |> json.FromString<'T> |> expect)
let RoundTripList (json: JsonEncoder) (xs: 'T list) = List.map (RoundTrip json) xs |> ignore

[<TestFixture>]
type ``1: Basic Round Trips``() =
    
    let Json = new JsonEncoder({ JsonSettings.Default with AllowNullStrings = false })
    let JsonWithNullString = new JsonEncoder({ JsonSettings.Default with AllowNullStrings = true })

    [<Test>]
    member this.String_AllowNull() =
        [""; "Hello"; "\n"; "§"; "\r\t\\😋"; null]
        |> RoundTripList JsonWithNullString
        Assert.Pass()
    
    [<Test>]
    member this.String_DisallowNull() =
        [""; "Hello"; "\n§\r\t\\😋"]
        |> RoundTripList Json
        Assert.Throws<ArgumentNullException> ( fun () -> (null: string) |> Json.ToJson |> ignore ) |> ignore
        Assert.Throws<MapFailure> ( fun () -> Json.FromJson<string> JSON.Null |> expect |> ignore ) |> ignore
        Assert.Pass()

    [<Test>]
    member this.Integers() =
        //8 bit
        SByte.MaxValue |> RoundTrip Json
        Byte.MaxValue |> RoundTrip Json
        //16 bit
        [19s; Int16.MaxValue] |> RoundTripList Json
        UInt16.MaxValue |> RoundTrip Json
        //32 bit
        [5; Int32.MaxValue] |> RoundTripList Json
        UInt32.MaxValue |> RoundTrip Json
        //64 bit
        [60L; Int64.MinValue] |> RoundTripList Json
        UInt64.MaxValue |> RoundTrip Json
        //big
        [Numerics.BigInteger.One; -9999999999999999999999999999999999I] |> RoundTripList Json
        Assert.Pass()

    [<Test>]
    member this.FloatSingle() =
        [2.6f; nanf; Single.Epsilon; Single.MinValue; Single.NegativeInfinity; Single.PositiveInfinity] |> RoundTripList Json
        Assert.Pass()

    [<Test>]
    member this.FloatDouble() =
        [2.6; Math.PI; nan; Double.Epsilon; Double.MaxValue; Double.NegativeInfinity; Double.PositiveInfinity] |> RoundTripList Json
        Assert.Pass()

    [<Test>]
    member this.Primitives() =
        () |> RoundTrip Json
        [true; false] |> RoundTripList Json
        [' '; '§'; '\n'; '\t'] |> RoundTripList Json
        [Decimal.MinValue; Decimal.MaxValue; 0.69m] |> RoundTripList Json
        Assert.Pass()

    [<Test>]
    member this.Times() =
        [DateTime.Now; DateTime.MaxValue; DateTime.Today] |> RoundTripList Json
        [DateTimeOffset.Now; DateTimeOffset.MaxValue; DateTimeOffset.MinValue] |> RoundTripList Json
        [TimeSpan.MinValue; TimeSpan.MaxValue; TimeSpan.FromTicks(25565L)] |> RoundTripList Json
        Assert.Pass()

[<TestFixture>]
type ``2: Round Trips``() =
    
    let Json = new JsonEncoder()

    [<Test>]
    member this.Unions() =
        [ Union.Nullary; Union.Unary 6; Union.Binary (Union.Nullary, "zz") ]
        |> RoundTripList Json
        [ GenericUnion<string>.Nullary; GenericUnion<string>.Unary "Hello!"; GenericUnion<string>.Binary (GenericUnion<_>.Nullary, "zz") ]
        |> RoundTripList Json
    
    [<Test>]
    member this.StUnions() =
        [ StUnion.Nullary; StUnion.Unary 6; StUnion.Binary (7uy, "zz") ]
        |> RoundTripList Json
        [ GenericStUnion<string>.Nullary; GenericStUnion<string>.Unary "Hello!"; GenericStUnion<string>.Binary (255uy, "zz") ]
        |> RoundTripList Json

    [<Test>]
    member this.Enums() =
        [ EnumInt8.A; EnumInt8.A ||| EnumInt8.C; EnumInt8.A &&& EnumInt8.B] |> RoundTripList Json
        [ EnumInt32.A; EnumInt32.A ||| EnumInt32.C; enum -25; enum 600] |> RoundTripList Json
        [ EnumInt64.A; EnumInt64.A ||| EnumInt64.B; enum Int32.MinValue; enum 600] |> RoundTripList Json

    [<Test>]
    member this.Tuples() =
        (6, "Hello", Some 7uy) |> RoundTrip Json
        struct (6, "Hello", Some 9us) |> RoundTrip Json

    [<Test>]
    member this.Options() =
        [Some 5; None] |> RoundTripList Json
        [ValueSome "Hello"; ValueNone] |> RoundTripList Json
        Assert.Throws<ArgumentNullException> ( fun () -> (Some null: string option) |> RoundTrip Json ) |> ignore
        Assert.Pass()

    [<Test>]
    member this.Arrays() =
        [|1; 2; 3; 4; 5|] |> RoundTrip Json
        ([||]: string array) |> RoundTrip Json
        ([|[5]|]: int list array) |> RoundTrip Json

    [<Test>]
    member this.FSharpLists() =
        [1; 2; 3; 4; 5] |> RoundTrip Json
        ([]: string list) |> RoundTrip Json
        ([[""]]: string list list ) |> RoundTrip Json
    
    [<Test>]
    member this.CSharpLists() =
        let original = ResizeArray([5; 4; 3; 2; 1])
        let res =
            original
            |> Json.ToString
            |> Json.FromString<ResizeArray<int>>
            |> expect
        // checking this equality verifies the round trip list has the same contents (it has a different identity so this deep clones the list)
        Assert.AreEqual(List.ofSeq original, List.ofSeq res)

    [<Test>]
    member this.Maps() =
        [
            [(2, 1); (3, 1)] |> Map.ofList;
            Map.empty
        ] |> RoundTripList Json
        [
            [("", 1); ("", 1)] |> Map.ofList;
            Map.empty
        ] |> RoundTripList Json

    [<Test>]
    member this.CSharpDictionaries() =
        Assert.Fail() //too lazy at this current time

[<TestFixture>]
type ``3: Json Formatting``() =
    
    let Json = new JsonEncoder()
    let JsonEncodeMapsAsArr = new JsonEncoder({ JsonSettings.Default with EncodeAllMapsAsArrays = true })
    
    let isString = fun json -> printfn "%A" json; match json with JSON.String _ -> true | _ -> false
    let isArr = fun json -> printfn "%A" json; match json with JSON.Array _ -> true | _ -> false
    let isObj = fun json -> printfn "%A" json; match json with JSON.Object _ -> true | _ -> false
    let isNull = fun json -> printfn "%A" json; match json with JSON.Null -> true | _ -> false

    [<Test>]
    member this.FloatSingle_Special_EncodeAsJStr() =
        // ensure these special values are encoded as strings
        // since just `NaN` or `-Infinity` as a literal is not in the JSON spec and may not be portable to other JSON parsers
        [nanf; Single.NegativeInfinity; Single.PositiveInfinity]
        |> List.forall (Json.ToJson >> isString)
        |> Assert.That

    [<Test>]
    member this.FloatDouble_Special_EncodeAsJStr() =
        [nan; Double.NegativeInfinity; Double.PositiveInfinity]
        |> List.forall (Json.ToJson >> isString)
        |> Assert.That

    [<Test>]
    member this.Lists_EncodeAsJArr() =
        [1; 2; 3; 4; 5]
        |> (Json.ToJson >> isArr)
        |> Assert.That
    
    [<Test>]
    member this.Arrays_EncodeAsJArr() =
        [|1; 2; 3; 4; 5|]
        |> (Json.ToJson >> isArr)
        |> Assert.That
        
    [<Test>]
    member this.Maps_StringKeys_EncodeAsJObj() =
        [("A", 0); ("B", 2)]
        |> Map.ofList
        |> (Json.ToJson >> isObj)
        |> Assert.That

    [<Test>]
    member this.Maps_StringKeys_SettingEncodeAsJArr() =
        [("A", 0); ("B", 2)]
        |> Map.ofList
        |> (JsonEncodeMapsAsArr.ToJson >> isArr)
        |> Assert.That
    
    [<Test>]
    member this.Maps_NonStringKeys_EncodeAsJArr() =
        [(2, 0); (1, 2)]
        |> Map.ofList
        |> (Json.ToJson >> isArr)
        |> Assert.That

    [<Test>]
    member this.Union_NullaryCase_EncodeAsJStr() =
        Union.Nullary
        |> (Json.ToJson >> isString)
        |> Assert.That
        StUnion.Nullary
        |> (Json.ToJson >> isString)
        |> Assert.That
    
    [<Test>]
    member this.Union_NonNullaryCase_EncodeAsJObj() =
        Union.Unary 5
        |> (Json.ToJson >> isObj)
        |> Assert.That
        Union.Binary (Union.Nullary, String.Empty)
        |> (Json.ToJson >> isObj)
        |> Assert.That
        StUnion.Unary 5
        |> (Json.ToJson >> isObj)
        |> Assert.That

    [<Test>]
    member this.Option_EncodeFlat() =
        Some "hello"
        |> (Json.ToJson >> isString)
        |> Assert.That
        (None: int option)
        |> (Json.ToJson >> isNull)
        |> Assert.That

    [<Test>]
    member this.OptionValue_EncodeFlat() =
        ValueSome "hello"
        |> (Json.ToJson >> isString)
        |> Assert.That
        (ValueNone: int voption)
        |> (Json.ToJson >> isNull)
        |> Assert.That

    [<Test>]
    member this.Record_EncodeAsJObj() =
        SimpleRecordWithDefault.Default
        |> (Json.ToJson >> isObj)
        |> Assert.That

[<TestFixture>]
type ``4: Text Formatting``() =
    
    let Json = new JsonEncoder()

[<TestFixture>]
type ``5: Performance``() =
    
    let Json = new JsonEncoder()

    let simpleRecordInstance = Json.Default<SimpleRecord>()
    let simpleStRecordInstance = Json.Default<SimpleStRecord>()
    do
        Json.Default<SimpleRecordWithDefault>() |> ignore
        Json.Default<SimpleStRecordWithDefault>() |> ignore
        Json.Default<ComplexRecord>() |> ignore
        Json.Default<GenericUnion<int>>() |> ignore

    let thousand (original: 'T) =
        let mutable result = original
        for i in 0 .. 999 do
            result <- Json.ToJson result |> Json.FromJson<'T> |> expect
        Assert.AreEqual(original, result)

    [<Test>]
    member this.SimpleRecord() =
        simpleRecordInstance |> thousand

    [<Test>]
    member this.SimpleRecordWithDefault() =
        SimpleRecordWithDefault.Default |> thousand

    [<Test>]
    member this.ComplexRecord() =
        {
            union = GenericUnion<int>.Unary 8
            stunion = GenericStUnion<byte>.Nullary
            enum = EnumInt8.C
            stuple = struct (5, 7uy, true)
            tuple = (-5, 27uy, false)
            record = simpleRecordInstance
            strecord = simpleStRecordInstance
            option = None
            too = ComplexRecordToo.Default
        } |> thousand

    [<Test>]
    member this.Int32() =
        9 |> thousand
    
    [<Test>]
    member this.Float64() =
        Math.PI |> thousand
        