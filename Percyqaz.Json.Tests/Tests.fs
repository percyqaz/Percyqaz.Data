module Percyqaz.Json.Tests

open System
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Json.Json
open NUnit.Framework


let Json = new JsonEncoder()

(*
[<SetUp>]
let Setup () =
    Json.Mapping.Rules.addTypeRuleWithDefault<int * int>
        (fun (x, y) -> JSON.Null)
        (fun (x, y) json -> JsonMapResult.Success(7, y))
        (0, 9)
    Json.Mapping.Rules.addPicklerRule() *)

type Tuple = string * int

type Union<'T> =
| CaseOne of string
| CaseTwo of label1: Union<'T> * label2: 'T
| CaseThree

type Enum =
| A = 1u
| B = 2u
| C = 4u

type StructTuple = (struct (int * int * int))

type [<Struct>] StructUnion =
| CaseOne of Tuple
| CaseTwo

type PrimitiveRecord =
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

type [<Struct>] StructRecord =
    {
        one: int list
        two: int option
        three: int array
    }
    static member Default = { one = [1;2;3]; two = Some 7; three = [|3;2;1|] }

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
        union: Union<int>
        stunion: StructUnion
        enum: Enum
        stuple: StructTuple
        tuple: Tuple
        prim: PrimitiveRecord
        too: ComplexRecordToo
    }
and ComplexRecordToo =
    {
        record: ComplexRecord option
        arr: string array
        map: Map<string, int>
        list: Tuple list
        poco: POCO<float>
        anonymous: {|string: string; int: int|}
    }
    static member Default = { record = None; arr = [|"Hello"; "World"|]; map = Map.ofList[("",1);("a",2)]; list = [("", 0)]; poco = POCO(5.0, 10.0); anonymous = {|string = ""; int = 0|} } 

let getOrThrow (res: JsonResult<'t>) =
    match res with
    | Ok r -> r
    | Error e -> raise e

let ExpectFailure = function JsonResult.Ok _ -> Assert.Fail("Mapping was expected to fail here but succeeded"); failwith "impossible" | o -> o
let ExpectSuccess = function JsonResult.Error v -> Assert.Pass(sprintf "Mapping failed unexpectedly: %O" v); failwith "impossible" | o -> o
let RoundTrip(x: 'T) = Assert.AreEqual(x, x |> Json.ToString |> (fun j -> printfn "%s" j; j) |> Json.FromString<'T> |> getOrThrow)

let [<Test>] DefaultGenerationTest() =
    let v = Json.Default<Union<int>>()
    printfn "%A" v
    Assert.Pass()

let [<Test>] CustomRuleRoundTrip() = (7, 9) |> RoundTrip
let [<Test>] PrimitiveRoundTrip() = PrimitiveRecord.Default |> RoundTrip
let [<Test>] StructRecordRoundTrip() = StructRecord.Default |> RoundTrip
let [<Test>] OptionRoundTrip() = [Some 0; None] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] StTupleRoundTrip() = struct (5, "111", nanf) |> RoundTrip
let [<Test>] UnionRoundTrip() = [Union<unit>.CaseOne "Hello"; Union<unit>.CaseTwo (Union<unit>.CaseOne "World", ()); Union<unit>.CaseThree] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] StUnionRoundTrip() = [CaseOne ("", 0); CaseTwo] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] Float32RoundTrip() = [Single.Epsilon; Single.NegativeInfinity; Single.MaxValue; Single.NaN; -2782346.348726f] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] EnumRoundTrip() = [Enum.A; Enum.C; Enum.B ||| Enum.A; Enum.A &&& Enum.C] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] ComplexRecordRoundTrip() = { union = Union<_>.CaseOne ""; stunion = CaseTwo; enum = Enum.A; stuple = struct (1,2,Int32.MinValue); tuple = ("", 0); prim = PrimitiveRecord.Default; too = ComplexRecordToo.Default } |> RoundTrip
let [<Test>] MapRoundTrip() = [(1, ""); (3, "a"); (6, "e")] |> Map |> RoundTrip
//these are tested separately to round trip code as they don't have structural equality
let [<Test>] CSharpListRoundTrip() = let list = ResizeArray([3;2;1]) in Assert.AreEqual(List.ofSeq list, list |> Json.ToJson |> Json.FromJson<ResizeArray<int>> |> ExpectSuccess |> getOrThrow |> List.ofSeq)
let [<Test>] CSharpDictRoundTrip() = [(1, ""); (3, "a"); (6, "e")] |> Map |> Dictionary |> RoundTrip
let [<Test>] DefaultValuesAreUsed() = Assert.AreEqual(ComplexRecordToo.Default, Json.FromString<ComplexRecordToo>("{}") |> getOrThrow)
let [<Test>] JsonRoundTrip() = let j = PrimitiveRecord.Default |> Json.ToJson in Assert.AreEqual(j, Json.ToJson j); Assert.AreEqual(j, Json.FromJson<JSON>(j) |> getOrThrow)

let [<Test>] FileExist() = Json.FromFile<string>("doesntexist") |> ignore; Assert.Pass() //returns a failure instead of throwing an exception
let [<Test>] Primitive1000() = for i = 0 to 1000 do PrimitiveRecord.Default |> Json.ToJson |> Json.FromJson<PrimitiveRecord> |> ignore

let [<Test>] ComplexRecordRoundTripCached() = ComplexRecordRoundTrip()

//Todo:
//Tests for pickler-creation-time checks like attributes being present
//Benchmarks