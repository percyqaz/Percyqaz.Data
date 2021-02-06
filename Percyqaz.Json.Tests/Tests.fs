module Percyqaz.Json.Tests

open System
open Percyqaz.Json
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

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

type PrimitiveRecord = {
    int: int
    long: int64
    single: single
    bool: bool
    float: float
    string: string
    byte: byte
    time: DateTime
    time2: DateTimeOffset
} with
    static member Default = { int = 2; long = -2177777288888L; single = 0.5f; bool = true; float = -3.14159265358979; string = "Hello world"; byte = 127uy; time = DateTime.Now; time2 = DateTimeOffset.MaxValue }

type [<Struct>] StructRecord = {
    one: int list
    two: int option
    three: int array
} with
    static member Default = { one = [1;2;3]; two = Some 7; three = [|3;2;1|] }

type POCO<'T when 'T : equality>(defaultValue: 'T, value: 'T) =
    member this.Value = value
    member this.DefaultValue = defaultValue
    override this.Equals(other) =
        match other with
        | :? POCO<'T> as other -> other.Value = this.Value && other.DefaultValue = this.DefaultValue
        | _ -> false
    override this.GetHashCode() = 0 //shuts up compiler :)
    static member Pickler: Json.Mapping.JsonPickler<POCO<'T>> =
        let tP = Json.Mapping.getPickler<'T>()
        Json.Mapping.mkPickler
            (fun (o: POCO<'T>) -> tP.Encode(o.Value))
            (fun (o: POCO<'T>) json -> tP.Decode(o.DefaultValue)(json) |> JsonMapResult.map (fun v -> POCO(o.DefaultValue, v)))

type [<Json.AllRequired>] ComplexRecord = {
    union: Union<int>
    stunion: StructUnion
    enum: Enum
    stuple: StructTuple
    tuple: Tuple
    prim: PrimitiveRecord
    too: ComplexRecordToo
}
and ComplexRecordToo = {
    record: ComplexRecord option
    arr: string array
    map: Map<string, int>
    list: Tuple list
    poco: POCO<float>
    anonymous: {|string: string; int: int|}
} with
    static member Default = { record = None; arr = [|"Hello"; "World"|]; map = Map.ofList[("",1);("a",2)]; list = [("", 0)]; poco = POCO(5.0, 10.0); anonymous = {|string = ""; int = 0|} } 

type POCOExtension(a, b) =
    inherit POCO<int>(a, b)
    static member Pickler = POCO<int>.Pickler

let ExpectFailure = function JsonMapResult.Success _ -> Assert.Fail("Mapping was expected to fail here but succeeded"); failwith "impossible" | o -> o
let ExpectSuccess = function JsonMapResult.Failure v -> Assert.Pass(sprintf "Mapping failed unexpectedly: %O" v); failwith "impossible" | o -> o
let RoundTrip(x: 'T) = Assert.AreEqual(x, x |> Json.toJson |> Json.fromJson<'T> |> ExpectSuccess |> JsonMapResult.value)

let [<Test>] PrimitiveRoundTrip() = PrimitiveRecord.Default |> RoundTrip
let [<Test>] StructRecordRoundTrip() = StructRecord.Default |> RoundTrip
let [<Test>] OptionRoundTrip() = [Some 0; None] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] UnionRoundTrip() = [Union<unit>.CaseOne "Hello"; Union<unit>.CaseTwo (Union<unit>.CaseOne "World", ()); Union<unit>.CaseThree] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] StUnionRoundTrip() = [CaseOne ("", 0); CaseTwo] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] Float32RoundTrip() = [Single.Epsilon; Single.NegativeInfinity; Single.MaxValue; Single.NaN; -2782346.348726f] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] EnumRoundTrip() = [Enum.A; Enum.C; Enum.B ||| Enum.A; Enum.A &&& Enum.C] |> List.map RoundTrip |> ignore; Assert.Pass()
let [<Test>] ComplexRecordRoundTrip() = { union = Union<_>.CaseOne ""; stunion = CaseTwo; enum = Enum.A; stuple = struct (1,2,Int32.MinValue); tuple = ("", 0); prim = PrimitiveRecord.Default; too = ComplexRecordToo.Default } |> RoundTrip
//these are tested separately to round trip code as they don't have structural equality
let [<Test>] GenericListRoundTrip() = let list = ResizeArray([3;2;1]) in Assert.AreEqual(List.ofSeq list, list |> Json.toJson |> Json.fromJson<ResizeArray<int>> |> ExpectSuccess |> JsonMapResult.value |> List.ofSeq)

let [<Test>] FileExist() = Json.fromFile<string>("doesntexist") |> ignore; Assert.Pass() //returns a failure instead of throwing an exception
let [<Test>] Primitive1000() = for i = 0 to 1000 do PrimitiveRecord.Default |> Json.toJson |> Json.fromJson<PrimitiveRecord> |> ignore

//Todo:
//Tests for pickler-creation-time checks like attributes being present
//Benchmarks