namespace Percyqaz

open System
open System.Globalization

module Json =

    type JsonRequiredAttribute() = inherit Attribute()

    type Json =
    | Object of Map<string, Json>
    | Array of Json list
    | String of string
    | Number of string
    | True
    | False
    | Null
    with
        static member ToJson(json: Json) = json
    
    module Parsing =
        ()
        //fparsec parser to Json structure tree

    module Formatting =
        ()
        //rules to format Json tree into text

    type JsonResult<'T> = Success of 'T | Failure of Exception
    module JsonResult =
        let map f =
            function
            | Success x -> Success (f x)
            | Failure e -> Failure e

        let tuple2 =
            function
            | (Success a, Success b) -> Success (a, b)
            | (Failure e, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e) -> Exception ("Error in element 2", e) |> Failure

        let tuple3 =
            function
            | (Success a, Success b, Success c) -> Success (a, b, c)
            | (Failure e, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e) -> Exception ("Error in element 3", e) |> Failure

        let tuple4 =
            function
            | (Success a, Success b, Success c, Success d) -> Success (a, b, c, d)
            | (Failure e, _, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e, _) -> Exception ("Error in element 3", e) |> Failure
            | (_, _, _, Failure e) -> Exception ("Error in element 4", e) |> Failure

        let tuple5 =
            function
            | (Success a, Success b, Success c, Success d, Success e) -> Success (a, b, c, d, e)
            | (Failure e, _, _, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _, _, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e, _, _) -> Exception ("Error in element 3", e) |> Failure
            | (_, _, _, Failure e, _) -> Exception ("Error in element 4", e) |> Failure
            | (_, _, _, _, Failure e) -> Exception ("Error in element 5", e) |> Failure

        let tuple6 =
            function
            | (Success a, Success b, Success c, Success d, Success e, Success f) -> Success (a, b, c, d, e, f)
            | (Failure e, _, _, _, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _, _, _, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e, _, _, _) -> Exception ("Error in element 3", e) |> Failure
            | (_, _, _, Failure e, _, _) -> Exception ("Error in element 4", e) |> Failure
            | (_, _, _, _, Failure e, _) -> Exception ("Error in element 5", e) |> Failure
            | (_, _, _, _, _, Failure e) -> Exception ("Error in element 6", e) |> Failure

        let tuple7 =
            function
            | (Success a, Success b, Success c, Success d, Success e, Success f, Success g) -> Success (a, b, c, d, e, f, g)
            | (Failure e, _, _, _, _, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _, _, _, _, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e, _, _, _, _) -> Exception ("Error in element 3", e) |> Failure
            | (_, _, _, Failure e, _, _, _) -> Exception ("Error in element 4", e) |> Failure
            | (_, _, _, _, Failure e, _, _) -> Exception ("Error in element 5", e) |> Failure
            | (_, _, _, _, _, Failure e, _) -> Exception ("Error in element 6", e) |> Failure
            | (_, _, _, _, _, _, Failure e) -> Exception ("Error in element 7", e) |> Failure

        let tuple8 =
            function
            | (Success a, Success b, Success c, Success d, Success e, Success f, Success g, Success h) -> Success (a, b, c, d, e, f, g, h)
            | (Failure e, _, _, _, _, _, _, _) -> Exception ("Error in element 1", e) |> Failure
            | (_, Failure e, _, _, _, _, _, _) -> Exception ("Error in element 2", e) |> Failure
            | (_, _, Failure e, _, _, _, _, _) -> Exception ("Error in element 3", e) |> Failure
            | (_, _, _, Failure e, _, _, _, _) -> Exception ("Error in element 4", e) |> Failure
            | (_, _, _, _, Failure e, _, _, _) -> Exception ("Error in element 5", e) |> Failure
            | (_, _, _, _, _, Failure e, _, _) -> Exception ("Error in element 6", e) |> Failure
            | (_, _, _, _, _, _, Failure e, _) -> Exception ("Error in element 7", e) |> Failure
            | (_, _, _, _, _, _, _, Failure e) -> Exception ("Error in element 8", e) |> Failure

    module MappingQ =

        type JsonEncoder<'T> = 'T -> Json
        type JsonDecoder<'T> = 'T -> Json -> JsonResult<'T>

        //MAPPINGS FOR SIMPLE TYPES
        type DefaultTransforms = 
            //STRINGS
            static member ToJson(s: string) = Json.String s
            static member FromJson(_: string, json: Json) =
                match json with Json.String s -> Success s | _ -> Exception("Expected a json string") |> Failure

            //INTEGER TYPES
            static member ToJson(i: int16) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: int16, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Int16.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: int32) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: int32, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Int32.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: int64) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: int64, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Int64.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: uint16) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: uint16, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try UInt16.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: uint32) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: uint32, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try UInt32.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: uint64) = i.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: uint64, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try UInt64.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(i: bigint) = i.ToString("R", CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: bigint, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Numerics.BigInteger.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            //FLOATING POINT TYPES
            static member ToJson(f: float32) = f.ToString("R", CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: float32, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Single.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(f: float) = f.ToString("G17", CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: float, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Double.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            static member ToJson(f: decimal) = f.ToString(CultureInfo.InvariantCulture) |> Json.Number
            static member FromJson(_: decimal, json: Json) =
                match json with
                | Json.String s | Json.Number s -> try Int16.Parse(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err
                | _ -> Exception("Expected a number") |> Failure

            //OTHER
            static member ToJson(b: bool) = if b then Json.True else Json.False
            static member FromJson(_: bool, json: Json) =
                match json with
                | Json.String "" -> Success false
                | Json.Number "0" -> Success false
                | Json.Null -> Success false
                | Json.False -> Success false
                | Json.String _ -> Success true
                | Json.Number "1" -> Success true
                | Json.True -> Success true
                | _ -> Failure <| Exception("Expected a boolean value")
            static member ToJson(uo: unit option) = match uo with Some () -> Json.True | None -> Json.False
            static member FromJson(_: unit option, json: Json) = (DefaultTransforms.FromJson(false, json)) |> JsonResult.map (fun b -> if b then Some () else None)
            static member ToJson(_: unit) = Json.Null
            static member FromJson(_: unit, json: Json) = Success ()

        /// ---
        //            ENCODE/DECODE USING DUCK TYPING

        let defaultInstance = Unchecked.defaultof<DefaultTransforms>
        let inline toJsonInternal(defaults: ^defaults) (obj: ^T): Json =
            ((^T or ^defaults): (static member ToJson: 'T -> Json) obj)
        let inline fromJsonInternal(defaults: ^defaults) (defaultValue: ^T) (json: Json): JsonResult<'T> =
            ((^T or ^defaults): (static member FromJson: 'T -> Json -> JsonResult<'T>)(defaultValue, json))

        let inline toJson(obj: ^T) = toJsonInternal(defaultInstance)(obj)
        let inline fromJson(obj: ^T, json: Json) = fromJsonInternal(defaultInstance)(obj)(json)

        /// ---

        //MAPPINGS FOR COMPOSITE TYPES
        type DefaultTransforms with

            //LISTS
            static member inline ToJson(list: ^T list) = list |> (List.map toJson) |> Json.Array
            static member inline FromJson(_: ^T list, json: Json) =
                let rec f (n, list) = 
                    let d = Unchecked.defaultof<'T>
                    match list with
                    | [] -> Success []
                    | (x :: xs) -> 
                        match fromJson(d, x) with
                        | Success y ->
                            match f (n + 1, xs) with
                            | Success ys -> Success (y :: ys)
                            | Failure e -> Failure e
                        | Failure e -> new Exception(sprintf "Error in item %i" n, e) |> Failure
                match json with
                | Json.Array xs -> f (0, xs)
                | _ -> Exception("Expected a json array") |> Failure

            //ARRAYS
            static member inline ToJson(list: ^T array) = list |> (Array.map toJson) |> List.ofArray |> Json.Array
            static member inline FromJson(_: ^T array, json: Json) = fromJson(([]: 'T list), json) |> JsonResult.map Array.ofList

            //OPTIONS
            static member inline ToJson(op: ^T option) = match op with Some x -> toJson x | None -> Json.Null
            static member inline FromJson(_: ^T option, json: Json) = match json with Json.Null -> Success None | j -> fromJson(Unchecked.defaultof<'T>, j) |> JsonResult.map Some

            //TUPLES UP TO 8
            //todo: fix unchecked issue
            static member inline ToJson((a, b): ^A * ^B) = [toJson a; toJson b] |> Json.Array
            static member inline FromJson((a_, b_): ^A * ^B, json: Json) =
                match json with
                | Json.Array [a; b] ->
                    (fromJson (a_, a), fromJson (b_, b)) |> JsonResult.tuple2
                | _ -> Exception("Expected a json array with 2 elements") |> Failure
            static member inline ToJson((a, b, c): ^A * ^B * ^C) = [toJson a; toJson b; toJson c] |> Json.Array
            static member inline FromJson(_: ^A * ^B * ^C, json: Json) =
                match json with
                | Json.Array [a; b; c] ->
                    (fromJson (Unchecked.defaultof<'A>, a), fromJson (Unchecked.defaultof<'B>, b), fromJson (Unchecked.defaultof<'C>, c)) |> JsonResult.tuple3
                | _ -> Exception("Expected a json array with 3 elements") |> Failure
            static member inline ToJson((a, b, c, d): ^A * ^B * ^C * ^D) = [toJson a; toJson b; toJson c; toJson d] |> Json.Array
            static member inline FromJson((a_, b_, c_, d_): ^A * ^B * ^C * ^D, json: Json) =
                match json with
                | Json.Array [a; b; c; d] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c), fromJson (d_, d)) |> JsonResult.tuple4
                | _ -> Exception("Expected a json array with 4 elements") |> Failure
            static member inline ToJson((a, b, c, d, e): ^A * ^B * ^C * ^D * ^E) = [toJson a; toJson b; toJson c; toJson d; toJson e] |> Json.Array
            static member inline FromJson((a_, b_, c_, d_, e_): ^A * ^B * ^C * ^D * ^E, json: Json) =
                match json with
                | Json.Array [a; b; c; d; e] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c), fromJson (d_, d), fromJson (e_, e)) |> JsonResult.tuple5
                | _ -> Exception("Expected a json array with 5 elements") |> Failure
            static member inline ToJson((a, b, c, d, e, f): ^A * ^B * ^C * ^D * ^E * ^F) = [toJson a; toJson b; toJson c; toJson d; toJson e; toJson f] |> Json.Array
            static member inline FromJson((a_, b_, c_, d_, e_, f_): ^A * ^B * ^C * ^D * ^E * ^F, json: Json) =
                match json with
                | Json.Array [a; b; c; d; e; f] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c), fromJson (d_, d), fromJson (e_, e), fromJson (f_, f)) |> JsonResult.tuple6
                | _ -> Exception("Expected a json array with 6 elements") |> Failure
            static member inline ToJson((a, b, c, d, e, f, g): ^A * ^B * ^C * ^D * ^E * ^F * ^G) = [toJson a; toJson b; toJson c; toJson d; toJson e; toJson f; toJson g] |> Json.Array
            static member inline FromJson((a_, b_, c_, d_, e_, f_, g_): ^A * ^B * ^C * ^D * ^E * ^F * ^G, json: Json) =
                match json with
                | Json.Array [a; b; c; d; e; f; g] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c), fromJson (d_, d), fromJson (e_, e), fromJson (f_, f), fromJson (g_, g)) |> JsonResult.tuple7
                | _ -> Exception("Expected a json array with 7 elements") |> Failure
            static member inline ToJson((a, b, c, d, e, f, g, h): ^A * ^B * ^C * ^D * ^E * ^F * ^G * ^H) = [toJson a; toJson b; toJson c; toJson d; toJson e; toJson f; toJson g; toJson h] |> Json.Array
            static member inline FromJson((a_, b_, c_, d_, e_, f_, g_, h_): ^A * ^B * ^C * ^D * ^E * ^F * ^G * ^H, json: Json) =
                match json with
                | Json.Array [a; b; c; d; e; f; g; h] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c), fromJson (d_, d), fromJson (e_, e), fromJson (f_, f), fromJson (g_, g), fromJson (h_, h)) |> JsonResult.tuple8
                | _ -> Exception("Expected a json array with 8 elements") |> Failure

    module Mapping =

        open TypeShape.Core
        open TypeShape.Core.Utils

        type JsonPickler<'T> = {
            Encode: 'T -> Json
            Decode: 'T -> Json -> JsonResult<'T>
        }

        let mkPickler (encode: 'T -> Json) (decode: 'T -> Json -> JsonResult<'T>) =
            {
                Encode = unbox encode
                Decode = unbox decode
            }

        let rec getPickler<'T>() : JsonPickler<'T> = genPickler<'T>()

        and genPickler<'T>() : JsonPickler<'T> =
            match shapeof<'T> with
            | Shape.Unit ->
                mkPickler
                    (fun _ -> Json.Null)
                    (fun _ _ -> Success ())
            | Shape.Bool ->
                mkPickler
                    (fun b -> if b then Json.True else Json.False)
                    (fun _ json -> 
                        match json with
                        | Json.String ""
                        | Json.Number "0"
                        | Json.Null
                        | Json.False -> Success false
                        | Json.String _
                        | Json.Number "1"
                        | Json.True -> Success true
                        | _ -> Failure <| Exception("Expected a boolean value"))
            | Shape.Byte
            | Shape.SByte
            | Shape.Int16
            | Shape.Int32
            | Shape.Int64
            | Shape.IntPtr
            | Shape.UInt16
            | Shape.UInt32
            | Shape.UInt64
            | Shape.UIntPtr
            | Shape.BigInt
            | Shape.Single
            | Shape.Double
            | Shape.Decimal
            | Shape.Char -> failwith "nyi"
            | Shape.String -> mkPickler (Json.String) (fun _ json -> match json with Json.String s -> Success s | _ -> Failure <| Exception("Expected a JSON string"))
            | Shape.TimeSpan
            | Shape.DateTime
            | Shape.DateTimeOffset -> failwith "nyi"

            | Shape.Enum t -> failwith "nyi"
            | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
                let elemHandler (field: IShapeMember<'Class>) =
                    field.Accept { new IMemberVisitor<'Class, _> with
                    member _.Visit (shape: ShapeMember<'Class, 'Field>) =
                        let tP = getPickler()
                        (fun o -> tP.Encode(shape.Get o)),
                        (fun o json ->
                            match tP.Decode(shape.Get o)(json) with
                            | Success v -> Success (shape.Set o v)
                            | Failure e -> Failure <| Exception("Failed to parse " + field.Label, e)) }
                let encoders, decoders = shape.Elements |> Array.map elemHandler |> Array.unzip
                mkPickler
                    (fun o -> Array.map (fun enc -> enc(o)) encoders |> List.ofArray |> Json.Array)
                    (fun _ json -> 
                        match json with 
                        | Json.Array xs when List.length xs = shape.Elements.Length -> 
                            Array.ofList xs |> Array.zip decoders |> Array.fold(fun o (dec, json) -> match o with Success o -> dec(o)(json) | Failure e -> Failure e) (Success <| shape.CreateUninitialized())
                        | _ -> Failure <| Exception("Expected a JSON array of length " + shape.Elements.Length.ToString()))
            | Shape.Dictionary t -> failwith "nyi"
            | Shape.Array t -> failwith "nyi"
            | Shape.ResizeArray t -> failwith "nyi"
            | Shape.FSharpList t -> failwith "nyi"
            | Shape.FSharpOption s ->
                s.Element.Accept {
                    new ITypeVisitor<JsonPickler<'T>> with
                        member _.Visit<'t> () =
                            let tP = getPickler<'t>()
                            mkPickler
                                (function Some v -> tP.Encode(v) | None -> Json.Null)
                                (fun _ json ->
                                    match json with
                                    | Json.Null -> Success None
                                    | json -> tP.Decode(Unchecked.defaultof<'t>)(json) |> JsonResult.map(Some)) }
            | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
                let memberHandler (field: IShapeMember<'Class>) =
                    let required = field.MemberInfo.GetCustomAttributes(typeof<JsonRequiredAttribute>, true).Length > 0
                    field.Accept { new IMemberVisitor<'Class, ('Class -> Map<string, Json> -> Map<string, Json>) * ('Class -> Map<string, Json> -> JsonResult<'Class>)> with
                    member _.Visit(shape: ShapeMember<'Class, 'Member>) =
                        let tP = getPickler()
                        (
                            (fun o map -> Map.add field.Label (tP.Encode(shape.Get o)) map),
                            (fun o map ->
                                if Map.containsKey(field.Label)(map) then
                                    match tP.Decode(shape.Get o)(map.[field.Label]) with
                                    | Success v -> Success (shape.Set o v)
                                    | Failure e -> if required then Failure <| Exception("Required field \"" + field.Label + "\" was not parsed successfully", e) else Success o
                                else if required then Failure <| Exception("Required field \"" + field.Label + "\" was not provided") else Success o) )}
                let (inserters: ('T -> Map<string, Json> -> Map<string, Json>)[]), (decoders: ('T -> Map<string, Json> -> JsonResult<'T>)[]) = shape.Fields |> Array.map memberHandler |> Array.unzip
                mkPickler
                    (fun o -> Array.fold (fun map inserter -> inserter(o)(map)) Map.empty inserters |> Json.Object)
                    (fun o json ->
                        match json with
                        | Json.Object map -> decoders |> Array.fold (fun o decoder -> match o with | Success v -> decoder(v)(map) | Failure e -> Failure e) (Success o)
                            //put this through 'T.Verify(o)
                        | _ -> Failure (Exception("Expected a JSON object")))
            | Shape.FSharpUnion t -> failwith "nyi"

            | _ -> failwith "This type is unsupported"

        let toJson<'T>(obj: 'T) = getPickler<'T>().Encode(obj)
        let fromJson<'T>(json: Json) = getPickler<'T>().Decode(Unchecked.defaultof<'T>)(json)
        let inline fromJsonRecord(json: Json): JsonResult<'T> =
            let def = (^T: (static member Default: ^T)())
            getPickler<'T>().Decode(def)(json)