namespace Percyqaz

open System
open System.Globalization
open System.Collections.Generic

module Json =

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

    module Mapping =

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
            static member inline ToJson((a, b): ^A * ^B) = [toJson a; toJson b] |> Json.Array
            static member inline FromJson((a_, b_): ^A * ^B, json: Json) =
                match json with
                | Json.Array [a; b] ->
                    (fromJson (a_, a), fromJson (b_, b)) |> JsonResult.tuple2
                | _ -> Exception("Expected a json array with 2 elements") |> Failure
            static member inline ToJson((a, b, c): ^A * ^B * ^C) = [toJson a; toJson b; toJson c] |> Json.Array
            static member inline FromJson((a_, b_, c_): ^A * ^B * ^C, json: Json) =
                match json with
                | Json.Array [a; b; c] ->
                    (fromJson (a_, a), fromJson (b_, b), fromJson (c_, c)) |> JsonResult.tuple3
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

            static member ToJson(o: obj) =
                let t = o.GetType()
                Json.Null

        let test =
            toJson(("Hello", 6, 30.0f))