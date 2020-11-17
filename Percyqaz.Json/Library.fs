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

        (*
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
                | _ -> Exception("Expected a json array") |> Failure*)

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

        let inline mkNumericPickler (encode: 'T -> string) (decode: (string *  IFormatProvider) -> 'T) =
            mkPickler (encode >> Json.Number) (fun _ json -> match json with Json.String s | Json.Number s -> (try decode(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err) | _ -> Exception("Expected a number") |> Failure)

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
                        | Json.String "" | Json.Number "0" | Json.Null | Json.False -> Success false
                        | Json.String _ | Json.Number "1" | Json.True -> Success true
                        | _ -> Failure <| Exception("Expected a boolean value"))
            | Shape.Byte
            | Shape.SByte -> failwith "nyi"
            | Shape.Int16 -> mkNumericPickler (fun (i: int16) -> i.ToString(CultureInfo.InvariantCulture)) (Int16.Parse)
            | Shape.Int32 -> mkNumericPickler (fun (i: int32) -> i.ToString(CultureInfo.InvariantCulture)) (Int32.Parse)
            | Shape.Int64 -> mkNumericPickler (fun (i: int64) -> i.ToString(CultureInfo.InvariantCulture)) (Int64.Parse)
            | Shape.IntPtr -> failwith "nyi"
            | Shape.UInt16 -> mkNumericPickler (fun (i: uint16) -> i.ToString(CultureInfo.InvariantCulture)) (UInt16.Parse)
            | Shape.UInt32 -> mkNumericPickler (fun (i: uint32) -> i.ToString(CultureInfo.InvariantCulture)) (UInt32.Parse)
            | Shape.UInt64 -> mkNumericPickler (fun (i: uint64) -> i.ToString(CultureInfo.InvariantCulture)) (UInt64.Parse)
            | Shape.UIntPtr -> failwith "nyi"
            | Shape.BigInt -> mkNumericPickler (fun (i: bigint) -> i.ToString("R", CultureInfo.InvariantCulture)) (Numerics.BigInteger.Parse)
            | Shape.Single -> mkNumericPickler (fun (f: single) -> f.ToString("R", CultureInfo.InvariantCulture)) (Single.Parse)
            | Shape.Double -> mkNumericPickler (fun (f: double) -> f.ToString("G17", CultureInfo.InvariantCulture)) (Double.Parse)
            | Shape.Decimal -> mkNumericPickler (fun (f: decimal) -> f.ToString(CultureInfo.InvariantCulture)) (Decimal.Parse)
            | Shape.Char -> mkPickler (string >> Json.String) (fun _ json -> match json with Json.String s when s.Length > 0 -> Success s.[0] | _ -> Failure <| Exception("Expected a nonempty JSON string"))
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
                let verifier =
                    let mi = typeof<'T>.GetMethod("Verify")
                    if isNull mi then id else (mi.CreateDelegate(typeof<Func<'T, 'T>>) :?> Func<'T, 'T>) |> fun d o -> d.Invoke(o)
                let memberHandler (field: IShapeMember<'Class>) =
                    let required = field.MemberInfo.GetCustomAttributes(typeof<JsonRequiredAttribute>, true).Length > 0
                    field.Accept { new IMemberVisitor<'Class, _> with
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
                let inserters, decoders = shape.Fields |> Array.map memberHandler |> Array.unzip
                mkPickler
                    (fun o -> Array.fold (fun map inserter -> inserter(o)(map)) Map.empty inserters |> Json.Object)
                    (fun o json ->
                        match json with
                        | Json.Object map -> decoders |> Array.fold (fun o decoder -> match o with | Success v -> decoder(v)(map) | Failure e -> Failure e) (Success o) |> JsonResult.map verifier
                        | _ -> Failure (Exception("Expected a JSON object")))
            | Shape.FSharpUnion t -> failwith "nyi"

            | _ -> failwith "This type is unsupported"

        let toJson<'T>(obj: 'T) = getPickler<'T>().Encode(obj)
        let fromJson<'T>(json: Json) = getPickler<'T>().Decode(Unchecked.defaultof<'T>)(json)
        let inline fromJsonRecord(json: Json): JsonResult<'T> =
            let def = (^T: (static member Default: ^T)())
            getPickler<'T>().Decode(def)(json)