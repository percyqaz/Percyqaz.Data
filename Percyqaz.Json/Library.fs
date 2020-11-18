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
        
    type JsonResult<'T> = Success of 'T | Failure of Exception
    module JsonResult =
        let map f =
            function
            | Success x -> Success (f x)
            | Failure e -> Failure e
    
    module Parsing =
        open FParsec

        //adapted directly from https://www.quanttec.com/fparsec/tutorial.html#parsing-json
        let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

        let jnull  = stringReturn "null" Json.Null
        let jtrue  = stringReturn "true"  (Json.True)
        let jfalse = stringReturn "false" (Json.False)
        let jnumber = many1Satisfy (isNoneOf " \t\r\n") |>> Json.Number

        let str s = pstring s
        let stringLiteral =
            let escape =  anyOf "\"\\/bfnrt"
                          |>> function
                              | 'b' -> "\b"
                              | 'f' -> "\u000C"
                              | 'n' -> "\n"
                              | 'r' -> "\r"
                              | 't' -> "\t"
                              | c   -> string c
            let unicodeEscape =
                let hex2int c = (int c &&& 15) + (int c >>> 6)*9
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string)
            let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
            between (str "\"") (str "\"")
                    (stringsSepBy normalCharSnippet escapedCharSnippet)
        let ws = spaces
        let jstring = stringLiteral |>> Json.String
        let listBetweenStrings sOpen sClose pElement f =
            between (str sOpen) (str sClose)
                    (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
        let jlist   = listBetweenStrings "[" "]" jvalue Json.Array
        let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> Json.Object)
        do jvalueRef := choice [jobject; jlist; jstring; jnumber; jtrue; jfalse; jnull]

    module Formatting =
        open System.Text
        
        
        let formatJson json = 
            let escapeChars =
                [| '"'; '\\'; '\n'; '\r'; '\t'; '\b'; '\f'
                   '\u0000'; '\u0001'; '\u0002'; '\u0003'
                   '\u0004'; '\u0005'; '\u0006'; '\u0007'
                   '\u000B'; '\u000E'; '\u000F'
                   '\u0010'; '\u0011'; '\u0012'; '\u0013'
                   '\u0014'; '\u0015'; '\u0016'; '\u0017'
                   '\u0018'; '\u0019'; '\u001A'; '\u001B'
                   '\u001C'; '\u001D'; '\u001E'; '\u001F' |]
            let isEscapeChar = function | '"' | '\\' -> true | c when c >= '\u0000' && c <= '\u001F' -> true | _ -> false
            let isEscapeCharPred = System.Predicate<_> isEscapeChar
            let escaped = function
                | '"' -> @"\""" | '\\' -> @"\\" | '\n' -> @"\n" | '\r' -> @"\r" | '\t' -> @"\t" | '\f' -> @"\f" | '\b' -> @"\b"
                | '\u0000' -> @"\u0000" | '\u0001' -> @"\u0001" | '\u0002' -> @"\u0002" | '\u0003' -> @"\u0003" | '\u0004' -> @"\u0004"
                | '\u0005' -> @"\u0005" | '\u0006' -> @"\u0006" | '\u0007' -> @"\u0007" | '\u000B' -> @"\u000B" | '\u000E' -> @"\u000E"
                | '\u000F' -> @"\u000F" | '\u0010' -> @"\u0010" | '\u0011' -> @"\u0011" | '\u0012' -> @"\u0012" | '\u0013' -> @"\u0013"
                | '\u0014' -> @"\u0014" | '\u0015' -> @"\u0015" | '\u0016' -> @"\u0016" | '\u0017' -> @"\u0017" | '\u0018' -> @"\u0018"
                | '\u0019' -> @"\u0019" | '\u001A' -> @"\u001A" | '\u001B' -> @"\u001B" | '\u001C' -> @"\u001C" | '\u001D' -> @"\u001D"
                | '\u001E' -> @"\u001E" | '\u001F' -> @"\u001F" | c -> @"\u" + (int c).ToString("X4", CultureInfo.InvariantCulture)

            let inline append (s: string) (sb: StringBuilder) = sb.Append s
            let inline appendSubstr (sb: StringBuilder) (s: string) start count = sb.Append (s, start, count) |> ignore

            let writeString (cs: string) (sb: StringBuilder) =
                let rec escapeState index =
                    append (escaped cs.[index]) sb |> ignore
                    let nextIndex = index + 1
                    if nextIndex < cs.Length then
                        if isEscapeChar cs.[nextIndex] |> not then coreState nextIndex else escapeState nextIndex
                and coreState index =
                    let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                    if nextEscapeIndex = -1 then
                        appendSubstr sb cs index (cs.Length - index)
                    else
                        appendSubstr sb cs index (nextEscapeIndex - index)
                        escapeState nextEscapeIndex
                coreState 0
                sb

            let rec stringifyJson (sb: StringBuilder) =
                let inline append (s: string) (sb: StringBuilder) = sb.Append s
                function
                | Json.Null -> sb.Append "null"
                | Json.True -> sb.Append "true"
                | Json.False -> sb.Append "false"
                | Json.Number s -> sb.Append s
                | Json.String s -> sb |> append "\"" |> writeString s |> append "\""
                | Json.Array xs ->
                    let rec f xs sb =
                        match xs with [] -> sb | x::[] -> stringifyJson(sb)(x) | x::xs -> stringifyJson(sb)(x) |> append ", " |> f xs
                    sb |> append "[" |> f xs |> append "]"
                | Json.Object m ->
                    let rec f xs sb =
                        match xs with
                        | [] -> sb
                        | (k, x)::[] ->
                            sb |> append "\"" |> append k |> append "\": " |> fun sb -> stringifyJson(sb)(x)
                        | (k, x)::xs ->
                            sb |> append "\"" |> append k |> append "\": " |> fun sb -> stringifyJson(sb)(x) |> append ", " |> f xs
                    sb |> append "{" |> f (Map.toList m) |> append "}"
            (stringifyJson (StringBuilder()) json).ToString()

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

        let private cache = new TypeGenerationContext()

        let private mkPickler (encode: 'T -> Json) (decode: 'T -> Json -> JsonResult<'T>) =
            {
                Encode = unbox encode
                Decode = unbox decode
            }

        let inline private mkNumericPickler (encode: 'T -> string) (decode: (string *  IFormatProvider) -> 'T) =
            mkPickler (encode >> Json.Number) (fun _ json -> match json with Json.String s | Json.Number s -> (try decode(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err) | _ -> Exception("Expected a number") |> Failure)

        let rec getPickler<'T>() : JsonPickler<'T> =
            let delay (c : Cell<JsonPickler<'T>>) : JsonPickler<'T> =
                { Encode = fun o -> c.Value.Encode o
                  Decode = fun o json -> c.Value.Decode o json }
            match cache.InitOrGetCachedValue<JsonPickler<'T>> delay with
            | Cached(value = f) -> f
            | NotCached t ->
                let p = genPickler<'T>()
                cache.Commit t p

        and private genPickler<'T>() : JsonPickler<'T> =
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
                    member _.Visit (shape: ShapeMember<'Class, _>) =
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
                    member _.Visit(shape: ShapeMember<'Class, _>) =
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
            | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
                let elemHandler (field: IShapeMember<'Class>) =
                    field.Accept { new IMemberVisitor<'Class, _> with
                    member _.Visit (shape: ShapeMember<'Class, _>) =
                        let tP = getPickler()
                        (fun o -> tP.Encode(shape.Get o)),
                        (fun o json ->
                            match tP.Decode(shape.Get o)(json) with
                            | Success v -> Success (shape.Set o v)
                            | Failure e -> Failure <| Exception("Failed to parse " + field.Label, e)) }
                let caseHandler (case: ShapeFSharpUnionCase<'Class>) =
                    case.Fields
                    |> Array.map elemHandler
                shape.UnionCases
                |> Array.map caseHandler
                |> ignore
                failwith "nyi"

            | _ -> failwith "This type is unsupported"

    let toJson<'T>(obj: 'T) = Mapping.getPickler<'T>().Encode(obj)
    let fromJson<'T>(json: Json) = Mapping.getPickler<'T>().Decode(Unchecked.defaultof<'T>)(json)
    let inline fromJsonRecord(json: Json): JsonResult<'T> =
        let def = (^T: (static member Default: ^T)())
        Mapping.getPickler<'T>().Decode(def)(json)