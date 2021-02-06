namespace Percyqaz

module Json =

    open System
    open System.Globalization

    type JSON =
    | Object of Map<string, JSON>
    | Array of JSON list
    | String of string
    | Number of string
    | True
    | False
    | Null

    type JsonResult<'T> = Success of 'T | MapFailure of Exception | ParseFailure of Exception
    type JsonMapResult<'T> = Success of 'T | Failure of Exception

    module JsonMapResult =
        let map f =
            function
            | Success x -> Success (f x)
            | Failure e -> Failure e
        let value =
            function
            | Success x -> x
            | Failure e -> raise e

    module JsonResult =
        open FParsec
        let make t (res: ParserResult<JSON, _>) =
            match res with
            | ParserResult.Success (v, _, _) ->
                match (t v): JsonMapResult<'T> with
                | JsonMapResult.Success v -> JsonResult.Success v
                | JsonMapResult.Failure e -> JsonResult.MapFailure e
            | ParserResult.Failure (e, _, _) -> JsonResult.ParseFailure (Exception(e))
        let map f =
            function
            | JsonResult.Success o -> f o
            | otherwise -> otherwise
        let value =
            function
            | JsonResult.Success o -> o
            | JsonResult.MapFailure err | JsonResult.ParseFailure err -> raise err
    
    module Json =
        type RequiredAttribute() = inherit Attribute()
    
        module Parsing =
            open FParsec

            //adapted directly from https://www.quanttec.com/fparsec/tutorial.html#parsing-json
            let jsonParser = 
                let jvalue, jvalueRef = createParserForwardedToRef<JSON, unit>()

                let jnull  = stringReturn "null" JSON.Null
                let jtrue  = stringReturn "true" JSON.True
                let jfalse = stringReturn "false" JSON.False
                let jnumber = many1Satisfy (isNoneOf " \t\r\n}],") |>> JSON.Number

                let str s = pstring s
                let stringLiteral =
                    let escape = 
                        anyOf "\"\\/bfnrt" |>> function | 'b' -> "\b" | 'f' -> "\u000C" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c
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
                let jstring = stringLiteral |>> JSON.String
                let listBetweenStrings sOpen sClose pElement f =
                    between (str sOpen) (str sClose)
                            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
                let jlist   = listBetweenStrings "[" "]" jvalue JSON.Array
                let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
                let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JSON.Object)
                do jvalueRef := choice [jobject; jlist; jstring; jtrue; jfalse; jnull; jnumber]
                jvalue .>> eof

            let parseStream name stream = runParserOnStream jsonParser () name stream (Text.Encoding.UTF8)
            let parseFile path = runParserOnFile jsonParser () path (Text.Encoding.UTF8)
            let parseString str = run jsonParser str

        module Formatting =
            open System.Text
        
            let stringBuildJson json = 
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
                    | JSON.Null -> sb.Append "null"
                    | JSON.True -> sb.Append "true"
                    | JSON.False -> sb.Append "false" 
                    | JSON.Number s -> sb.Append s
                    | JSON.String s -> sb |> append "\"" |> writeString s |> append "\""
                    | JSON.Array xs ->
                        let rec f xs sb =
                            match xs with [] -> sb | x::[] -> stringifyJson(sb)(x) | x::xs -> stringifyJson(sb)(x) |> append ", " |> f xs
                        sb |> append "[" |> f xs |> append "]"
                    | JSON.Object m ->
                        let rec f xs sb =
                            match xs with
                            | [] -> sb
                            | (k, x)::[] ->
                                sb |> append "\"" |> writeString k |> append "\": " |> fun sb -> stringifyJson(sb)(x)
                            | (k, x)::xs ->
                                sb |> append "\"" |> writeString k |> append "\": " |> fun sb -> stringifyJson(sb)(x) |> append ", " |> f xs
                        sb |> append "{" |> f (Map.toList m) |> append "}"
                stringifyJson (StringBuilder()) json

            let formatJson json = (stringBuildJson json).ToString()

        module Mapping =

            open TypeShape.Core
            open TypeShape.Core.Utils

            type JsonPickler<'T> = {
                Encode: 'T -> JSON
                Decode: 'T -> JSON -> JsonMapResult<'T>
            }

            let private cache = new TypeGenerationContext()

            let mkPickler (encode: 'T -> JSON) (decode: 'T -> JSON -> JsonMapResult<'T>) = { Encode = unbox encode; Decode = unbox decode }
            let jsonErr(desc, json) = Exception(sprintf "%s\nReceived JSON: %A" desc json) |> Failure

            let private decodeList decoder json =
                let o = Unchecked.defaultof<'t>
                let rec f (n, list) =
                    match list with
                    | [] -> Success []
                    | (x :: xs) ->
                        match decoder o x with
                        | Success y ->
                            match f (n + 1, xs) with
                            | Success ys -> Success (y :: ys)
                            | Failure e -> Failure e
                        | Failure e -> Failure <| Exception(sprintf "Error in item %i" n, e)
                match json with
                | JSON.Array xs -> f (0, xs)
                | _ -> jsonErr("Expected a JSON array", json)

            let inline private mkNumericPickler (encode: 'T -> string) (decode: (string *  IFormatProvider) -> 'T) =
                mkPickler (encode >> JSON.Number) (fun _ json -> match json with JSON.String s | JSON.Number s -> (try decode(s, CultureInfo.InvariantCulture) |> Success with err -> Failure err) | json -> jsonErr("Expected a number", json))

            let rec getPickler<'T>() : JsonPickler<'T> =
                let delay (c : Cell<JsonPickler<'T>>) : JsonPickler<'T> = { Encode = (fun o -> c.Value.Encode o); Decode = (fun o json -> c.Value.Decode o json) }
                match cache.InitOrGetCachedValue<JsonPickler<'T>> delay with
                | Cached(value = f) -> f | NotCached t -> let p = genPickler<'T>() in cache.Commit t p

            and private genPickler<'T>() : JsonPickler<'T> =
                let mi = typeof<'T>.GetProperty("Pickler")
                if isNull mi |> not then (try mi.GetValue(null) :?> JsonPickler<'T> with _ -> failwithf "Type %O must define a static property Pickler of type JsonPickler<%O>" typeof<'T> typeof<'T>) else
                match shapeof<'T> with
                | Shape.Unit -> mkPickler (fun _ -> JSON.Null) (fun _ _ -> Success ())
                | Shape.Bool ->
                    mkPickler (fun b -> if b then JSON.True else JSON.False)
                        (fun _ json -> 
                            match json with
                            | JSON.String "" | JSON.Number "0" | JSON.Null | JSON.False -> Success false
                            | JSON.String _ | JSON.Number "1" | JSON.True -> Success true
                            | _ -> jsonErr("Expected a boolean value", json))
                | Shape.Byte -> mkNumericPickler (fun (i: byte) -> i.ToString(CultureInfo.InvariantCulture)) (Byte.Parse)
                | Shape.SByte -> mkNumericPickler (fun (i: sbyte) -> i.ToString(CultureInfo.InvariantCulture)) (SByte.Parse)
                | Shape.Int16 -> mkNumericPickler (fun (i: int16) -> i.ToString(CultureInfo.InvariantCulture)) (Int16.Parse)
                | Shape.UInt16 -> mkNumericPickler (fun (i: uint16) -> i.ToString(CultureInfo.InvariantCulture)) (UInt16.Parse)
                | Shape.Int32 -> mkNumericPickler (fun (i: int32) -> i.ToString(CultureInfo.InvariantCulture)) (Int32.Parse)
                | Shape.UInt32 -> mkNumericPickler (fun (i: uint32) -> i.ToString(CultureInfo.InvariantCulture)) (UInt32.Parse)
                | Shape.Int64 -> mkNumericPickler (fun (i: int64) -> i.ToString(CultureInfo.InvariantCulture)) (Int64.Parse)
                | Shape.UInt64 -> mkNumericPickler (fun (i: uint64) -> i.ToString(CultureInfo.InvariantCulture)) (UInt64.Parse)
                | Shape.IntPtr -> failwith "nyi"
                | Shape.UIntPtr -> failwith "nyi"
                | Shape.BigInt -> mkNumericPickler (fun (i: bigint) -> i.ToString("R", CultureInfo.InvariantCulture)) (Numerics.BigInteger.Parse)
                | Shape.Single -> mkNumericPickler (fun (f: single) -> f.ToString("R", CultureInfo.InvariantCulture)) (Single.Parse)
                | Shape.Double -> mkNumericPickler (fun (f: double) -> f.ToString("G17", CultureInfo.InvariantCulture)) (Double.Parse)
                | Shape.Decimal -> mkNumericPickler (fun (f: decimal) -> f.ToString(CultureInfo.InvariantCulture)) (Decimal.Parse)
                | Shape.Char -> mkPickler (string >> JSON.String) (fun _ json -> match json with JSON.String s when s.Length > 0 -> Success s.[0] | json -> jsonErr("Expected a nonempty JSON string", json))
                | Shape.String -> mkPickler (JSON.String) (fun _ json -> match json with JSON.String s -> Success s | json -> jsonErr("Expected a JSON string", json))
                | Shape.TimeSpan -> failwith "nyi"
                | Shape.DateTime ->
                    mkPickler (fun (dt: DateTime) -> dt.ToUniversalTime().ToString("o", CultureInfo.InvariantCulture) |> JSON.String)
                        (fun _ json -> 
                            match json with
                            | JSON.String s -> (try DateTime.ParseExact (s, [| "s"; "r"; "o"; "yyyy'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFK" |], CultureInfo.InvariantCulture, DateTimeStyles.AdjustToUniversal) |> Success with err -> Failure err)
                            | json -> jsonErr("Expected a formatted DateTime string", json))
                | Shape.DateTimeOffset -> 
                    mkPickler (fun (dt: DateTimeOffset) -> dt.ToString("o", CultureInfo.InvariantCulture) |> JSON.String)
                        (fun _ json ->
                            match json with
                            | JSON.String s -> (try DateTimeOffset.ParseExact (s, [| "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'"; "o"; "r" |], CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) |> Success with err -> Failure err)
                            | _ -> jsonErr("Expected a formatted DateTime string", json))
                | Shape.Enum s ->
                    s.Accept { new IEnumVisitor<JsonPickler<'T>> with 
                    member _.Visit<'t, 'u when 't : enum<'u> and 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() =
                        let tP = getPickler<'u>()
                        mkPickler (LanguagePrimitives.EnumToValue >> tP.Encode) (fun _ json -> tP.Decode(Unchecked.defaultof<'u>)json |> JsonMapResult.map LanguagePrimitives.EnumOfValue<'u,'t>)}
                | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
                    let elemHandler (field: IShapeMember<'Class>) =
                        field.Accept { new IMemberVisitor<'Class, _> with
                        member _.Visit (shape: ShapeMember<'Class, _>) =
                            let tP = getPickler()
                            (fun o -> tP.Encode(shape.Get o)),
                            (fun o json ->
                                match tP.Decode(try shape.Get o with err -> Unchecked.defaultof<'a>)(json) with
                                | Success v -> Success (shape.Set o v)
                                | Failure e -> Failure <| Exception("Failed to parse " + field.Label, e)) }
                    let encoders, decoders = shape.Elements |> Array.map elemHandler |> Array.unzip
                    mkPickler
                        (fun o -> Array.map (fun enc -> enc(o)) encoders |> List.ofArray |> JSON.Array)
                        (fun _ json -> 
                            match json with 
                            | JSON.Array xs when List.length xs = shape.Elements.Length -> 
                                Array.ofList xs |> Array.zip decoders |> Array.fold(fun o (dec, json) -> match o with Success o -> dec(o)(json) | Failure e -> Failure e) (Success <| shape.CreateUninitialized())
                            | _ -> jsonErr("Expected a JSON array of length " + shape.Elements.Length.ToString(), json))
                | Shape.FSharpMap s ->
                    s.Accept { new IFSharpMapVisitor<JsonPickler<'T>> with
                    member _.Visit<'K, 'V when 'K : comparison>() =
                        if typeof<'K> <> typeof<string> then failwith "Map must have keys of type string"
                        let tP = getPickler()
                        mkPickler ((Map.map (fun _ -> tP.Encode)) >> JSON.Object)
                            (fun _ json ->
                                match json with
                                | JSON.Object m ->
                                    m |> Map.map (fun k -> tP.Decode Unchecked.defaultof<'V>) |> Map.toList
                                    |> List.fold
                                        (fun s kv -> match (s, kv) with (Failure e, _) -> Failure e | (Success xs, (k, Success v)) -> Success ((k, v) :: xs) | (_, (k, Failure e)) -> Failure <| Exception(sprintf "Failed to parse key '%s'" k, e))(Success [])
                                    |> JsonMapResult.map Map.ofList
                                | _ -> jsonErr("Expected a JSON object", json)) }
                | Shape.Dictionary s ->
                    s.Accept { new IDictionaryVisitor<JsonPickler<'T>> with
                    member _.Visit<'K, 'V when 'K : equality>() =
                        if typeof<'K> <> typeof<string> then failwith "Dictionary must have keys of type string"
                        let tP = getPickler()
                        mkPickler (fun (d: System.Collections.Generic.Dictionary<string, 'V>) -> d |> Seq.map (fun kv -> (kv.Key, tP.Encode kv.Value)) |> Map.ofSeq |> JSON.Object)
                            (fun _ json ->
                                match json with
                                | JSON.Object m ->
                                    m |> Map.map (fun k -> tP.Decode Unchecked.defaultof<'V>) |> Map.toList
                                    |> List.fold
                                        (fun s kv -> match (s, kv) with (Failure e, _) -> Failure e | (Success xs, (k, Success v)) -> Success ((k, v) :: xs) | (_, (k, Failure e)) -> Failure <| Exception(sprintf "Failed to parse key '%s'" k, e))(Success [])
                                    |> JsonMapResult.map Map.ofList |> JsonMapResult.map (System.Collections.Generic.Dictionary)
                                | _ -> jsonErr("Expected a JSON object", json)) }
                | Shape.Array s when s.Rank = 1 -> s.Element.Accept { new ITypeVisitor<JsonPickler<'T>> with member _.Visit<'t>() = let tP = getPickler<'t>() in mkPickler (List.ofArray >> List.map tP.Encode >> JSON.Array) (fun _ json -> decodeList tP.Decode json |> JsonMapResult.map Array.ofList) }
                | Shape.ResizeArray s -> s.Element.Accept { new ITypeVisitor<JsonPickler<'T>> with member _.Visit<'t>() = let tP = getPickler<'t>() in mkPickler (List.ofSeq >> List.map tP.Encode >> JSON.Array) (fun _ json -> decodeList tP.Decode json |> JsonMapResult.map ResizeArray) }
                | Shape.FSharpList s -> s.Element.Accept { new ITypeVisitor<JsonPickler<'T>> with member _.Visit<'t>() = let tP = getPickler<'t>() in mkPickler (List.map tP.Encode >> JSON.Array) (fun _ -> decodeList tP.Decode) }
                | Shape.FSharpOption s ->
                    s.Element.Accept { new ITypeVisitor<JsonPickler<'T>> with
                    member _.Visit<'t> () =
                        let tP = getPickler()
                        mkPickler (function Some v -> tP.Encode(v) | None -> JSON.Null)
                            (fun _ json -> match json with JSON.Null -> Success None | json -> tP.Decode(Unchecked.defaultof<'t>)(json) |> JsonMapResult.map(Some)) }
                | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
                    let allRequired, defaultRec =
                        let mi = typeof<'T>.GetProperty("Default", typeof<'T>)
                        if isNull mi then true, fun () -> shape.CreateUninitialized()
                        else false, fun () -> mi.GetValue(null) :?> 'T
                    let memberHandler (field: IShapeMember<'Class>) =
                        let required = allRequired || field.MemberInfo.GetCustomAttributes(typeof<RequiredAttribute>, true).Length > 0
                        field.Accept { new IMemberVisitor<'Class, _> with
                        member _.Visit(shape: ShapeMember<'Class, _>) =
                            let tP = getPickler()
                            ((fun o map -> Map.add field.Label (tP.Encode(shape.Get o)) map),
                                (fun o map ->
                                    if Map.containsKey(field.Label)(map) then
                                        match tP.Decode(try shape.Get o with err -> Unchecked.defaultof<_>)(map.[field.Label]) with
                                        | Success v -> Success (shape.Set o v)
                                        | Failure e -> if required then Failure <| Exception("Required field \"" + field.Label + "\" was not parsed successfully", e) else Success o
                                    else if required then Failure <| Exception("Required field \"" + field.Label + "\" was not provided") else Success o) )}
                    let inserters, decoders = shape.Fields |> Array.map memberHandler |> Array.unzip
                    mkPickler
                        (fun o -> Array.fold (fun map inserter -> inserter(o)(map)) Map.empty inserters |> JSON.Object)
                        (fun o json ->
                            let o = if obj.ReferenceEquals(o, null) then defaultRec() else o
                            match json with
                            | JSON.Object map -> decoders |> Array.fold (fun o decoder -> match o with | Success v -> decoder(v)(map) | Failure e -> Failure e) (Success o)
                            | _ -> jsonErr("Expected a JSON object", json))
                | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
                    let elemHandler (field: IShapeMember<'Class>) =
                        field.Accept { new IMemberVisitor<'Class, _> with
                        member _.Visit (shape: ShapeMember<'Class, _>) =
                            let tP = getPickler()
                            (fun o -> tP.Encode(shape.Get o)),
                            (fun o json ->
                                match tP.Decode(try shape.Get o with err -> Unchecked.defaultof<'b>)(json) with
                                | Success v -> Success (shape.Set o v)
                                | Failure e -> Failure <| Exception("Failed to parse " + field.Label, e)) }
                    let caseHandler (case: ShapeFSharpUnionCase<'Class>) =
                        let encoders, decoders = case.Fields |> Array.map elemHandler |> Array.unzip
                        (
                            (match case.Fields.Length with
                            | 0 -> fun _ -> JSON.Null
                            | 1 -> fun case -> encoders.[0](case)
                            | _ -> fun case -> Array.map (fun e -> e(case)) encoders |> List.ofArray |> JSON.Array),
                            (match case.Fields.Length with
                            | 0 -> fun _ _ -> Success <| case.CreateUninitialized()
                            | 1 -> fun _ json -> decoders.[0](case.CreateUninitialized())json
                            | n -> fun _ json ->
                                match json with
                                | JSON.Array xs ->
                                    let xs = List.toArray xs
                                    if xs.Length = n then
                                        Array.fold2 (fun o dec j -> match o with Success o -> dec o j | Failure e -> Failure e) (Success <| case.CreateUninitialized()) decoders xs
                                    else jsonErr(sprintf "Expected a JSON array with %i elements for case %s" n case.CaseInfo.Name, json)
                                | _ -> jsonErr("Expected a JSON array for case " + case.CaseInfo.Name, json)),
                            case.Arity)
                    let cases = shape.UnionCases |> Array.map caseHandler
                    mkPickler
                        (fun o ->
                            let t = shape.GetTag (o: 'T)
                            let c = shape.UnionCases.[t].CaseInfo.Name
                            let (e, _, a) = cases.[t]
                            if a = 0 then JSON.String c else [(c , e o)] |> Map.ofList |> JSON.Object)
                        (fun o json ->
                            match json with
                            | JSON.String s ->
                                try let (_, d, a) = cases.[shape.GetTag s]
                                    if a = 0 then d o JSON.Null else jsonErr(sprintf "Expected a JSON object because tag '%s' has additional data" s, json)
                                with err -> Failure (Exception(sprintf "Unexpected tag '%s'" s, err))
                            | JSON.Object m ->
                                let (s, j) = Map.toList m |> List.head
                                try let (_, d, a) =  cases.[shape.GetTag s]
                                    d o j
                                with err -> Failure (Exception(sprintf "Unexpected tag '%s'" s, err))
                            | _ -> jsonErr("Expected a JSON object or a JSON string", json))
                | _ -> failwithf "The type '%O' is unsupported; Declare a static property Pickler to implement your own behaviour" typeof<'T>

        type JSON with static member Pickler: Mapping.JsonPickler<JSON> = Mapping.mkPickler(id)(fun _ -> id >> JsonMapResult.Success)

        let toJson<'T>(obj: 'T) = Mapping.getPickler<'T>().Encode(obj)
        let toString<'T>(obj: 'T) = obj |> toJson<'T> |> Formatting.formatJson
        let toStream<'T>(stream: System.IO.Stream)(obj: 'T) =
            use w = new System.IO.StreamWriter(stream)
            w.Write(obj |> toJson<'T> |> Formatting.formatJson)
        let toFile<'T>(file, overwrite)(obj: 'T) =
            if overwrite || (System.IO.File.Exists(file) |> not) then
                System.IO.File.WriteAllText(file, obj |> toJson<'T> |> Formatting.formatJson)
            else failwithf "Overwriting existing file %s is disallowed" file

        let fromJson<'T>(json: JSON) = Mapping.getPickler<'T>().Decode(Unchecked.defaultof<'T>)(json)
        let fromString<'T>(str: string) = str |> Parsing.parseString |> JsonResult.make fromJson<'T>
        let fromStream<'T>(nameOfStream, stream) = stream |> Parsing.parseStream nameOfStream |> JsonResult.make fromJson<'T>
        let fromFile<'T>(filePath) = try filePath |> Parsing.parseFile |> JsonResult.make fromJson<'T> with err -> JsonResult.ParseFailure(err)