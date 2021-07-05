namespace Percyqaz

module Json =
    
    open System
    open System.Globalization

    type JSON =
        | Object of Map<string, JSON>
        | Array of JSON list
        | String of string // neither of these string values should be null
        | Number of string
        | Bool of bool
        | Null

    type ParseFailure(inner: FParsec.Error.ParserError) = inherit Exception()
    type MapFailure(json: JSON, msg: string, inner: Exception option) = 
        inherit Exception(sprintf "%s\nReceived JSON: %A" msg json)
        new(json, msg) = MapFailure(json, msg, None)

    type JsonResult<'T> = Result<'T, Exception>

    type JsonSettings =
        {
            FormatExpandArrays: bool
            FormatExpandObjects: bool

            AllowNullStrings: bool
            AllowNullArrays: bool

            EncodeAllMapsAsArrays: bool
        }
        static member Default =
            {
                FormatExpandArrays = false
                FormatExpandObjects = true

                AllowNullStrings = false
                AllowNullArrays = false

                EncodeAllMapsAsArrays = false
                //Allow NaN in floats
                //Allow +-Infinity in floats
            }

    let IDPRINT x = printfn "%A" x; x

    module Json =

        type RequiredAttribute() = inherit Attribute()

        module Parsing =
            open FParsec

            // adapted directly from https://www.quanttec.com/fparsec/tutorial.html#parsing-json
            let jsonParser =
                let jvalue, jvalueRef = createParserForwardedToRef<JSON, unit>()

                let jnull  = stringReturn "null" JSON.Null
                let jtrue  = stringReturn "true" (JSON.Bool true)
                let jfalse = stringReturn "false" (JSON.Bool false)
                let jnumber = many1Satisfy (isNoneOf " \t\r\n}],") |>> JSON.Number

                let str s = pstring s
                let stringLiteral =

                    let escape =
                        anyOf "\"\\/bfnrt" |>> function | 'b' -> "\b" | 'f' -> "\u000C" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c
                    let unicodeEscape =
                        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
                        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                            (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
                            |> char |> string)
                    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
                    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

                    between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

                let ws = spaces
                let jstring = stringLiteral |>> JSON.String
                let listBetweenStrings sOpen sClose pElement f =
                    between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
                let jlist = listBetweenStrings "[" "]" jvalue JSON.Array
                let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
                let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JSON.Object)
                do jvalueRef := choice [jobject; jlist; jstring; jtrue; jfalse; jnull; jnumber]
                jvalue .>> eof

            let parseStream name stream = runParserOnStream jsonParser () name stream Text.Encoding.UTF8
            let parseFile path = runParserOnFile jsonParser () path Text.Encoding.UTF8
            let parseString str = run jsonParser str

        module Formatting =
            open System.Text

            let stringBuildJson expandObj expandArray json =

                let sb = new StringBuilder()
                let append (x: string) = sb.Append x |> ignore

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

                let appendSubstr (s: string) start count = sb.Append (s, start, count) |> ignore

                let writeString (cs: string) =
                    let rec escapeState index =
                        append (escaped cs.[index])
                        let nextIndex = index + 1
                        if nextIndex < cs.Length then
                            if isEscapeChar cs.[nextIndex] |> not then coreState nextIndex else escapeState nextIndex
                    and coreState index =
                        let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                        if nextEscapeIndex = -1 then
                            appendSubstr cs index (cs.Length - index)
                        else
                            appendSubstr cs index (nextEscapeIndex - index)
                            escapeState nextEscapeIndex
                    coreState 0

                let mutable indent = 0

                let newline() =
                    append "\n"
                    append (String.replicate indent "    ")

                let rec stringifyJson =
                    function
                    | JSON.Null -> append "null"
                    | JSON.Bool x -> append (if x then "true" else "false")
                    | JSON.Number s -> append s
                    | JSON.String s -> append "\""; writeString s; append "\""
                    | JSON.Array xs ->
                        let rec f xs =
                            match xs with
                            | [] -> ()
                            | x :: [] -> stringifyJson x
                            | x :: xs -> stringifyJson x; append ", "; (if expandArray then newline()); f xs
                        if expandArray then
                            append "["
                            indent <- indent + 1
                            newline()
                            f xs
                            indent <- indent - 1
                            newline()
                            append "]"
                        else append "["; f xs; append "]"
                    | JSON.Object m ->
                        let rec f xs =
                            match xs with
                            | [] -> ()
                            | (k, x) :: [] -> append "\""; writeString k; append "\": "; stringifyJson x
                            | (k, x) :: xs -> append "\""; writeString k; append "\": "; stringifyJson x; append ", "; (if expandObj then newline()); f xs
                        if expandObj then
                            append "{"
                            indent <- indent + 1
                            newline()
                            f (Map.toList m)
                            indent <- indent - 1
                            newline()
                            append "}"
                        else append "{"; f (Map.toList m); append "}"

                stringifyJson json
                sb.ToString()

            let formatJson (settings: JsonSettings) json = stringBuildJson settings.FormatExpandObjects settings.FormatExpandArrays json

        module Mapping =

            open FSharp.Reflection
            open TypeShape.Core
            open TypeShape.Core.Utils

            type JsonPartialCodec<'T, 'U> =
                {
                    EncodeMember: 'T -> JSON
                    DecodeMember: 'T -> JSON -> 'T

                    DefaultMember: unit -> 'U
                }
            
            type JsonCodec<'T> = 
                {
                    Encode: 'T -> JSON
                    Decode: 'T -> JSON -> 'T //may throw MapFailure exception

                    Default: unit -> 'T
                }
                member this.DecodeWithDefault json : 'T =
                    this.Decode (this.Default()) json
                member this.DecodeWithDefaultNonThrowing json : JsonResult<'T> =
                    try this.DecodeWithDefault json |> Ok
                    with :? MapFailure as e -> Error (e :> Exception)

            type CustomCodecRule =
                abstract member Get<'T> : TypeGenerationContext * JsonSettings * CustomCodecRule list -> JsonCodec<'T> option

            module Codec =

                let map (enc: 'A -> 'B) (dec: 'B -> 'A) (codec: JsonCodec<'B>) : JsonCodec<'A> =
                    let defaultInst = codec.Default >> dec
                    {
                        Encode = fun x -> enc x |> codec.Encode
                        Decode = fun inst json -> codec.Decode (enc inst) json |> dec

                        Default = defaultInst
                    }

            module Error =

                let expectedObj json = raise (MapFailure (json, "Expected a JSON object"))
                let expectedArr json = raise (MapFailure (json, "Expected a JSON array"))
                let expectedStr json = raise (MapFailure (json, "Expected a string"))
                let expectedNum json = raise (MapFailure (json, "Expected a number"))
                let expectedBool json = raise (MapFailure (json, "Expected a boolean"))
                let expectedNull json = raise (MapFailure (json, "Expected null"))

                let generic json msg = raise (MapFailure (json, msg))

                // rethrows all errors as MapFailure errors
                // only used in contexts where you know you want to catch all exceptions
                let wrap json msg (func: 'A -> 'B) : 'A -> 'B =
                    fun a ->
                        try func a
                        with err -> raise (MapFailure(json, msg, Some err))

            module Helpers =

                let cast (codec: JsonCodec<_>) : JsonCodec<'T> =
                    {
                        Encode = unbox codec.Encode
                        Decode = unbox codec.Decode

                        Default = unbox codec.Default
                    }
                
                let numericCodec (enc: 'T -> string) (dec: (string * IFormatProvider) -> 'T) =
                    {
                        Encode = enc >> JSON.Number
                        Decode =
                            fun _ json ->
                                match json with
                                | JSON.String s
                                | JSON.Number s -> Error.wrap json "Could not parse number" dec (s, CultureInfo.InvariantCulture)
                                | _ -> Error.expectedNum json
                        Default = fun () -> Unchecked.defaultof<'T>
                    }

                let numericFloatCodec (useString: 'T -> bool) (enc: 'T -> string) (dec: (string * IFormatProvider) -> 'T) =
                    {
                        Encode = fun f -> if useString f then JSON.String (enc f) else JSON.Number (enc f)
                        Decode =
                            fun _ json ->
                                match json with
                                | JSON.String s
                                | JSON.Number s -> Error.wrap json "Could not parse number" dec (s, CultureInfo.InvariantCulture)
                                | _ -> Error.expectedNum json
                        Default = fun () -> Unchecked.defaultof<'T>
                    }

            module Rules =
                
                let customCodecMethod =
                    { new CustomCodecRule with
                        member _.Get<'t>(cache, settings, rules) =
                            let mi = typeof<'t>.GetMethod("JsonCodec")
                            if not (isNull mi) then
                                mi.Invoke(null, [|cache; settings; rules|]) :?> JsonCodec<'t> |> Some
                                // todo: better error message than what this will normally throw
                            else None
                    }

                let typeRule<'T> (codecFunc: _ -> JsonCodec<'T>) =
                    { new CustomCodecRule with
                        member _.Get<'t>(cache, settings, rules) =
                            if typeof<'t> = typeof<'T> then Some (Helpers.cast (codecFunc(cache, settings, rules)) : JsonCodec<'t>) else None
                    }

            module Primitives =
                
                let uint8 = Helpers.numericCodec (fun (i: byte) -> i.ToString CultureInfo.InvariantCulture) Byte.Parse
                let int8 = Helpers.numericCodec (fun (i: sbyte) -> i.ToString CultureInfo.InvariantCulture) SByte.Parse
                let uint16 = Helpers.numericCodec (fun (i: uint16) -> i.ToString CultureInfo.InvariantCulture) UInt16.Parse
                let int16 = Helpers.numericCodec (fun (i: int16) -> i.ToString CultureInfo.InvariantCulture) Int16.Parse
                let uint32 = Helpers.numericCodec (fun (i: uint32) -> i.ToString CultureInfo.InvariantCulture) UInt32.Parse
                let int32 = Helpers.numericCodec (fun (i: int32) -> i.ToString CultureInfo.InvariantCulture) Int32.Parse
                let uint64 = Helpers.numericCodec (fun (i: uint64) -> i.ToString CultureInfo.InvariantCulture) UInt64.Parse
                let int64 = Helpers.numericCodec (fun (i: int64) -> i.ToString CultureInfo.InvariantCulture) Int64.Parse
                let bigint = Helpers.numericCodec (fun (i: bigint) -> i.ToString("R", CultureInfo.InvariantCulture)) Numerics.BigInteger.Parse
                let single = Helpers.numericFloatCodec (fun (f: single) -> Single.IsInfinity f || Single.IsNaN f) (fun (f: single) -> f.ToString("R", CultureInfo.InvariantCulture)) Single.Parse
                let double = Helpers.numericFloatCodec (fun (f: double) -> Double.IsInfinity f || Double.IsNaN f) (fun (f: double) -> f.ToString("G17", CultureInfo.InvariantCulture)) Double.Parse
                let decimal = Helpers.numericCodec (fun (f: decimal) -> f.ToString CultureInfo.InvariantCulture) Decimal.Parse

                let unit = { Encode = (fun _ -> JSON.Null); Decode = (fun _ _ -> ()); Default = fun () -> () }

                let bool =
                    {
                        Encode = JSON.Bool
                        Decode = fun _ json ->
                            match json with
                            | JSON.String "" | JSON.Number "0" | JSON.Bool false -> false
                            | JSON.String _ | JSON.Number "1" | JSON.Bool true -> true
                            | _ -> Error.expectedBool json
                        Default = fun () -> false
                    }

                let char =
                    {
                        Encode = string >> JSON.String
                        Decode = fun _ json ->
                            match json with
                            | JSON.String s when s.Length > 0 -> s.[0]
                            | _ -> Error.generic json "Expected a nonempty JSON string"
                        Default = fun () -> ' '
                    }

                let string =
                    {
                        Encode = fun (s: string) ->
                            match s with
                            | null -> JSON.Null
                            | _ -> JSON.String s
                        Decode = fun _ json ->
                            match json with
                            | JSON.String s -> s
                            | JSON.Null -> null
                            | _ -> Error.expectedStr json
                        Default = fun () -> null
                    }
                let stringNoNull =
                    {
                        Encode = fun (s: string) ->
                            match s with
                            | null -> nullArg "s"
                            | _ -> JSON.String s
                        Decode = fun _ json ->
                            match json with
                            | JSON.String s -> s
                            | _ -> Error.expectedStr json
                        Default = fun () -> ""
                    }

                let timespan = Codec.map (fun (ts: TimeSpan) -> ts.Ticks) TimeSpan.FromTicks int64

                let datetime =
                    {
                        Encode = fun (dt: DateTime) ->
                            dt.ToUniversalTime().ToString("o", CultureInfo.InvariantCulture) |> JSON.String
                        Decode = fun _ json ->
                            match json with
                            | JSON.String s ->
                                Error.wrap json "Could not parse DateTime"
                                    (fun s ->
                                        DateTime.ParseExact(
                                            s, [| "s"; "r"; "o"; "yyyy'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFK" |],
                                            CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal)) s
                            | _ -> Error.expectedStr json
                        Default = fun () -> DateTime.MinValue
                    }

                let datetimeoffset =
                    {
                        Encode = fun (dto: DateTimeOffset) ->
                            dto.ToString("o", CultureInfo.InvariantCulture) |> JSON.String
                        Decode = fun _ json ->
                            match json with
                            | JSON.String s -> 
                                Error.wrap json "Could not parse DateTimeOffset"
                                    (fun s ->
                                        DateTimeOffset.ParseExact(
                                            s, [| "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'"; "o"; "r" |],
                                            CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal)) s
                            | _ -> Error.expectedStr json
                        Default = fun () -> DateTimeOffset.MinValue
                    }

                let json =
                    {
                        Encode = id
                        Decode = fun _ -> id
                        Default = fun _ -> JSON.Null
                    }

            let rec getCodec<'T> (cache: TypeGenerationContext, settings: JsonSettings, rules: CustomCodecRule list) : JsonCodec<'T> =
                let delay (c: Cell<JsonCodec<'T>>) : JsonCodec<'T> =
                    { Encode = (fun o -> c.Value.Encode o); Decode = (fun o json -> c.Value.Decode o json); Default = (fun () -> c.Value.Default()) }
                (fun () ->
                    match cache.InitOrGetCachedValue<JsonCodec<'T>> delay with
                    Cached(value = f) -> f | NotCached t -> let p = genCodec(cache, settings, rules) in cache.Commit t p)
                |> lock cache

            and genCodec (cache: TypeGenerationContext, settings, rules) : JsonCodec<'T> =
                
                // custom codecs:

                match List.tryPick (fun (ccr: CustomCodecRule) -> ccr.Get<'T>(cache, settings, rules)) rules with
                | Some codec -> codec
                | None ->

                // defaults codecs:

                match shapeof<'T> with
                | Shape.Unit -> Helpers.cast Primitives.unit
                | Shape.Bool -> Helpers.cast Primitives.bool
                | Shape.Byte -> Helpers.cast Primitives.uint8
                | Shape.SByte -> Helpers.cast Primitives.int8
                | Shape.Int16 -> Helpers.cast Primitives.int16
                | Shape.UInt16 -> Helpers.cast Primitives.uint16
                | Shape.Int32 -> Helpers.cast Primitives.int32
                | Shape.UInt32 -> Helpers.cast Primitives.uint32
                | Shape.Int64 -> Helpers.cast Primitives.int64
                | Shape.UInt64 -> Helpers.cast Primitives.uint64
                | Shape.BigInt -> Helpers.cast Primitives.bigint
                | Shape.Single -> Helpers.cast Primitives.single
                | Shape.Double -> Helpers.cast Primitives.double
                | Shape.Decimal -> Helpers.cast Primitives.decimal
                | Shape.Char -> Helpers.cast Primitives.char
                | Shape.String -> Helpers.cast (if settings.AllowNullStrings then Primitives.string else Primitives.stringNoNull)
                | Shape.TimeSpan -> Helpers.cast Primitives.timespan
                | Shape.DateTime -> Helpers.cast Primitives.datetime
                | Shape.DateTimeOffset -> Helpers.cast Primitives.datetimeoffset
                | Shape.Enum s -> enum (cache, settings, rules) s

                | Shape.FSharpOption s -> option (cache, settings, rules) s
                | Shape.Tuple (:? ShapeTuple<'T> as shape) -> tuple (cache, settings, rules) shape
                | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) -> record (cache, settings, rules) shape
                | Shape.Dictionary s -> dict (cache, settings, rules) s
                | Shape.FSharpMap s -> map (cache, settings, rules) s
                | Shape.FSharpSet s -> set (cache, settings, rules) s
                | Shape.Array s when s.Rank = 1 -> array (cache, settings, rules) s
                | Shape.FSharpList s -> fsharplist (cache, settings, rules) s
                | Shape.ResizeArray s -> csharplist (cache, settings, rules) s
                | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
                    if typeof<'T> = typeof<JSON> then Helpers.cast Primitives.json
                    else union (cache, settings, rules) shape
                | _ -> failwithf "The type '%O' is unsupported; Create a custom rule to implement your own behaviour" typeof<'T>

            and private enum (cache, settings, rules) (s: IShapeEnum) =
                { new IEnumVisitor<JsonCodec<'T>> with
                    member _.Visit<'t, 'u when 't : enum<'u> and 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() =
                        getCodec<'u>(cache, settings, rules)
                        |> Codec.map LanguagePrimitives.EnumToValue LanguagePrimitives.EnumOfValue<'u, 't>
                        |> Helpers.cast
                } |> s.Accept

            and private option (cache, settings, rules) (s: IShapeFSharpOption) =
                { new ITypeVisitor<JsonCodec<'T>> with
                    member _.Visit<'t>() =
                        let tP =
                            if typeof<'t> = typeof<string> then Primitives.stringNoNull |> Helpers.cast
                            else getCodec<'t>(cache, settings, rules)
                        {
                            Encode = function Some v -> tP.Encode v | None -> JSON.Null
                            Decode = fun _ json ->
                                match json with
                                | JSON.Null -> None
                                | _ -> tP.DecodeWithDefault json |> Some
                            Default = fun () -> None
                        } |> Helpers.cast
                } |> s.Element.Accept

            // helper function for members of objects
            and private elem (cache, settings, rules) (field: IShapeMember<'Class>) =
                { new IMemberVisitor<'Class, _> with
                    member _.Visit (shape: ShapeMember<'Class, _>) =
                        let tP = getCodec(cache, settings, rules)
                        {
                            EncodeMember = fun o -> tP.Encode(shape.Get o)
                            DecodeMember = fun o json -> shape.Set o (tP.Decode (shape.Get o) json)
                            DefaultMember = fun () -> tP.Default() :> obj
                        }
                } |> field.Accept

            and private tuple (cache, settings, rules) (shape: ShapeTuple<'T>) =
                let codecs = shape.Elements |> Array.map (fun e -> (elem (cache, settings, rules) e, e.Label))
                {
                    Encode = fun o -> codecs |> Array.map (fun (codec, _) -> codec.EncodeMember o) |> List.ofArray |> JSON.Array
                    Decode = fun o json ->
                        match json with
                        | JSON.Array xs when List.length xs = shape.Elements.Length ->
                            let mutable o = o
                            for ((codec, _), json) in Array.ofList xs |> Array.zip codecs do
                                o <- codec.DecodeMember o json
                            o
                        | JSON.Array _ -> Error.generic json (sprintf "Expected %i items in JSON array" shape.Elements.Length)
                        | _ -> Error.expectedArr json
                    Default =
                        let typ = typeof<'T>
                        fun () ->
                            Array.map (fun (codec, _) -> codec.DefaultMember()) codecs
                            |> fun os -> FSharpValue.MakeTuple(os, typ) :?> 'T
                }

            and private record (cache, settings, rules) (shape: ShapeFSharpRecord<'T>) =
                let codecs = shape.Fields |> Array.map (fun f -> (elem (cache, settings, rules) f, f))
                
                let requireAll, defaultCtor =
                    let typ = typeof<'T>
                    let mi = typ.GetProperty("Default", typeof<'T>)
                    if isNull mi then true, fun () ->
                        Array.map (fun (codec, _) -> codec.DefaultMember()) codecs
                        |> fun os -> FSharpValue.MakeRecord(typ, os) :?> 'T
                    else false, fun () -> mi.GetValue(null) :?> 'T

                let codecs = Array.map (fun (codec, f: IShapeMember<'T>) -> (codec, f.Label, requireAll || f.MemberInfo.GetCustomAttributes(typeof<RequiredAttribute>, true).Length > 0)) codecs

                {
                    Encode = fun (o: 'T) ->
                        Array.fold
                            (fun map (codec, name, _) -> Map.add name (codec.EncodeMember o) map)
                            Map.empty codecs
                        |> JSON.Object
                    Decode = fun (o: 'T) json ->
                        match json with
                        | JSON.Object map ->
                            
                            let mutable o = o
                            for (codec, name, required) in codecs do
                                if Map.containsKey name map then
                                    o <- try codec.DecodeMember o map.[name] with :? MapFailure -> if required then reraise() else o
                                else if required then Error.generic json (sprintf "Required field '%s' was not provided" name)
                            o
                        | _ -> Error.expectedObj json
                    Default = defaultCtor
                }

            and private dict (cache, settings, rules) (s: IShapeDictionary) =
                { new IDictionaryVisitor<JsonCodec<'T>> with
                    member _.Visit<'K, 'V when 'K : equality>() =
                        let from_xs (xs: (_ * 'V) list) =
                            let d = new Collections.Generic.Dictionary<_, 'V>()
                            for (key, value) in xs do
                                d.Add(key, value)
                            d
                        if typeof<'K> = typeof<string> && not settings.EncodeAllMapsAsArrays then
                            let tP = getCodec<'V>(cache, settings, rules)
                            {
                                Encode = fun dictionary ->
                                    dictionary
                                    |> Seq.map (|KeyValue|)
                                    |> Seq.map (fun (key, value) -> (key, tP.Encode value))
                                    |> Map.ofSeq
                                    |> JSON.Object
                                Decode = 
                                    let list_tP = getCodec<(string * 'V) list>(cache, settings, rules)
                                    fun _ json ->
                                        match json with
                                        | JSON.Object map ->
                                            let d = new Collections.Generic.Dictionary<string, 'V>()
                                            for (key, innerJson) in Map.toSeq map do
                                                d.Add(key, tP.DecodeWithDefault innerJson)
                                            d
                                        // bad error message; will say expected a list when normally an object is expected
                                        | _ -> list_tP.DecodeWithDefault json |> from_xs
                                Default = fun () -> new Collections.Generic.Dictionary<string, 'V>()
                            } |> Helpers.cast
                        else
                            getCodec<('K * 'V) list>(cache, settings, rules)
                            |> Codec.map (Seq.map (|KeyValue|) >> List.ofSeq) from_xs
                            |> Helpers.cast
                } |> s.Accept

            and private map (cache, settings, rules) (s: IShapeFSharpMap) =
                { new IFSharpMapVisitor<JsonCodec<'T>> with
                    member _.Visit<'K, 'V when 'K : comparison>() =
                        if typeof<'K> = typeof<string> && not settings.EncodeAllMapsAsArrays then
                            let tP = getCodec<'V>(cache, settings, rules)
                            let list_tP = getCodec<(string * 'V) list>(cache, settings, rules)
                            {
                                Encode = Map.map (fun _ -> tP.Encode) >> JSON.Object
                                Decode = fun _ json ->
                                    match json with
                                    | JSON.Object map -> Map.map (fun _ -> tP.DecodeWithDefault) map
                                    | _ -> list_tP.DecodeWithDefault json |> Map.ofList
                                Default = fun () -> Map.empty<string, 'V>
                            } |> Helpers.cast
                        else
                            getCodec<('K * 'V) list>(cache, settings, rules)
                            |> Codec.map Map.toList Map.ofList
                            |> Helpers.cast
                } |> s.Accept

            and private set (cache, settings, rules) (s: IShapeFSharpSet) =
                { new ITypeVisitor<JsonCodec<'T>> with
                    member _.Visit<'t>() =
                        failwith "not yet implemented"
                } |> s.Element.Accept

            and private array (cache, settings, rules) (s: IShapeArray) =
                { new ITypeVisitor<JsonCodec<'T>> with
                    member _.Visit<'t>() =
                        let tP = getCodec<'t>(cache, settings, rules)
                        {
                            Encode = 
                                if settings.AllowNullArrays then
                                    function
                                    | null -> JSON.Null
                                    | xs -> Array.map tP.Encode xs |> List.ofArray |> JSON.Array
                                else
                                    function
                                    | null -> nullArg "arr"
                                    | xs -> Array.map tP.Encode xs |> List.ofArray |> JSON.Array
                            Decode = 
                                if settings.AllowNullArrays then
                                    fun _ json ->
                                        match json with
                                        | JSON.Null -> null
                                        | JSON.Array xs -> xs |> Array.ofList |> Array.map tP.DecodeWithDefault
                                        | _ -> Error.expectedArr json
                                else
                                    fun _ json ->
                                        match json with
                                        | JSON.Array xs -> xs |> Array.ofList |> Array.map tP.DecodeWithDefault
                                        | _ -> Error.expectedArr json
                            Default = fun () -> Array.empty
                        } |> Helpers.cast
                } |> s.Element.Accept

            and private fsharplist (cache, settings, rules) (s: IShapeFSharpList) =
                { new ITypeVisitor<JsonCodec<'T>> with
                    member _.Visit<'t>() =
                        let tP = getCodec<'t>(cache, settings, rules)
                        {
                            Encode = (List.map tP.Encode) >> JSON.Array
                            Decode = fun _ json ->
                                match json with
                                | JSON.Array xs -> List.map tP.DecodeWithDefault xs
                                | _ -> Error.expectedArr json
                            Default = fun () -> List.empty
                        } |> Helpers.cast
                } |> s.Element.Accept

            and private csharplist (cache, settings, rules) (s: IShapeResizeArray) =
                { new ITypeVisitor<JsonCodec<'T>> with
                    member _.Visit<'t>() =
                        let tP = getCodec<'t>(cache, settings, rules)
                        {
                            Encode = List.ofSeq >> List.map tP.Encode >> JSON.Array
                            Decode = fun _ json ->
                                match json with
                                | JSON.Array xs -> List.map tP.DecodeWithDefault xs |> ResizeArray
                                | _ -> Error.expectedArr json
                            Default = fun () -> new ResizeArray<'t>()
                        } |> Helpers.cast
                } |> s.Element.Accept

            and private union (cache, settings, rules) (shape: ShapeFSharpUnion<'T>) =
                let case (case: ShapeFSharpUnionCase<'Class>) =
                    let codecs = case.Fields |> Array.map (elem (cache, settings, rules))
                    match case.Arity with
                    | 0 ->
                        {
                            // encode is never called, string is used to encode 0-arity union cases
                            Encode = fun _ -> JSON.Null
                            Decode = fun o _ -> o
                            Default = case.CreateUninitialized
                        }
                    | 1 ->
                        {
                            Encode = fun case -> codecs.[0].EncodeMember case
                            Decode = fun o json -> codecs.[0].DecodeMember o json
                            Default = fun () ->
                                FSharpValue.MakeUnion(case.CaseInfo, [|codecs.[0].DefaultMember()|]) :?> 'Class
                        }
                    | n ->
                        {
                            Encode = fun case -> Array.map (fun codec-> codec.EncodeMember case) codecs |> List.ofArray |> JSON.Array
                            Decode = fun o json ->
                                match json with
                                | JSON.Array xs when xs.Length = n ->
                                    let mutable o = o
                                    for (codec, json) in Array.ofList xs |> Array.zip codecs do
                                        o <- codec.DecodeMember o json
                                    o
                                | JSON.Array _ -> Error.generic json (sprintf "Expected %i items in JSON array" n)
                                // maybe in future, support object with named members of union?
                                | _ -> Error.expectedArr json
                            Default = fun () ->
                                let os = Array.map (fun codec -> codec.DefaultMember()) codecs
                                FSharpValue.MakeUnion(case.CaseInfo, os) :?> 'Class
                        }
                let codecs = shape.UnionCases |> Array.map (fun c -> (case c, c.Arity))
                {
                    Encode = fun (o: 'T) ->
                        let caseId = shape.GetTag o
                        let case = shape.UnionCases.[caseId].CaseInfo
                        let (codec, arity) = codecs.[caseId]
                        if arity = 0 then JSON.String case.Name
                        else [(case.Name, codec.Encode o)] |> Map.ofList |> JSON.Object
                    Decode = fun _ json ->
                        match json with
                        | JSON.String s ->
                            let caseId = try shape.GetTag s with :? System.Collections.Generic.KeyNotFoundException -> Error.generic json "Unexpected union tag"
                            let (codec, _) = codecs.[caseId]
                            codec.DecodeWithDefault JSON.Null // will throw a sensible error for non-0-arity cases, and work for 0-arity cases
                        | JSON.Object m ->
                            let caseName, innerJson =
                                if m.IsEmpty then Error.generic json "Expected a nonempty JSON object"
                                else Map.toList m |> List.head
                            let caseId = try shape.GetTag caseName with :? System.Collections.Generic.KeyNotFoundException -> Error.generic json "Unexpected union tag"
                            let (codec, _) = codecs.[caseId]
                            codec.DecodeWithDefault innerJson
                        | _ -> Error.expectedObj json
                    Default =
                        // this is unused by Decode, but still might get consumed by regular F# code
                        let value = (fst codecs.[0]).Default()
                        fun () -> value
                }

    open System.IO
    open FParsec
    open TypeShape.Core.Utils
    open Json

    type JsonEncoder(settings: JsonSettings) =

        let cache = new TypeGenerationContext()
        let mutable rules = [Mapping.Rules.customCodecMethod]

        new() = JsonEncoder(JsonSettings.Default)

        member this.AddRule (rule: Mapping.CustomCodecRule) = rules <- rule :: rules

        member this.ToJson (obj: 'T) = Mapping.getCodec(cache, settings, rules).Encode obj

        member this.ToString (obj: 'T) = obj |> this.ToJson |> Formatting.formatJson settings

        member this.ToStream (stream: Stream) (obj: 'T) =
            use w = new StreamWriter(stream)
            obj |> this.ToJson |> Formatting.formatJson settings |> w.Write

        member this.ToFile (path, overwrite) (obj: 'T) =
            if overwrite || (File.Exists path |> not) then
                File.WriteAllText(path, this.ToString obj)
            else raise <| IOException "This file already exists!"

        member this.FromJson<'T> (json: JSON) : JsonResult<'T> = 
            let codec = Mapping.getCodec(cache, settings, rules)
            codec.DecodeWithDefaultNonThrowing json
        
        member this.FromString<'T> (str: string) : JsonResult<'T> =
            match Parsing.parseString str with
            | ParserResult.Success (res, _, _) -> Result.Ok res
            | ParserResult.Failure (_, err, _) -> Result.Error (ParseFailure(err) :> Exception)
            |> Result.bind this.FromJson<'T>

        member this.FromStream<'T> (nameOfStream, stream) =
            match Parsing.parseStream nameOfStream stream with
            | ParserResult.Success (res, _, _) -> Result.Ok res
            | ParserResult.Failure (_, err, _) -> Result.Error (ParseFailure(err) :> Exception)
            |> Result.bind this.FromJson<'T>

        member this.FromFile<'T> (path: string) =
            try Parsing.parseFile path |> Result.Ok
            with :? FileNotFoundException as e -> Result.Error (e :> Exception)
            |> Result.bind (function
                | ParserResult.Success (res, _, _) -> Result.Ok res
                | ParserResult.Failure (_, err, _) -> Result.Error (ParseFailure(err) :> Exception))
            |> Result.bind this.FromJson<'T>

        member this.Default<'T>() : 'T = Mapping.getCodec(cache, settings, rules).Default()