namespace Percyqaz.Json

open System
open System.Linq
open System.Collections.Generic
open System.Globalization

[<RequireQualifiedAccess>]
type JSON =
    | Object of Map<string, JSON>
    | Array of JSON list
    | String of string // neither of these string values should be null
    | Number of string
    | Bool of bool
    | Null

module Json =

    type Settings =
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
            }

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

        let formatJson (settings: Settings) json = stringBuildJson settings.FormatExpandObjects settings.FormatExpandArrays json

    type CachedCodec<'T> = { To: 'T -> JSON; From: JSON -> 'T }

    [<AbstractClass>]
    type Context(settings: Settings) =
        abstract member GetCodec<'T> : unit -> CachedCodec<'T>
        member this.Settings = settings
    
    [<AbstractClass>]
    type Codec<'T>() =
        abstract member To : Context -> ('T -> JSON)
        abstract member From : Context -> (JSON -> 'T) // or throw exception

    module Codecs =

        type Unit() =
            inherit Codec<unit>()
            override this.To (ctx: Context) = fun _ -> JSON.Null
            override this.From (ctx: Context) = fun _ -> ()

        type Bool() =
            inherit Codec<bool>()
            override this.To (ctx: Context) = JSON.Bool
            override this.From (ctx: Context) = function JSON.Bool b -> b | json -> failwithf "Expected True or False, got: %O" json

        type UInt8() =
            inherit Codec<uint8>()
            override this.To (ctx: Context) = 
                (fun (i: uint8) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Byte.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt8: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type Int8() =
            inherit Codec<int8>()
            override this.To (ctx: Context) = 
                (fun (i: int8) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = SByte.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int8: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type UInt16() =
            inherit Codec<uint16>()
            override this.To (ctx: Context) = 
                (fun (i: uint16) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt16.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt16: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type Int16() =
            inherit Codec<int16>()
            override this.To (ctx: Context) = 
                (fun (i: int16) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int16.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int16: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type UInt32() =
            inherit Codec<uint32>()
            override this.To (ctx: Context) = 
                (fun (i: uint32) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt32.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt32: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type Int32() =
            inherit Codec<int32>()
            override this.To (ctx: Context) = 
                (fun (i: int32) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int32.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int32: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type UInt64() =
            inherit Codec<uint64>()
            override this.To (ctx: Context) = 
                (fun (i: uint64) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt64.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt64: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json
        
        type Int64() =
            inherit Codec<int64>()
            override this.To (ctx: Context) = 
                (fun (i: int64) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int64.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int64: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json
                
        type Float32() =
            inherit Codec<float32>()
            override this.To (ctx: Context) =
                ( fun (f: float32) -> 
                    let s = f.ToString CultureInfo.InvariantCulture
                    if Single.IsNaN f || Single.IsInfinity f then JSON.String s else JSON.Number s
                )
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Single.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Float32: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json
                
        type Float64() =
            inherit Codec<float>()
            override this.To (ctx: Context) =
                ( fun (f: float) -> 
                    let s = f.ToString CultureInfo.InvariantCulture
                    if Double.IsNaN f || Double.IsInfinity f then JSON.String s else JSON.Number s
                )
            override this.From (ctx: Context) =
                function 
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Float64: %s" s
                | json -> failwithf "Expected a JSON number, got: %O" json

        type Char() =
            inherit Codec<char>()
            override this.To (ctx: Context) = string >> JSON.String
            override this.From (ctx: Context) = 
                function 
                | JSON.String s when s.Length > 0 -> s.[0]
                | json -> failwithf "Expected a nonempty JSON string, got: %O" json
        
        type String() =
            inherit Codec<string>()
            override this.To (ctx: Context) = 
                if ctx.Settings.AllowNullStrings then 
                    function null -> JSON.Null | s -> JSON.String s
                else
                    function null -> failwith "Null strings not permitted" | s -> JSON.String s

            override this.From (ctx: Context) =
                if ctx.Settings.AllowNullStrings then
                    function
                    | JSON.String s -> s
                    | JSON.Null -> null
                    | json -> failwithf "Expected a JSON string or null, got: %O" json
                else
                    function JSON.String s -> s | json -> failwithf "Expected a JSON string, got: %O" json

        type Json() =
            inherit Codec<JSON>()
            override this.To _ = id
            override this.From _ = id

        type List<'T>() =
            inherit Codec<'T list>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                List.map cdc.To >> JSON.Array

            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function JSON.Array xs -> List.map cdc.From xs | json -> failwithf "Expected a JSON array, got: %O" json

        type Array<'T>() =
            inherit Codec<'T array>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                Array.map cdc.To >> List.ofArray >>JSON.Array
        
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function JSON.Array xs -> List.map cdc.From xs |> Array.ofList | json -> failwithf "Expected a JSON array, got: %O" json

        type Option<'T>() =
            inherit Codec<'T option>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function Some v -> cdc.To v | None -> JSON.Null

            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function JSON.Null -> None | json -> Some (cdc.From json)
                
        type ValueOption<'T>() =
            inherit Codec<'T voption>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function ValueSome v -> cdc.To v | ValueNone -> JSON.Null
                
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function JSON.Null -> ValueNone | json -> ValueSome (cdc.From json)

        type Tuple2<'A, 'B>() =
            inherit Codec<'A * 'B>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun (a, b) -> JSON.Array [c_a.To a; c_b.To b]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                function
                | JSON.Array [a; b] -> (c_a.From a, c_b.From b)
                | json -> failwithf "Expected a JSON array with 2 elements, got: %O" json
                
        type Tuple3<'A, 'B, 'C>() =
            inherit Codec<'A * 'B * 'C>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                fun (a, b, c) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                function
                | JSON.Array [a; b; c] -> (c_a.From a, c_b.From b, c_c.From c)
                | json -> failwithf "Expected a JSON array with 3 elements, got: %O" json
        
        type Tuple4<'A, 'B, 'C, 'D>() =
            inherit Codec<'A * 'B * 'C * 'D>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                fun (a, b, c, d) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c; c_d.To d]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                function
                | JSON.Array [a; b; c; d] -> (c_a.From a, c_b.From b, c_c.From c, c_d.From d)
                | json -> failwithf "Expected a JSON array with 4 elements, got: %O" json
        
        type Tuple5<'A, 'B, 'C, 'D, 'E>() =
            inherit Codec<'A * 'B * 'C * 'D * 'E>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                fun (a, b, c, d, e) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c; c_d.To d; c_e.To e]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                function
                | JSON.Array [a; b; c; d; e] -> (c_a.From a, c_b.From b, c_c.From c, c_d.From d, c_e.From e)
                | json -> failwithf "Expected a JSON array with 5 elements, got: %O" json

open Json

type Json(settings: Settings) as this =
        
    let codecs = ResizeArray<Type>()
    let cache = Dictionary<Type, obj>()

    let ctx = { new Context(settings) with
        override _.GetCodec<'T>() = this.GetCodec<'T>() }

    member this.WithCodec<'Codec>() = 
        let ty = typeof<'Codec>
        let bt = ty.BaseType
        if typeof<Codec<_>>.GetGenericTypeDefinition() = bt.GetGenericTypeDefinition() then
            codecs.Add ty; this
        else failwithf "Type %O must be assignable from Codec<>" ty

    member this.WithDefaults() =
        let codecs_module = typeof<Codecs.Unit>.DeclaringType
        for ty in codecs_module.GetNestedTypes() do
            codecs.Add ty
        this

    member private this.GenCodec<'T>() : CachedCodec<'T> =

        let ty = typeof<'T>
        let cdc = 
            Seq.tryPick ( fun (candidate : Type) ->

                let candidate = if candidate.IsGenericType then candidate.GetGenericTypeDefinition() else candidate

                let type_arguments = candidate.GetGenericArguments()

                // returns true if by assigning values generic parameters you can get from source to target
                // returns false if cannot reach target
                let rec matchup (source: Type) (target: Type) =
                    if source.IsGenericTypeParameter then
                        // assign type argument
                        let i = Array.IndexOf(type_arguments, source)
                        type_arguments.[i] <- target
                        true
                    elif source.IsAssignableTo target then 
                        true
                    else
                    
                    let t_args = target.GetGenericArguments()
                    let base_args = source.GetGenericArguments()

                    if target.IsArray && source.IsArray then
                        matchup (source.GetElementType()) (target.GetElementType())
                    else

                    t_args.Length > 0 &&
                    t_args.Length = base_args.Length &&
                    (target.IsGenericType && source.IsGenericType && target.GetGenericTypeDefinition() = source.GetGenericTypeDefinition()) &&
                    Array.zip base_args t_args |> Array.forall (fun (source, target) -> matchup source target)

                if matchup (candidate.BaseType.GetGenericArguments().First()) ty then
                    let gen_type = if type_arguments.Length > 0 then candidate.MakeGenericType(type_arguments) else candidate
                    let cdc = unbox<Codec<'T>> (Activator.CreateInstance gen_type)
                    Some cdc
                else None

                ) codecs
        match cdc with
        | Some cdc -> { To = cdc.To ctx; From = cdc.From ctx }
        | None -> failwithf "No codec found for type %O" ty

    member this.GetCodec<'T>() : CachedCodec<'T> =
        let ty = typeof<'T>
        if cache.ContainsKey ty then 
            unbox cache.[ty]
        else
            let new_codec = this.GenCodec<'T>()
            cache.Add(ty, unbox new_codec)
            new_codec