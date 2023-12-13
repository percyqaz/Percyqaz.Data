namespace Percyqaz.Data

open System
open System.Linq
open System.Collections.Generic
open System.Globalization
open FSharp.Reflection

[<RequireQualifiedAccess>]
type JSON =
    | Object of Map<string, JSON>
    | Array of JSON list
    | String of string // neither of these string values should be null
    | Number of string
    | Bool of bool
    | Null
    override this.ToString() =
        match this with
        | Object xs ->
            sprintf "{ %s ... }" (xs |> Map.toSeq |> Seq.truncate 2 |> Seq.map (fun (k, v) -> sprintf "\"%s\": %O" k v) |> String.concat ", ")
        | Array xs ->
            sprintf "[ %s ... ]" (xs |> List.truncate 2 |> List.map (fun x -> x.ToString()) |> String.concat ", ")
        | String s -> "\"" + s + "\""
        | Number n -> "'" + n + "'"
        | Bool b -> if b then "True" else "False"
        | Null -> "Null"

module Json =

    type Settings =
        {
            FormatExpandArrays: bool
            FormatExpandObjects: bool

            AllowNullStrings: bool
            AllowNullArrays: bool

            EncodeAllMapsAsArrays: bool
            EncodeEnumsAsStrings: bool
        }
        static member Default =
            {
                FormatExpandArrays = false
                FormatExpandObjects = true

                AllowNullStrings = false
                AllowNullArrays = false

                EncodeAllMapsAsArrays = false
                EncodeEnumsAsStrings = true
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

        open System.IO

        let private writeToStream (s: Stream) (expandObj: bool) (expandArray: bool) (json: JSON) =

            let tw = new StreamWriter(s)
            let write (x: string) = tw.Write x
            let writeSub (x: string) (start: int) (count: int) = tw.Write (x.Substring(start, count))

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

            let writeString (cs: string) =
                let rec escapeState index =
                    write (escaped cs.[index])
                    let nextIndex = index + 1
                    if nextIndex < cs.Length then
                        if isEscapeChar cs.[nextIndex] |> not then coreState nextIndex else escapeState nextIndex
                and coreState index =
                    let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                    if nextEscapeIndex = -1 then
                        writeSub cs index (cs.Length - index)
                    else
                        writeSub cs index (nextEscapeIndex - index)
                        escapeState nextEscapeIndex
                coreState 0

            let mutable indent = 0

            let newline() =
                write "\n"
                write (String.replicate indent "    ")

            let rec writeJson =
                function
                | JSON.Null -> write "null"
                | JSON.Bool x -> write (if x then "true" else "false")
                | JSON.Number s -> write s
                | JSON.String s -> write "\""; writeString s; write "\""
                | JSON.Array xs ->
                    let rec f xs =
                        match xs with
                        | [] -> ()
                        | x :: [] -> writeJson x
                        | x :: xs -> writeJson x; write ", "; (if expandArray then newline()); f xs
                    if expandArray then
                        write "["
                        indent <- indent + 1
                        newline()
                        f xs
                        indent <- indent - 1
                        newline()
                        write "]"
                    else write "["; f xs; write "]"
                | JSON.Object m ->
                    let rec f xs =
                        match xs with
                        | [] -> ()
                        | (k, x) :: [] -> write "\""; writeString k; write "\": "; writeJson x
                        | (k, x) :: xs -> write "\""; writeString k; write "\": "; writeJson x; write ", "; (if expandObj then newline()); f xs
                    if expandObj then
                        write "{"
                        indent <- indent + 1
                        newline()
                        f (Map.toList m)
                        indent <- indent - 1
                        newline()
                        write "}"
                    else write "{"; f (Map.toList m); write "}"

            writeJson json
            tw.Flush()

        let formatJsonToStream (settings: Settings) (stream: Stream) (json: JSON) =
            writeToStream stream settings.FormatExpandObjects settings.FormatExpandArrays json

        let formatJsonString (settings: Settings) (json: JSON) = 
            use ms = new MemoryStream()
            writeToStream ms settings.FormatExpandObjects settings.FormatExpandArrays json
            System.Text.Encoding.Default.GetString(ms.ToArray())

    type CachedCodec<'T> = 
        { To: 'T -> JSON; From: 'T -> JSON -> 'T; Default: unit -> 'T }
        member this.FromDefault json : 'T = this.From (this.Default()) json

    [<AbstractClass>]
    type Context(settings: Settings) =
        abstract member GetCodec<'T> : unit -> CachedCodec<'T>
        abstract member GetBoxedCodec<'T> : unit -> CachedCodec<obj>
        member this.Settings = settings
    
    [<AbstractClass>]
    type Codec<'T>() =
        abstract member To : Context -> ('T -> JSON)
        abstract member From : Context -> ('T -> JSON -> 'T) // or throw exception
        abstract member Default : Context -> (unit -> 'T)
        default this.Default _ = fun () -> Unchecked.defaultof<'T>

    module Codecs =

        type Unit() =
            inherit Codec<unit>()
            override this.To (ctx: Context) = fun _ -> JSON.Null
            override this.From (ctx: Context) = fun _ _ -> ()

        type Bool() =
            inherit Codec<bool>()
            override this.To (ctx: Context) = JSON.Bool
            override this.From (ctx: Context) = fun _ json -> 
                match json with 
                | JSON.Bool b -> b
                | _ -> failwithf "Expected True or False, got: %O" json

        type UInt8() =
            inherit Codec<uint8>()
            override this.To (ctx: Context) = 
                (fun (i: uint8) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Byte.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt8: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type Int8() =
            inherit Codec<int8>()
            override this.To (ctx: Context) = 
                (fun (i: int8) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = SByte.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int8: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type UInt16() =
            inherit Codec<uint16>()
            override this.To (ctx: Context) = 
                (fun (i: uint16) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt16.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt16: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type Int16() =
            inherit Codec<int16>()
            override this.To (ctx: Context) = 
                (fun (i: int16) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int16.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int16: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type UInt32() =
            inherit Codec<uint32>()
            override this.To (ctx: Context) = 
                (fun (i: uint32) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt32.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt32: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type Int32() =
            inherit Codec<int32>()
            override this.To (ctx: Context) = 
                (fun (i: int32) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int32.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int32: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type UInt64() =
            inherit Codec<uint64>()
            override this.To (ctx: Context) = 
                (fun (i: uint64) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = UInt64.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for UInt64: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json
        
        type Int64() =
            inherit Codec<int64>()
            override this.To (ctx: Context) = 
                (fun (i: int64) -> i.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Int64.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Int64: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json
                
        type Float32() =
            inherit Codec<float32>()
            override this.To (ctx: Context) =
                ( fun (f: float32) -> 
                    let s = f.ToString CultureInfo.InvariantCulture
                    if Single.IsNaN f || Single.IsInfinity f then JSON.String s else JSON.Number s
                )
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Single.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Float32: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json
                
        type Float64() =
            inherit Codec<float>()
            override this.To (ctx: Context) =
                ( fun (f: float) -> 
                    let s = f.ToString CultureInfo.InvariantCulture
                    if Double.IsNaN f || Double.IsInfinity f then JSON.String s else JSON.Number s
                )
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Float64: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type Decimal() =
            inherit Codec<decimal>()
            override this.To (ctx: Context) =
                (fun (d: decimal) -> d.ToString CultureInfo.InvariantCulture) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Decimal.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for Decimal: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type BigInt() =
            inherit Codec<bigint>()
            override this.To (ctx: Context) =
                (fun (i: bigint) -> i.ToString("R", CultureInfo.InvariantCulture)) >> JSON.Number
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s | JSON.Number s -> 
                    let ok, res = Numerics.BigInteger.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
                    if ok then res else failwithf "Parse failure for BigInteger: %s" s
                | _ -> failwithf "Expected a JSON number, got: %O" json

        type Char() =
            inherit Codec<char>()
            override this.To (ctx: Context) = string >> JSON.String
            override this.From (ctx: Context) = fun _ json ->
                match json with
                | JSON.String s when s.Length > 0 -> s.[0]
                | _ -> failwithf "Expected a nonempty JSON string, got: %O" json
        
        type String() =
            inherit Codec<string>()
            override this.To (ctx: Context) = 
                if ctx.Settings.AllowNullStrings then 
                    function null -> JSON.Null | s -> JSON.String s
                else
                    function null -> failwith "Null strings not permitted" | s -> JSON.String s

            override this.From (ctx: Context) =
                if ctx.Settings.AllowNullStrings then fun _ json ->
                    match json with
                    | JSON.String s -> s
                    | JSON.Null -> null
                    | _ -> failwithf "Expected a JSON string or null, got: %O" json
                else fun _ json ->
                    match json with JSON.String s -> s | _ -> failwithf "Expected a JSON string, got: %O" json

            override this.Default _ = fun _ -> String.Empty

        type Json() =
            inherit Codec<JSON>()
            override this.To _ = id
            override this.From _ = fun _ x -> x

        type List<'T>() =
            inherit Codec<'T list>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                List.map cdc.To >> JSON.Array

            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                fun _ json ->
                match json with
                | JSON.Array xs -> List.map cdc.FromDefault xs
                | _ -> failwithf "Expected a JSON array, got: %O" json
            
            override this.Default _ = fun _ -> []

        type ResizeArray<'T>() =
            inherit Codec<Collections.Generic.List<'T>>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                Seq.map cdc.To >> List.ofSeq >> JSON.Array
        
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                fun l json ->
                match json with
                | JSON.Array xs -> 
                    l.Clear()
                    for x in List.map cdc.FromDefault xs do l.Add x
                    l
                | _ -> failwithf "Expected a JSON array, got: %O" json

            override this.Default _ = fun _ -> Collections.Generic.List<'T>()

        type Array<'T>() =
            inherit Codec<'T array>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                if ctx.Settings.AllowNullArrays then
                    fun arr -> if isNull arr then JSON.Null else Array.map cdc.To arr |> List.ofArray |> JSON.Array
                else Array.map cdc.To >> List.ofArray >> JSON.Array
        
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                if ctx.Settings.AllowNullArrays then
                    fun _ json ->
                    match json with
                    | JSON.Array xs -> List.map cdc.FromDefault xs |> Array.ofList
                    | JSON.Null -> null
                    | _ -> failwithf "Expected a JSON array or null, got: %O" json
                else
                    fun _ json ->
                    match json with
                    | JSON.Array xs -> List.map cdc.FromDefault xs |> Array.ofList
                    | _ -> failwithf "Expected a JSON array, got: %O" json

            override this.Default _ = fun _ -> null

        type Set<'T when 'T : comparison>() =
            inherit Codec<Collections.Set<'T>>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                Seq.map cdc.To >> List.ofSeq >> JSON.Array

            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                fun _ json ->
                match json with
                | JSON.Array xs -> List.map cdc.FromDefault xs |> set
                | _ -> failwithf "Expected a JSON array, got: %O" json

            override this.Default _ = fun _ -> Set.empty

        type Dictionary<'K, 'V when 'K : comparison>() =
            inherit Codec<Collections.Generic.Dictionary<'K, 'V>>()
            override this.To (ctx: Context) =
                if not ctx.Settings.EncodeAllMapsAsArrays && typeof<'K> = typeof<string> then
                    let cdc = ctx.GetCodec<'V>()

                    Seq.map (|KeyValue|)
                    >> Seq.map (fun (key, value) -> (key.ToString(), cdc.To value))
                    >> Map.ofSeq
                    >> JSON.Object
                else
                    let cdc = ctx.GetCodec<('K * 'V) list>()
                    Seq.map (|KeyValue|)
                    >> List.ofSeq
                    >> cdc.To
            override this.From (ctx: Context) =
                let list_cdc = ctx.GetCodec<('K * 'V) list>()
                let v_cdc = ctx.GetCodec<'V>()
                fun dict json ->
                match json with
                | JSON.Object xs when typeof<'K> = typeof<string> ->
                    dict.Clear()
                    for (k, v) in xs |> Map.map (fun k v -> v_cdc.From (v_cdc.Default()) v) |> Map.toSeq do
                        dict.Add(unbox<'K> k, v)
                    dict
                | _ -> 
                    for (k, v) in list_cdc.From (list_cdc.Default()) json do
                        dict.Add(k, v)
                    dict
                
            override this.Default _ = fun _ -> new Collections.Generic.Dictionary<'K, 'V>()

        type Map<'K, 'V when 'K : comparison and 'V : comparison>() =
            inherit Codec<Collections.Map<'K, 'V>>()
            override this.To (ctx: Context) =
                if not ctx.Settings.EncodeAllMapsAsArrays && typeof<'K> = typeof<string> then
                    let cdc = ctx.GetCodec<'V>()

                    Map.toSeq
                    >> Seq.map (fun (key, value) -> (key.ToString(), cdc.To value))
                    >> Map.ofSeq
                    >> JSON.Object
                else
                    let cdc = ctx.GetCodec<('K * 'V) list>()
                    Map.toList
                    >> cdc.To

            override this.From (ctx: Context) =
                let list_cdc = ctx.GetCodec<('K * 'V) list>()
                let v_cdc = ctx.GetCodec<'V>()
                fun _ json ->
                match json with
                | JSON.Object xs when typeof<'K> = typeof<string> ->
                    Map.toSeq xs
                    |> Seq.map (fun (key, value) -> (unbox<'K> key, v_cdc.From (v_cdc.Default()) value))
                    |> Map.ofSeq
                | _ -> list_cdc.From (list_cdc.Default()) json |> Map.ofList

            override this.Default _ = fun _ -> Map.empty

        type Option<'T>() =
            inherit Codec<'T option>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function Some v -> cdc.To v | None -> JSON.Null

            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                fun existing json ->
                match json with JSON.Null -> None | _ -> Some (cdc.From (existing |> Option.defaultWith cdc.Default) json)
            
            override this.Default _ = fun _ -> None
                
        type ValueOption<'T>() =
            inherit Codec<'T voption>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                function ValueSome v -> cdc.To v | ValueNone -> JSON.Null
                
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<'T>()
                fun existing json ->
                match json with JSON.Null -> ValueNone | _ -> ValueSome (cdc.From (existing |> ValueOption.defaultWith cdc.Default) json)

            override this.Default _ = fun _ -> ValueNone

        type Tuple2<'A, 'B>() =
            inherit Codec<'A * 'B>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun (a, b) -> JSON.Array [c_a.To a; c_b.To b]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun _ json ->
                match json with
                | JSON.Array [a; b] -> (c_a.FromDefault a, c_b.FromDefault b)
                | _ -> failwithf "Expected a JSON array with 2 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun _ -> (c_a.Default(), c_b.Default())
                
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
                fun _ json ->
                match json with
                | JSON.Array [a; b; c] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c)
                | json -> failwithf "Expected a JSON array with 3 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default())
        
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
                fun _ json ->
                match json with
                | JSON.Array [a; b; c; d] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c, c_d.FromDefault d)
                | json -> failwithf "Expected a JSON array with 4 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default(), c_d.Default())
        
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
                fun _ json ->
                match json with
                | JSON.Array [a; b; c; d; e] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c, c_d.FromDefault d, c_e.FromDefault e)
                | json -> failwithf "Expected a JSON array with 5 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default(), c_d.Default(), c_e.Default())

        type StructTuple2<'A, 'B>() =
            inherit Codec<struct ('A * 'B)>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun (struct (a, b)) -> JSON.Array [c_a.To a; c_b.To b]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun _ json ->
                match json with
                | JSON.Array [a; b] -> (c_a.FromDefault a, c_b.FromDefault b)
                | _ -> failwithf "Expected a JSON array with 2 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                fun _ -> (c_a.Default(), c_b.Default())
        
                
        type StructTuple3<'A, 'B, 'C>() =
            inherit Codec<struct ('A * 'B * 'C)>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                fun (struct (a, b, c)) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                fun _ json ->
                match json with
                | JSON.Array [a; b; c] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c)
                | json -> failwithf "Expected a JSON array with 3 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default())
        
        type StructTuple4<'A, 'B, 'C, 'D>() =
            inherit Codec<struct ('A * 'B * 'C * 'D)>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                fun (struct (a, b, c, d)) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c; c_d.To d]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                fun _ json ->
                match json with
                | JSON.Array [a; b; c; d] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c, c_d.FromDefault d)
                | json -> failwithf "Expected a JSON array with 4 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default(), c_d.Default())
        
        type StructTuple5<'A, 'B, 'C, 'D, 'E>() =
            inherit Codec<struct ('A * 'B * 'C * 'D * 'E)>()
            override this.To (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                fun (struct (a, b, c, d, e)) -> JSON.Array [c_a.To a; c_b.To b; c_c.To c; c_d.To d; c_e.To e]
            override this.From (ctx: Context) =
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                fun _ json ->
                match json with
                | JSON.Array [a; b; c; d; e] -> (c_a.FromDefault a, c_b.FromDefault b, c_c.FromDefault c, c_d.FromDefault d, c_e.FromDefault e)
                | json -> failwithf "Expected a JSON array with 5 elements, got: %O" json
            override this.Default (ctx: Context) = 
                let c_a = ctx.GetCodec<'A>()
                let c_b = ctx.GetCodec<'B>()
                let c_c = ctx.GetCodec<'C>()
                let c_d = ctx.GetCodec<'D>()
                let c_e = ctx.GetCodec<'E>()
                fun _ -> (c_a.Default(), c_b.Default(), c_c.Default(), c_d.Default(), c_e.Default())

        type Timespan() =
            inherit Codec<TimeSpan>()
            override this.To (ctx: Context) =
                let cdc = ctx.GetCodec<int64>()
                fun timespan -> cdc.To timespan.Ticks
            override this.From (ctx: Context) =
                let cdc = ctx.GetCodec<int64>()
                fun _ json -> TimeSpan.FromTicks(cdc.FromDefault json)
            override this.Default (ctx: Context) = fun _ -> TimeSpan.Zero

        type Datetime() =
            inherit Codec<DateTime>()
            override this.To (ctx: Context) =
                fun datetime -> datetime.ToUniversalTime().ToString("o", CultureInfo.InvariantCulture) |> JSON.String
            override this.From (ctx: Context) =
                fun _ json ->
                match json with
                | JSON.String s ->
                    let ok, res = DateTime.TryParseExact(s, [| "s"; "r"; "o"; "yyyy'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFK" |], CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal)
                    if ok then res else failwithf "Unable to parse DateTime from string: %s, got: %O" s json
                | _ -> failwithf "Expected a JSON string, got: %O" json
            override this.Default (ctx: Context) = fun _ -> DateTime.MinValue

        type DatetimeOffset() =
            inherit Codec<DateTimeOffset>()
            override this.To (ctx: Context) =
                fun datetimeoffset -> datetimeoffset.ToUniversalTime().ToString("o", CultureInfo.InvariantCulture) |> JSON.String
            override this.From (ctx: Context) =
                fun _ json ->
                match json with
                | JSON.String s ->
                    let ok, res = DateTimeOffset.TryParseExact(s, [| "o"; "r"; "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'" |], CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal)
                    if ok then res else failwithf "Unable to parse DateTime from string: %s, got: %O" s json
                | _ -> failwithf "Expected a JSON string, got: %O" json
            override this.Default (ctx: Context) = fun _ -> DateTimeOffset.MinValue

        type Guid() =
            inherit Codec<System.Guid>()
            override this.To (ctx: Context) =
                fun guid -> guid.ToString("N", CultureInfo.InvariantCulture) |> JSON.String
            override this.From (ctx: Context) =
                fun _ json ->
                match json with
                | JSON.String s -> 
                    let ok, res = Guid.TryParse s
                    if ok then res else failwithf "Unable to parse Guid from string: %s, got: %O" s json
                | _ -> failwithf "Expected a JSON string, got: %O" json
                

    type [<Sealed>] AutoCodecAttribute(RequireAll: bool) =
        inherit Attribute()
        new() = AutoCodecAttribute(true)
        member this.RequireAll = RequireAll

    type [<Sealed>] RequiredAttribute() = inherit Attribute()
    type [<Sealed>] OptionalAttribute() = inherit Attribute()

    module AutoCodecs =

        type Default_Delegate<'T> = delegate of unit -> 'T

        let boxed_codec(ctx: Context, ty: Type) =
            ctx.GetType().GetMethod(nameof ctx.GetBoxedCodec).MakeGenericMethod(ty).Invoke(ctx, [||])
            |> unbox<CachedCodec<obj>>
        
        let record<'T> (ctx: Context) (requireAll: bool) : CachedCodec<'T> =
            let ty = typeof<'T>

            let fields = FSharpType.GetRecordFields ty
            let fieldNames = fields |> Array.map (fun f -> f.Name)
            let fieldRequired = 
                fields
                |> Array.map (fun f -> 
                        if requireAll then f.GetCustomAttributes(typeof<OptionalAttribute>, false).Length = 0
                        else f.GetCustomAttributes(typeof<RequiredAttribute>, false).Length > 0
                    )
            let reader = FSharpValue.PreComputeRecordReader ty
            let constructor = FSharpValue.PreComputeRecordConstructor ty
            let codecs = fields |> Array.map (fun f -> boxed_codec (ctx, f.PropertyType))
            let mi = ty.GetProperty("Default", typeof<'T>)
            
            let defaultFunc =
                if isNull mi then
                    fun () -> 
                        codecs
                        |> Array.map (fun x -> x.Default())
                        |> constructor
                        |> unbox<'T>
                else 
                    let d = Default_Delegate<'T>.CreateDelegate(typeof<Default_Delegate<'T>>, mi.GetMethod)
                    fun () -> unbox<'T> (d.DynamicInvoke())
                
            {
                To = fun record ->
                    let values = reader record
                    let mutable map = Map.empty
                    for i = 0 to fieldNames.Length - 1 do
                        map <- Map.add fieldNames.[i] (codecs.[i].To values.[i]) map
                    JSON.Object map

                From = fun record json ->
                    match json with
                    | JSON.Object xs ->
                        let values = reader record
                        for i = 0 to fieldNames.Length - 1 do
                            if xs.ContainsKey(fieldNames.[i]) then
                                values.[i] <- codecs.[i].From values.[i] xs.[fieldNames.[i]]
                            elif fieldRequired.[i] then failwithf "Missing required value: %s, got: %O" fieldNames.[i] json
                        unbox<'T> (constructor values)
                    | _ -> failwithf "Expected a JSON object, got: %O" json

                Default = defaultFunc
            }

        let union<'T> (ctx: Context) : CachedCodec<'T> =
            let ty = typeof<'T>
            let unionCases = FSharpType.GetUnionCases ty
            let caseNames = unionCases |> Array.map (fun c -> c.Name)
            let tagReader = FSharpValue.PreComputeUnionTagReader ty

            let case (ci: UnionCaseInfo) : CachedCodec<'T> =
                let fields = ci.GetFields()
                let reader = FSharpValue.PreComputeUnionReader ci
                let constructor = FSharpValue.PreComputeUnionConstructor ci
                let codecs = fields |> Array.map (fun f -> boxed_codec (ctx, f.PropertyType))
                
                if fields.Length = 0 then
                    {
                        To = fun (x: 'T) -> JSON.String ci.Name
                        From = fun _ _ -> constructor [||] :?> 'T
                        Default = fun () -> constructor [||] :?> 'T
                    }
                elif fields.Length = 1 then
                    {
                        To = fun (x: 'T) -> 
                            let inner = codecs.[0].To (reader x).[0]
                            JSON.Object (Map.ofList [(ci.Name, inner)])
                        From = fun (d: 'T) (json: JSON) -> 
                            // todo: use the reader to provide defaults
                            constructor [|codecs.[0].FromDefault json|] :?> 'T
                        Default = fun () -> Array.map (fun cdc -> cdc.Default()) codecs |> constructor |> unbox<'T>
                    }
                else
                    {
                        To = fun (x: 'T) -> 
                            let inner = reader x |> Array.map2 (fun cdc v -> cdc.To v) codecs |> List.ofArray |> JSON.Array
                            JSON.Object (Map.ofList [(ci.Name, inner)])
                        From = fun (d: 'T) (json: JSON) ->
                            match json with
                            | JSON.Array xs ->
                                // todo: use the reader to provide defaults
                                xs |> Array.ofList |> Array.map2 (fun (cdc: CachedCodec<obj>) v -> cdc.FromDefault v) codecs |> constructor |> unbox<'T>
                            | _ -> failwithf "Expected nested JSON array with %i elements, got: %O" fields.Length json
                        Default = fun () -> Array.map (fun cdc -> cdc.Default()) codecs |> constructor |> unbox<'T>
                    }

            let caseCodecs = unionCases |> Array.map case
            {
                To = fun (x: 'T) ->
                    let i = tagReader x
                    caseCodecs.[i].To x
                From = fun (x: 'T) (json: JSON) ->
                    match json with
                    | JSON.Object xs ->
                        let i = Array.IndexOf(caseNames, xs.Keys.First())
                        if i < 0 then failwithf "Unrecognised case name: %s at: %O" (xs.Keys.First()) json
                        caseCodecs.[i].From x (xs.Values.First())
                    | JSON.String s ->
                        let i = Array.IndexOf(caseNames, s)
                        if i < 0 then failwithf "Unrecognised case name: %s at: %O" s json
                        caseCodecs.[i].From x JSON.Null
                    | _ -> failwithf "Expected a JSON object encoding union, got: %O" json
                Default = caseCodecs.[0].Default
            }

        let enum<'T> (ctx: Context) : CachedCodec<'T> =
            let ty = typeof<'T>
            let underlying_ty = Enum.GetUnderlyingType ty
            let cdc = boxed_codec (ctx, underlying_ty)

            let default_value = ty.GetEnumValues().Cast<'T>().First()

            if ctx.Settings.EncodeEnumsAsStrings then
                {
                    To = fun (x: 'T) -> 
                        if Enum.IsDefined(ty, x) then JSON.String (x.ToString())
                        else cdc.To x
                    From = fun _ json ->
                        match json with
                        | JSON.String s -> 
                            let ok, res = Enum.TryParse(ty, s)
                            if ok then res :?> 'T else failwithf "Unrecognised enum name: %s at: %O" s json
                        | _ -> cdc.FromDefault json :?> 'T
                    Default = fun () -> default_value
                }
            else
                {
                    To = fun (x: 'T) -> cdc.To x
                    From = fun _ json ->
                        match json with
                        | JSON.String s -> 
                            let ok, res = Enum.TryParse(ty, s)
                            if ok then res :?> 'T else failwithf "Unrecognised enum name: %s at: %O" s json
                        | _ -> cdc.FromDefault json :?> 'T
                    Default = fun () -> default_value
                }

open Json
open System.IO

exception ParseFailure of FParsec.Error.ParserError
type ParserResult<'Result, 'UserState> = FParsec.CharParsers.ParserResult<'Result, 'UserState>
type JsonResult<'T> = Result<'T, Exception>

type Json(settings: Settings) as this =
        
    let codecs = ResizeArray<Type>()
    let cache = Dictionary<Type, obj>()
    let autocodecs = Dictionary<Type, bool>()

    let ctx = { new Context(settings) with
        override _.GetCodec<'T>() = this.GetCodec<'T>()
        override _.GetBoxedCodec<'T>() = this.GetBoxedCodec<'T>() }

    member this.WithCodec<'Codec>() = 
        let ty = typeof<'Codec>
        let bt = ty.BaseType
        if typeof<Codec<_>>.GetGenericTypeDefinition() = bt.GetGenericTypeDefinition() then
            codecs.Add ty; this
        else failwithf "Type %O must be assignable from Codec<>" ty

    member this.WithAutoCodec<'T>(requireAll: bool) =
        autocodecs.TryAdd(typeof<'T>, requireAll) |> ignore; this
    member this.WithAutoCodec<'T>() = this.WithAutoCodec<'T>(true)

    member this.WithDefaults() =
        let codecs_module = typeof<Codecs.Unit>.DeclaringType
        for ty in codecs_module.GetNestedTypes() do
            codecs.Add ty
        this

    member private this.GenCodec<'T>() : CachedCodec<'T> =

        let ty = typeof<'T>

        if FSharpType.IsRecord ty && ty.Name.Contains("<>f__AnonymousType") then
            AutoCodecs.record<'T> ctx true

        elif FSharpType.IsRecord ty && autocodecs.ContainsKey ty then
            AutoCodecs.record<'T> ctx autocodecs.[ty]

        elif FSharpType.IsRecord ty && ty.GetCustomAttributes(typeof<AutoCodecAttribute>, false).Length > 0 then
            AutoCodecs.record<'T> ctx (ty.GetCustomAttributes(typeof<AutoCodecAttribute>, false).First() :?> AutoCodecAttribute).RequireAll

        elif FSharpType.IsUnion ty && autocodecs.ContainsKey ty then
            AutoCodecs.union<'T> ctx

        elif FSharpType.IsUnion ty && ty.GetCustomAttributes(typeof<AutoCodecAttribute>, false).Length > 0 then
            AutoCodecs.union<'T> ctx

        elif ty.IsEnum then
            AutoCodecs.enum<'T> ctx

        else

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
                    elif target.IsAssignableFrom source then 
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
        | Some cdc -> { To = cdc.To ctx; From = cdc.From ctx; Default = cdc.Default ctx }
        | None -> failwithf "No codec found for type %O" ty

    member this.GetCodec<'T>() : CachedCodec<'T> =
        let ty = typeof<'T>

        lock (cache) (fun () -> 

        if cache.ContainsKey ty then 
            unbox cache.[ty]
        else
            let mutable codec = Unchecked.defaultof<_>
            let delay : CachedCodec<'T> = 
                { 
                    To = fun x -> codec.To x
                    From = fun x json -> codec.From x json
                    Default = fun () -> codec.Default()
                }
            cache.Add(ty, unbox delay)
            codec <- this.GenCodec<'T>()
            ignore(cache.Remove ty); cache.Add(ty, unbox codec)
            codec )

    member this.GetBoxedCodec<'T>() : CachedCodec<obj> =
        let cdc = this.GetCodec<'T>()
        {
            To = unbox<'T> >> cdc.To
            From = fun x json -> cdc.From (unbox<'T> x) json |> box
            Default = cdc.Default >> box
        }

    member this.ToJson (obj: 'T) = this.GetCodec<'T>().To obj
    
    member this.ToString (obj: 'T) = obj |> this.ToJson |> Formatting.formatJsonString settings
    
    member this.ToStream (stream: Stream) (obj: 'T) =
        use w = new StreamWriter(stream)
        obj |> this.ToJson |> Formatting.formatJsonToStream settings stream
    
    member this.ToFile (path, overwrite) (obj: 'T) =
        if overwrite || not(File.Exists path) then
            use stream = File.Open(path, FileMode.Create)
            this.ToStream stream obj
        else raise <| IOException "This file already exists!"
    
    member this.FromJson<'T> (json: JSON) : JsonResult<'T> = 
        let cdc = this.GetCodec<'T>()
        try 
            Ok(cdc.FromDefault json)
        with err ->
            Error (exn("Error decoding json", err))
            
    member this.FromString<'T> (str: string) : JsonResult<'T> =
        match Parsing.parseString str with
        | ParserResult.Success (res, _, _) -> Ok(res)
        | ParserResult.Failure (_, err, _) -> Error (exn("Error parsing json", ParseFailure err))
        |> Result.bind this.FromJson<'T>
    
    member this.FromStream<'T> (nameOfStream, stream) =
        match Parsing.parseStream nameOfStream stream with
        | ParserResult.Success (res, _, _) -> Ok(res)
        | ParserResult.Failure (_, err, _) -> Error (exn("Error parsing json", ParseFailure err))
        |> Result.bind this.FromJson<'T>
    
    member this.FromFile<'T> (path: string) =
        try Ok(Parsing.parseFile path)
        with :? FileNotFoundException as err -> Error (exn("File not found", err))
        |> Result.bind (function
            | ParserResult.Success (res, _, _) -> Ok res
            | ParserResult.Failure (_, err, _) -> Error (exn("Error parsing json", ParseFailure err)))
        |> Result.bind this.FromJson<'T>
    
    member this.Default<'T>() : 'T = this.GetCodec<'T>().Default()