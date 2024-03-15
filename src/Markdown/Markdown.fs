// --------------------------------------------------------------------------------------
// F# Markdown (Main.fs)
// (c) Tomas Petricek, 2012, Available under Apache 2.0 license.
// --------------------------------------------------------------------------------------

namespace Percyqaz.Data.Markdown

open System
open System.IO
open System.Collections.Generic

open Percyqaz.Data.Markdown.Parser

/// <summary>
/// Representation of a Markdown document - the representation of Paragraphs
/// uses an F# discriminated union type and so is best used from F#.
/// </summary>
/// <namespacedoc>
///   <summary>Functionality for processing markdown documents, converting to HTML, LaTeX, ipynb and scripts</summary>
/// </namespacedoc>
type MarkdownDocument(paragraphs, links) =
    /// Returns a list of paragraphs in the document
    member x.Paragraphs: MarkdownParagraphs = paragraphs

    /// Returns a dictionary containing explicitly defined links
    member x.DefinedLinks: IDictionary<string, string * string option> = links

/// Static class that provides methods for formatting
/// and transforming Markdown documents.
type Markdown() =
    /// Parse the specified text into a MarkdownDocument. Line breaks in the
    /// inline HTML (etc.) will be stored using the specified string.
    static member Parse(text, ?newline, ?parseOptions) =
        let newline = defaultArg newline Environment.NewLine

        let parseOptions = defaultArg parseOptions MarkdownParseOptions.None

        use reader = new StringReader(text)

        let lines =
            [
                let line = ref ""
                let mutable lineNo = 1

                while (line := reader.ReadLine()
                       line.Value <> null) do
                    yield
                        (line.Value,
                         {
                             StartLine = lineNo
                             StartColumn = 0
                             EndLine = lineNo
                             EndColumn = line.Value.Length
                         })

                    lineNo <- lineNo + 1

                if text.EndsWith(newline, StringComparison.Ordinal) then
                    yield
                        ("",
                         {
                             StartLine = lineNo
                             StartColumn = 0
                             EndLine = lineNo
                             EndColumn = 0
                         })
            ]
        //|> Utils.replaceTabs 4
        let links = Dictionary<_, _>()
        //let (Lines.TrimBlank lines) = lines
        let ctx: ParsingContext =
            {
                Newline = newline
                IsFirst = true
                Links = links
                CurrentRange = Some(MarkdownRange.zero)
                ParseOptions = parseOptions
            }

        let paragraphs =
            lines
            |> List.skipWhile (fun (s, _n) -> String.IsNullOrWhiteSpace s)
            |> parseParagraphs ctx
            |> List.ofSeq

        MarkdownDocument(paragraphs, links)
