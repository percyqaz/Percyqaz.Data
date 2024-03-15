// --------------------------------------------------------------------------------------
// F# Markdown (Markdown.fs)
// (c) Tomas Petricek, 2012, Available under Apache 2.0 license.
// --------------------------------------------------------------------------------------

namespace rec Percyqaz.Data.Markdown

[<Struct>]
type MarkdownRange =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

module MarkdownRange =

    let zero =
        {
            StartLine = 0
            StartColumn = 0
            EndLine = 0
            EndColumn = 0
        }

    let mergeRanges (ranges: MarkdownRange list) =
        let startRange = ranges |> List.minBy (fun r -> r.StartLine, r.StartColumn)

        let endRange = ranges |> List.maxBy (fun r -> r.EndLine, r.EndColumn)

        {
            StartLine = startRange.StartLine
            StartColumn = startRange.StartColumn
            EndLine = endRange.EndLine
            EndColumn = endRange.EndColumn
        }

// --------------------------------------------------------------------------------------
// Definition of the Markdown format
// --------------------------------------------------------------------------------------

/// <summary>
///   A list kind can be Ordered or Unordered corresponding to <c>&lt;ol&gt;</c> and <c>&lt;ul&gt;</c> elements
/// </summary>
type MarkdownListKind =
    | Ordered
    | Unordered

/// Column in a table can be aligned to left, right, center or using the default alignment
type MarkdownColumnAlignment =
    | AlignLeft
    | AlignRight
    | AlignCenter
    | AlignDefault

/// Represents inline formatting inside a paragraph. This can be literal (with text), various
/// formattings (string, emphasis, etc.), hyperlinks, images, inline maths etc.
type MarkdownSpan =
    | Literal of text: string * range: MarkdownRange option
    | InlineCode of code: string * range: MarkdownRange option
    | Strong of body: MarkdownSpans * range: MarkdownRange option
    | Emphasis of body: MarkdownSpans * range: MarkdownRange option
    | AnchorLink of link: string * range: MarkdownRange option
    | DirectLink of body: MarkdownSpans * link: string * title: string option * range: MarkdownRange option
    | IndirectLink of body: MarkdownSpans * original: string * key: string * range: MarkdownRange option
    | DirectImage of body: string * link: string * title: string option * range: MarkdownRange option
    | IndirectImage of body: string * link: string * key: string * range: MarkdownRange option
    | HardLineBreak of range: MarkdownRange option
    | LatexInlineMath of code: string * range: MarkdownRange option
    | LatexDisplayMath of code: string * range: MarkdownRange option
    | EmbedSpans of customSpans: MarkdownEmbedSpans * range: MarkdownRange option

/// A type alias for a list of MarkdownSpan values
type MarkdownSpans = MarkdownSpan list

/// Provides an extensibility point for adding custom kinds of spans into a document
/// (MarkdownEmbedSpans values can be embedded using MarkdownSpan.EmbedSpans)
type MarkdownEmbedSpans =
    abstract Render: unit -> MarkdownSpans

/// A paragraph represents a (possibly) multi-line element of a Markdown document.
/// Paragraphs are headings, inline paragraphs, code blocks, lists, quotations, tables and
/// also embedded LaTeX blocks.
type MarkdownParagraph =
    | Heading of size: int * body: MarkdownSpans * range: MarkdownRange option
    | Paragraph of body: MarkdownSpans * range: MarkdownRange option

    /// A code block, whether fenced or via indentation
    | CodeBlock of
        code: string *
        executionCount: int option *
        fence: string option *
        language: string *
        ignoredLine: string *
        range: MarkdownRange option

    /// A HTML block
    | InlineHtmlBlock of code: string * executionCount: int option * range: MarkdownRange option

    /// A Markdown List block
    | ListBlock of kind: MarkdownListKind * items: MarkdownParagraphs list * range: MarkdownRange option

    /// A Markdown Quote block
    | QuotedBlock of paragraphs: MarkdownParagraphs * range: MarkdownRange option

    /// A Markdown Span block
    | Span of body: MarkdownSpans * range: MarkdownRange option

    /// A Markdown Latex block
    | LatexBlock of env: string * body: string list * range: MarkdownRange option

    /// A Markdown Horizontal rule
    | HorizontalRule of character: char * range: MarkdownRange option

    /// A Markdown Table
    | TableBlock of
        headers: MarkdownTableRow option *
        alignments: MarkdownColumnAlignment list *
        rows: MarkdownTableRow list *
        range: MarkdownRange option

    /// Represents a block of markdown produced when parsing of code or tables or quoted blocks is suppressed
    | OtherBlock of lines: (string * MarkdownRange) list * range: MarkdownRange option

    /// A special addition for computing paragraphs
    | EmbedParagraphs of customParagraphs: MarkdownEmbedParagraphs * range: MarkdownRange option

    /// A special addition for YAML-style frontmatter
    | YamlFrontmatter of yaml: string list * range: MarkdownRange option

    /// A special addition for inserted outputs
    | OutputBlock of output: string * kind: string * executionCount: int option

/// A type alias for a list of paragraphs
type MarkdownParagraphs = MarkdownParagraph list

/// A type alias representing table row as a list of paragraphs
type MarkdownTableRow = MarkdownParagraphs list

/// Provides an extensibility point for adding custom kinds of paragraphs into a document
/// (MarkdownEmbedParagraphs values can be embedded using MarkdownParagraph.EmbedParagraphs)
type MarkdownEmbedParagraphs =
    abstract Render: unit -> MarkdownParagraphs

/// Controls the parsing of markdown
type MarkdownParseOptions =
    | None = 0
    | ParseCodeAsOther = 1
    | ParseNonCodeAsOther = 2
    | AllowYamlFrontMatter = 4
