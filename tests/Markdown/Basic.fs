namespace Percyqaz.Data.Tests.Markdown

open NUnit.Framework
open Percyqaz.Data.Markdown

[<TestFixture>]
type Basic() =

    [<Test>]
    member this.BasicParserTest() =

        let sample = """
# Heading

----

## Subheading

Paragraph 1 
Paragraph 1  

Paragraph 2  
Paragraph 2  

- List items
- List items
- List items
"""

        Markdown.Parse(sample.Trim()).Paragraphs
        |> printfn "%A"