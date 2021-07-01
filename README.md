# What's this?

Yet another JSON library, specificially for my F# JSON needs which were not met by JSON.Net or Chiron

## Key design principles

**JSON text**

↕ parsed (using FParsec)

**JSON data structure (as a DU)**

↕ mapped (using rules/combinators)

**Your F# types**

---

- Don't care for how the outputted JSON text is formatted (as long as it is quite readable)
- Library should operate foremost by hand-made mapping rules for types, with options for auto-generation/caching of rules
- Null-safety - You can't deserialise JSON to get null for non-nullable F# objects etc
- Support for all simple F# types out-of-box
- Exceptions are thrown internally instead of railroading because the code is simpler
- Rules for mapping back and forth are bundled together for easy combinator use

Basically this library is very similar to Chiron/Fleece but super stripped down to just things I need + some handy auto-generating
