# What's this?

Yet another JSON library, specificially for my F# JSON needs which were not met by JSON.Net or Chiron

## How it works (briefly)
**JSON text**

↕ parsed (using FParsec)

**JSON data structure (as a DU)**

↕ mapped (using codecs)

**Your F# types**

## Key design principles

- Don't care for how the outputted JSON text is formatted (as long as it is quite readable)
- Null-safety - You can't deserialise JSON to get null for non-nullable F# objects etc
- Support for all simple F# types out-of-box
- Exceptions are thrown internally instead of railroading because the code is simpler
- Rules for mapping back and forth are bundled together for easy combinator use


