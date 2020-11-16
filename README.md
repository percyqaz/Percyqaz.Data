# What's this?

(Under construction)

Yet another JSON library, specificially for my F# JSON needs which were not met by JSON.Net or Chiron

## How it works
- JSON is parsed with FParsec into an abstract syntax tree
- Mappings are created and cached for each type to transform back and forth between JSON trees and data
- Uses TypeShape to abstract away from reflection mess and do things with proper type safety

## Particular features
- Fail-safe: The mappings can fail midway and still recover because parsing is already done
- Version resistant: The mappings can handle variations in JSON representations/changes to records without much fuss
- Null-safe for F# types: Record default values must be provided

more details will follow as this gets finished

## Todo list (that I may never get through):
- Finish functionality
- Quick use guide
- Tests and benchmarks
