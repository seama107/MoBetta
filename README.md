# MoBetta Language

A toy programming language implemented in Haskell

## Getting Started

This project requires the [haskell engine](https://www.haskell.org/).
Find more info about this project [here](https://mandrewmoshier.github.io/CPSC354/).

### Running files

The MoBetta interpretter runs files with the '.mb' extension. Run code by
```
runhaskell MoBetta <YOUR_FILE>.mb
```

### File Listing
* MoBetta.hs - contains the main method of the interpreter, links other files.
* MoBettaAST.hs - The abstract syntax tree of the programming language. Used by the parser
* MoBettaEngine.hs - Translates and executes program trees built by the parser.
* MoBettaParser.hs - Translates source code files into program trees to be executed by MoBettaEngine
* testFiles - Some sample files included with the assignment.

## Authors

* **Michael Seaman** - *Initial work* - [seama107](https://github.com/seama107)
* **Dr. Andrew Moshier** - *Skeleton code* - [github pages](https://mandrewmoshier.github.io/CPSC354/)
