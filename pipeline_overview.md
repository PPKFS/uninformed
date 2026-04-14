This is a high level overview of how the compilation flow goes.
It will also highlight some/most of places that functionality is missing compared to the original i7 compiler.

On the plus side, the pipeline stages (defined in `Uninformed.Pipeline`) do break everything into discrete segments with inputs and outputs.

# Lexing

Takes a source file input (of some sort) and breaks it into a list of tokens (words) and a `VocabMap` - basically a symbol table for every unique word in the file. There's some amount of whitespace handling and tracking the original file the tokens came from.

Currently missing: the Preform handling in the `words` module.

# Sentence Breaking

Takes the stream of lexemes and produces a very narrow syntax tree of sentences. The tests currently do not check for the arrangement of this under headers, just that they do exist.

Currently missing: assigning headers a level (the 'dividing sentence' preform), a test that the headers are correctly aligned in the tree.
