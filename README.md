# BURGer
A general purpose programming language.

## Compiling
Use the following instructions to compile BURGer and run .bun files.

1. ```make``` in the src directory to produce ```burger.native```
2. ```./burgr.sh <filename>``` to compile .bun file, produce executable, and run the executable. Do not include ".bun" in the filename.


## Testing with Menhir

```menhir --interpret --interpret-show-cst parser.mly``` will let you type in tokens to see if the parser accepts a certain string.

## Testing parser grammar

To test the grammar specified in our ```parser.mly```, run the following:

```ocamlyacc -v parser.mly```

This will produce ```parser.output```, which you can ```cat``` to look at the LR(0) states and transitions.

## Documentation
Our Final Report and Slides are located in the ```doc``` folder.
