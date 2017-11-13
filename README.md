# BURGer
A programming language for creating text-based games.

BURGer is currently being build. Stay tuned!

## Compiling
Use the following instructions to compile BURGer and run .bun files.

1. ```make``` in the src directory to produce ```burger.native```
2. ```./burger.native < test.bun > test.ll```, replacing ```test``` with your desired filename.
3. ```clang test.ll``` produces the executable ```a.out```.


## Testing with Menhir

```menhir --interpret --interpret-show-cst parser.mly``` will let you type in tokens to see if the parser accepts a certain string.

## Testing parser grammar

To test the grammar specified in our ```parser.mly```, run the following:

```ocamlyacc -v parser.mly```

This will produce ```parser.output```, which you can ```cat``` to look at the LR(0) states and transitions.
