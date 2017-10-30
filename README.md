# BURGer
A programming language for creating text-based games

## Testing parser grammar

To test the grammar specified in our ```parser.mly```, run the following:

```ocamlacc -v parser.mly```

This will produce ```parser.output```, which you can ```cat``` to look at the LR(0) states and transitions. 
