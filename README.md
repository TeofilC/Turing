# Turing

A simple webapp, that expands Turing's abbreviated notation for describing the configuration of an a-machine (as laid out in his 1937 paper, *On Computable Numbers, With an Application to the Entscheidungproblem*) into the more explicit notation through which a-machines are formally defined.

It is written in Haskell, which is then compiled with GHCJS to JS

## Differences in notation from Turing's '37 Paper
Turing uses a host of typography to distinguish between the different syntactic elements of his notation (gothic letters for m-configs, etc)
These characters would be very difficult to input with a standard keyboard so I adopt a slightly different approach.

  * A lower case character followed by a sequence of lowercase or numeric characters is interpreted to be a symbol, which can be read or printed onto the tape by the a-machine
  * An upper case character followed by a sequence of uppercase, lowercase or numeric characters is interpreted to be the name of an m-config or an m-function if it is followed by a list of symbols, m-config names, and m-function names, separated by commas and surrounded by brackets, eg, "B", "Pr1(A,a)", "Find(e,Pr1(B,y))"
  * In addition to the terms that can go into the "Read Symbol" column in Turing's abbreviated notation (namely, a symbol, "Any", "None", "Read" followed by a symbol), I also allow "AnyAndNone", which as one might expect reads any symbol or the blank symbol


## Building
The project can be built with stack

```shell
stack build
```

To run it you will additionally need to copy the `index.html` and `style.css` files to the build folder and place a copy of the font-awesome icon library into the `fa/` subdirectory.
Alternatively you can edit the url in `index.html` to point to the location of `font-awesome.css`
