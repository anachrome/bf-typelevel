# type-level brainfuck interpretter

A brainfuck interpretter that runs in `ghc`'s typechecker.  I wrote the original
program in a night or two about a year ago, but have since done a _lot_ of
polishing and rewriting.  (Including more or less completely rewriting the core
eval function).

## using

You can look at the example Main.hs file for an example of how to use the
interpretter to embed type-level brainfuck in your own haskell code.  The gist
is that you use create empty tape, input, and output types, a program type, and
then use the `EvalBF` type function to get a filled output type.  Reify is a bit
of wankery that turns the types into haskell values that you can use in your
program.

For the first part - generating the types - I've written a few template haskell
functions to make things easier.  For the most part they should be pretty self-
explanatory, if you look at the `Main.hs` file.

If you want to know what the types really look like, `BF/Types.hs` and
`BF/Core.hs` is where you want to look.  Or you could just fire up `ghci` and
ask it.

If you do use the modules in your own code, then `-XTemplateHaskell` is of
course optional if you really want to deal with the types manually, but
`-XDataKinds` and `-XPolyKinds` are required.

### bf.sh

I wrote a little script that automates the process of writing a haskell file
and running ghc on it, you can use it to interpret brainfuck programs with
a given input string:

```
./bf.sh cat.bf "hello, world\n\0"
```

The input string are the numbers (ascii values of the characters) that get seen
on every occurance of a `,` brainfuck command.  They need to be supplied statically
because, unfortunately, haskell's typechecker can't do arbitrary I/O.

Also notice the terminating '\0' character: the way I've written `cat.bf`, it
ends when it encounters a 0.  If try running it like this:

```
./bf.sh cat.bf "hello, world\n"
```

it will continue to ask for input after all of the input string has been consumed,
resulting in a type error.

If you've actually run the first command by now, you may have gotten an error
like this:

```
Makemain.hs:20:27:
    Context reduction stack overflow; size = 101
    Use -fcontext-stack=N to increase stack size to N
      Reify ('Succ ('Succ ('Succ ('Succ 'Zero)))) Int
    In the second argument of ‘(.)’, namely ‘reify’
    In the second argument of ‘(.)’, namely ‘map chr . reify’
    In the expression: putStr . map chr . reify
```

You can increase the context stack directly from the script by setting an
environment variable:

```
CONTEXTSTACK=200 ./bf.sh cat.bf "hello, world\n\0"
```

It's worth noting that, in addition to trying to read after the end of input,
running off either end of the tape is a type error as well.  The tape size
can be increased with the `TAPELENGTH` environment variable.

The input and output lists are both type-level lists; unfortunatly, ghc does not
automatically promote the `Int` or `Integer` types, so I had to write my own
`Int'` type.  The tape is just a ziplist, also of the `Int'` type.  The program
type uses the brainfuck AST defined at the top of `BF/Core.hs`.

Any imaginable runtime errors (essentially the two I talked about above) translate
into kind of horrifying type errors.  So ghc destroys your scrollback, it's probably
worth double-checking that your brainfuck isn't broken.

## code

### Main.hs

An example of how to use the modules.  Fairly straightforward.

### BF/Types.hs

This contains the data structures (which thanks to `-XDataKinds` get promoted
to the type level) used, and some basic type functions.  Nothing too interesting
happens here.

### BF/Core.hs

The `Eval` typefunction and friends.  If you've ever written a brainfuck
interpretter and have some understanding of type families, it should be pretty
obvious what's going on here.  `Eval` and `EvalElem` are the essential functions,
and `EvalBF`, `RunBF`, and `ExecBF` are the user interface, roughly analogous
to their similarly-named state monad friends.

### BF/Reify.hs

The function of note that this module exports is `reify`, which is a polymorphic
function in the identically named typeclass.  All of it's instances turn the
types mentioned in `BF/Types.hs` into terms, which you can then do with what
you like.

### BF/Typify.hs

Template Haskell wankery happens here.  The functions here basically do the
opposite of those in `BF/Reify.hs`, generating types from terms.  It's entirely
optional if you're okay with dealing with a bit of tedium.
