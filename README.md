# Haskell

Fooling around with Haskell

### In monads directory:

Several versions of a Reverse Polish Notation calculator program.

To run any of these rpn programs, load it into ghci and run the
testAll function.

rpn.lhs - reverse polish notation calculator - non-monadic

rpnm.lhs - reverse polish notation calculator - uses State monad,
but only for stacks

rpnma.lhs - reverse polish notation calculator - uses State monad
throughout

### In monads/rpn directory:

rpn.lhs - reverse polish notation calculator - uses State monad
    throughout, imports Stack type from a separate file.

Stack.lhs - The stack for rpn.lhs.
