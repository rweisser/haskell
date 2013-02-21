rpn.lhs

A reverse polish notation evaluator.  Non-Monadic.

The implementation uses stacks of Items.  The items in the stack
are numeric values or operators.  There is an expression stack which
starts off with the initial expression to be calculated.  There is
a separate work stack to hold values as they are being processed.
At the end, if everything goes well, the work stack will contain a
single value (the result) and the expression stack will be empty.

    Example:  1 3 + 5 * 6 3 / *

    Evaluation:  1 3 + 5 * 6 3 / *
                 -----
                 4     5 * 6 3 / *
                 ---------
                 20        6 3 / *
                           -----
                 20        2     *
                 -----------------
                 40

The stacks contain numeric values and operators.

> type Stack = [Item]

> data Item = Val { getVal :: Float }
>           | Op  Oper deriving (Eq, Show)

> data Oper = Add | Sub | Mult | Div deriving (Eq, Show)

An example rpn stack:

> example1 :: Stack
> example1 = [Val 1.0,
>             Val 3.0,
>             Op Add,
>             Val 5.0,
>             Op Mult,
>             Val 4.0,
>             Val 3.0,
>             Op Div,
>             Op Mult]

pop removes the item at the top of the stack.

> pop :: Stack -> (Item, Stack)
> pop stack = if empty stack
>               then error "pop used on empty stack"
>               else (head stack, tail stack)

push adds a new item to the stack.  push returns a new stack.  A
monadic version might have the type Item -> Stack -> ((), Stack)

> push :: Item -> Stack -> Stack
> push item stack = item:stack

empty checks if the stack is empty:

> empty :: Stack -> Bool
> empty = null

The evaluator pops items from the espression stack until it encounters
an operator.  It stores the popped items in the work stack in
reversed order.  It then evaluates the top two items in the work
stack using the operator and pushes the result back on the work
stack.

Using our example in the first set of comments above, the evaluation
will go like this:

    expression               work
    stack                op  stack
    -------------------  --  ----------
    [1 3 + 5 * 6 3 / *]      []
    [3 + 5 * 6 3 / *]        [1]
    [+ 5 * 6 3 / *]          [3 1]
    [5 * 6 3 / *]        +   [3 1]
    [5 * 6 3 / *]            [4]
    [* 6 3 / *]              [5 4]
    [6 3 / *]            *   [5 4]
    [6 3 / *]                [20]
    [3 / *]                  [6 20]
    [/ *]                    [3 6 20]
    [*]                  /   [3 6 20]
    [*]                      [2 20]
    []                   *   [2 20]
    []                       [40]

The process is complete when there is one number in the work stack
and nothing in the expression stack.

There are probably better ways to do this, but this looks like it
will work.

calc is the main function.  It just passes an empty work stack and
the expression stack to eval.  The work stack w returned by eval
should contain one value.

> calc :: Stack -> Float
> calc expr = let (e, w) = eval (expr, [])
>             in  getVal (head w)

eval evaluates an expression.  The function starts with an empty
work stack and a full expression stack and terminates when there
is one value in the work stack and the expression stack is empty.

> eval :: (Stack, Stack) -> (Stack, Stack)
> eval ([], [x]) = ([], [x])
> eval (e,  w)   = eval $ step e w

stop processes the top of the expression stack.  Note that step is
never called when the expression stack is empty.  If the item at
the top of the expression stack is an operator, call binOp to apply
the operator to the top 2 items on the work stack.  Then return the
popped expression stack and the possibly modified work stack.  If
the item at the top of the expression stack is a value, just push
it onto the work stack and return both stacks.

> step :: Stack -> Stack -> (Stack, Stack)
> step e w =
>   let (item, e') = pop e
>   in  case item of
>         (Val n) -> (e', push item w)
>         _       -> let w' = binOp item w
>                    in (e', w')

binOp does one binary operation.  Pop off the two operands.  Apply
the operator.  Push the result back on the stack and return the
stack.  The stack is always the work stack.  binOp is written as
if Stack were an abstract data type.  Otherwise, I would have used
pattern matching, as seen below.  Note that the second operand is
at the top of the stack, followed by the first operand.  This version
of binOp should be compared with the monadic version in rpnm.lhs.

> binOp :: Item -> Stack -> Stack
> binOp op s = let (item2, s1) = pop s
>                  (item1, s2) = pop s1
>                  val2  = getVal item2
>                  val1  = getVal item1
>                  res = case op of
>                    Op Add  -> val1 + val2
>                    Op Sub  -> val1 - val2
>                    Op Mult -> val1 * val2
>                    Op Div  -> val1 / val2
>              in push (Val res) s2

Here is a version of binOp using pattern matching instead of pop.
I didn't use it.  The version above makes a better comparision with
the one in rpnm.lhs.

    binOp :: Item -> Stack -> Stack
    binOp op (val2:val1:items) = let v1  = getVal val1
                                     v2  = getVal val2
                                     res = case op of
                                             Op Add  -> v1 + v2
                                             Op Sub  -> v1 - v2
                                             Op Mult -> v1 * v2
                                             Op Div  -> v1 / v2
                                 in push (Val res) items

testEq a test function.  It prints a comment and then compares two
expressions for equality, printing "passed" or "failed," respectively..

> testEq e r m = do putStr m
>                   putStr ": "
>                   let res = case e == r of
>                               True  -> "passed"
>                               False -> "failed"
>                   putStrLn res

Tests:

> test01 = testEq (binOp (Op Sub) [Val 3.0, Val 1.0, Val 10.0])
>                 [Val (-2.0), Val 10.0]
>                 "binOp test"

> test02 = testEq (binOp (Op Div) [Val 3.0, Val 1.0, Val 10.0])
>                 [Val 0.33333334, Val 10.0]
>                 "binOp test"

> test03 = testEq (step [Val 1.0, Val 3.0, Op Div, Val 10.0, Op Add] [])
>                 ([Val 3.0, Op Div, Val 10.0, Op Add], [Val 1.0])
>                 "step test"

> test04 = testEq (step [Val 3.0, Op Div, Val 10.0, Op Add] [Val 1.0])
>                 ([Op Div,Val 10.0,Op Add],[Val 3.0,Val 1.0]) 
>                 "step test"

> test05 = testEq (step [Op Div, Val 10.0, Op Add] [Val 3.0, Val 1.0])
>                 ([Val 10.0,Op Add],[Val 0.33333334])
>                 "step test"

> test06 = testEq (step [Val 10.0, Op Add] [Val 0.33333334])
>                 ([Op Add],[Val 10.0,Val 0.33333334])
>                 "step test"

> test07 = testEq (step [Op Add] [Val 10.00, Val 0.33333334])
>                 ([],[Val 10.333333])
>                 "step test"

> test08 = testEq (eval (example1, []))
>                 ([],[Val 26.666668])
>                 "eval test"

> test09 = testEq (calc example1)
>                 26.666668
>                 "calc test"

All the tests:

> testAll = do test01
>              test02
>              test03
>              test04
>              test05
>              test06
>              test07
>              test08
>              test09
>              testFail

Test a malformed rpn expression.
The result will be *** Exception: pop used on empty stack.

> testFail = do putStr "failure test: "
>               putStrLn $ show $ calc (Val 1.0 : example1)
