rpn/rpn.lhs

A reverse polish notation evaluator.  All monadic.

The implementation uses stacks of Items.  The items in the stack
are numeric values or operators.  There is an expression stack which
starts off with the initial expression to be calculated.  There is
a separate work stack to hold values as they are being processed.
At the end, if everything goes well, the work stack will contain a
single value (the result) and the expression stack will be empty.

There are two uses of the State monad in this program:

    The stack operations are monadic:  pushM, popM, and emptyM

    The overall state is an algebraic data type containing the
    expression stack and the work stack.

This is how an rpn evaluation works:

    Expression:  1 3 + 5 * 6 3 / *

    Evaluation:  1 3 + 5 * 6 3 / *
                 -----
                 4     5 * 6 3 / *
                 ---------
                 20        6 3 / *
                           -----
                 20        2     *
                 -----------------
                 40

The definitions of the State monad and related classes and functions
are in Control.Monad.State.

Reminders:

    runState  :: State s a -> s -> (a, s)
    evalState :: State s a -> s -> a
    execState :: State s a -> s -> s

    get       :: State s s
    get       =  state $ \s -> (s, s)

    put       :: s -> State s ()
    put s     =  state $ \_ -> ((), s)

> import Control.Monad.State
> import Stack(Stack, fromList, push, pop, size, empty)

The stacks contain numeric values and operators.

> data Item = Val { getVal :: Float }
>           | Op  Oper deriving (Eq, Show)

> data Oper = Add | Sub | Mult | Div deriving (Eq, Show)

An example rpn expression

> example1 :: [Item]
> example1 = [Val 1.0,
>             Val 3.0,
>             Op Add,
>             Val 5.0,
>             Op Mult,
>             Val 4.0,
>             Val 3.0,
>             Op Div,
>             Op Mult]

The monadic versions of pop, push and empty are wrappers around the
non-monadic versions from the Stack Abstract Data Type.

popM is the monadic version of pop.

> popM :: State (Stack a) a
> popM = do empty <- emptyM
>           if empty
>             then error "popM used on empty stack"
>             else state pop

pushM is the monadic version of push.

> pushM :: a -> State (Stack a) ()
> pushM item = do stack <- get
>                 put $ push item stack

emptyM is the monadic version of empty.

> emptyM :: State (Stack a) Bool
> emptyM = do stack <- get
>             return $ empty stack

The state for the eval and step functions is

    RpnState (Stack Item) (Stack Item)
    
The first stack is the expression stack.  It contains the part of
the expression which has not yet been used.  The second stack is
the work stack.  Values are pushed onto the work stack until an Op
is encountered.  When an Op is encountered, the Op and the work
stack are passed to binOp for evaluation.

> data RpnState = RpnState {exprStack :: Stack Item,
>                           workStack :: Stack Item}
>                 deriving (Eq, Show)

stacks is a convenience function for getting both stacks out of an
RpnState:

> stacks :: RpnState -> (Stack Item, Stack Item)
> stacks s = (exprStack s, workStack s)

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

calc is the main function.  It just passes an empty work stack and
the expression stack to eval.  The work stack returned by execState
eval should contain one value.

> calc :: [Item] -> Float
> calc expr = getVal
>           $ evalState popM
>           $ workStack
>           $ execState eval
>           $ RpnState (fromList expr) (fromList [])

eval evaluates an expression.  The function starts with a full
expression stack and an empty work stack and terminates when the
expression stack is empty and the work stack contains one value.

> eval :: State RpnState ()
> eval = do s <- get
>           let (e, w) = stacks s
>           if empty e && size w == 1
>             then put $ RpnState e w
>             else do step
>                     eval

step processes the top of the expression stack.  Note that step is
never called when the expression stack is empty.  If the item at
the top of the expression stack is an operator, step calls binOp
to apply the operator to the top 2 items on the work stack.  Then
it returns the popped expression stack and the modified work stack.
If the item at the top of the expression stack is a value, step
just pushes it onto the work stack and returns both stacks.

> step :: State RpnState ()
> step = do
>   s <- get
>   let (e, w)     = stacks s
>       (item, e') = runState popM e
>       w'         = case item of
>                      (Val n) -> execState (pushM item) w
>                      _       -> execState (binOp item) w
>   put $ RpnState e' w'

binOp performs one binary operation.  Pop off the two operands.
Apply the operator.  Push the result back on the stack and return
the stack.  The stack is always the work stack.

> binOp :: Item -> State (Stack Item) ()
> binOp op = do (Val v2) <- popM
>               (Val v1) <- popM
>               let res = case op of
>                     Op Add  -> v1 + v2
>                     Op Sub  -> v1 - v2
>                     Op Mult -> v1 * v2
>                     Op Div  -> v1 / v2
>               pushM (Val res)

testEq is a test function.  It prints a comment and then compares
two expressions for equality, printing "passed" or "failed,"
respectively.

> testEq e r m = do putStr m
>                   putStr ": "
>                   let res = case e == r of
>                               True  -> "passed"
>                               False -> "failed"
>                   putStrLn res

Tests:

> test01 = let work = fromList [Val 3.0, Val 1.0, Val 10.0]
>              res  = fromList [Val (-2.0), Val 10.0]
>          in  testEq (execState (binOp (Op Sub)) work)
>                     res
>                     "binOp test"

> test02 = let work = fromList [Val 3.0, Val 1.0, Val 10.0]
>              res  = fromList [Val 0.33333334, Val 10.0]
>          in  testEq (execState (binOp (Op Div)) work)
>                     res
>                     "binOp test"

> test03 = let expr1 = fromList [Val 1.0, Val 3.0, Op Div, Val 10.0, Op Add]
>              work1 = fromList []
>              expr2 = fromList [Val 3.0, Op Div, Val 10.0, Op Add]
>              work2 = fromList [Val 1.0]
>          in  testEq (execState step $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "step test"

> test04 = let expr1 = fromList [Val 3.0, Op Div, Val 10.0, Op Add]
>              work1 = fromList [Val 1.0]
>              expr2 = fromList [Op Div,Val 10.0,Op Add]
>              work2 = fromList [Val 3.0,Val 1.0]
>          in  testEq (execState step $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "step test"

> test05 = let expr1 = fromList [Op Div, Val 10.0, Op Add]
>              work1 = fromList [Val 3.0, Val 1.0]
>              expr2 = fromList [Val 10.0,Op Add]
>              work2 = fromList [Val 0.33333334]
>          in  testEq (execState step $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "step test"

> test06 = let expr1 = fromList [Val 10.0, Op Add]
>              work1 = fromList [Val 0.33333334]
>              expr2 = fromList [Op Add]
>              work2 = fromList [Val 10.0,Val 0.33333334]
>          in  testEq (execState step $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "step test"

> test07 = let expr1 = fromList [Op Add]
>              work1 = fromList [Val 10.00, Val 0.33333334]
>              expr2 = fromList []
>              work2 = fromList [Val 10.333333]
>          in  testEq (execState step $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "step test"

> test08 = let expr1 = fromList example1
>              work1 = fromList []
>              expr2 = fromList []
>              work2 = fromList [Val 26.666668]
>          in  testEq (execState eval $ RpnState expr1 work1)
>                     (RpnState expr2 work2)
>                     "eval test"

> test09 = testEq (calc example1) 26.666668 "calc test"

Test a malformed rpn expression.
The result will be *** Exception: popM used on empty stack.

> testFail = do putStr "failure test: "
>               putStrLn $ show $ calc $ Val 1.0 : example1

All the tests.  The last one causes an exception:

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
