rpn/Stack.lhs

A Stack Abstract Data Type

> module Stack (
>     Stack,
>     fromList,    -- [a] -> Stack a
>     push,        -- a -> Stack a -> Stack a
>     pop,         -- Stack a -> (a, Stack a)
>     size,        -- Stack a -> Int
>     empty)       -- Stack a -> Bool
>  where

> newtype Stack a = Stack [a] deriving (Eq, Show)

create a new Stack from a list

> fromList :: [a] -> Stack a
> fromList list = Stack list

push adds a new item to the stack.  push returns a new stack.

> push :: a -> Stack a -> Stack a
> push x (Stack xs) = Stack $ x:xs

pop removes the item at the top of the stack.

> pop :: Stack a -> (a, Stack a)
> pop s = if empty s then error "pop used on empty stack"
>                    else let (Stack list) = s
>                         in  (head list, Stack $ tail list)

size returns the size of the stack:

> size :: Stack a -> Int
> size (Stack s) = length s

empty checks if the stack is empty:

> empty :: Stack a -> Bool
> empty (Stack list) = null list
