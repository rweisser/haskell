-- random.hs
-- from Ertegrul's monad tutorial

{-
 - lcg and getRandom came from Ertegrul's tutorial.  I somehow
 - managed to create randomList and randomList' myself.
 -}

import Control.Monad.State
import Data.Word

-- Some types for reference:
--
-- get         :: s -> (a, s)
-- put         :: a -> s -> (a, s)
-- runState    :: (s -> (a, s)) -> s -> (a, s)
-- evalState   :: (s -> (a, s)) -> s -> a
-- execState   :: (s -> (a, s)) -> s -> s

type LCGState = Word32

-- Linear Congruential Generator
lcg :: LCGState -> (Integer, LCGState)
lcg s0 = (output, s1)
  where
    s1     = 1103515245 * s0 + 12345
    output = fromIntegral s1 * 2^16 `div` 2^32

-- produces next random vaue
getRandom :: State LCGState Integer
getRandom = get >>= \s0 -> let (x, s1) = lcg s0
                           in put s1 >> return x

-- produces an infinite list of random values using do notation
randomList :: State LCGState [Integer]
randomList = do x  <- getRandom
                xs <- randomList
                return (x:xs)

-- produces an infinite list of random values using bind
randomList' :: State LCGState [Integer]
randomList' = getRandom >>= \x -> randomList' >>= \xs -> return (x:xs)

test01 = take 20 (evalState randomList  0)
test02 = take 20 (evalState randomList' 0)
test03 = take 20 (evalState randomList  100)
test04 = take 20 (evalState randomList' 100)

test_all = do print test01
              print test02
              print test03
              print test04
