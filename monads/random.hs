-- random.hs
-- from Ertegrul's monad tutorial

{-
 - lcg and getRandom came from Ertegrul's tutorial.  I somehow
 - create randomList and randomList' myself.
 -}

-- \s -> (a, s)
-- \a -> \s -> (a, s)

import Control.Monad.State
import Data.Word

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
                           in put s1 >> return x   -- put returns ((), s1), return returns (x, s1)

-- producess infinite list of random values using do notation
randomList :: State LCGState [Integer]
randomList = do x  <- getRandom
                xs <- randomList
                return (x:xs)

-- producess infinite list of random values using bind
randomList' :: State LCGState [Integer]
randomList' = getRandom >>= \x -> randomList' >>= \xs -> return (x:xs)

test01 = take 20 (evalState randomList  0)
test02 = take 20 (evalState randomList' 0)
test03 = take 20 (evalState randomList  100)
test04 = take 20 (evalState randomList' 100)
