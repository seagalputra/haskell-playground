module EvenOrOdd where

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n | mod n 2 == 0 = "Even"
            | otherwise    = "Odd"
