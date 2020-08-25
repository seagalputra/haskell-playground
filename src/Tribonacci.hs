module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _         0 = []
tribonacci (a, b, c) n = [ tribonacci' (a, b, c) x | x <- [0 .. (n - 1)] ]
 where
  tribonacci' (a, b, c) n
    | n == 0
    = a
    | n == 1
    = b
    | n == 2
    = c
    | otherwise
    = tribonacci' (a, b, c) (n - 1)
      + tribonacci' (a, b, c) (n - 2)
      + tribonacci' (a, b, c) (n - 3)
