module Lib
  ( someFunc
  )
where

import           Data.List
import           Text.Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sqr x = x * x

-- functional programming

-- x -> y
-- y = f(x) = x^5 + x^3 + 10

-- data trans. (data & fn)
-- data : types
-- fn : domain & codomain
-- fn : input & output

-- Predicates => pengujian (True/False)

-- identity!!!! -> fn & value

my_pi = 6.28
bilangan_e = 123.23423542

-- f(x) = x^2
-- f y = y^2
-- f z = x^2 + y^2
kuadrat_njing x = x * x

-- (19 * 3) * (19 * 3) =>

square n = n * n

add a b = a + b

-- kecap a b c = ((-b) + (sqrt disk)) / (2 * a)
--   where disk = (b ^ 2) - (4 * a * c)

doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- let lostNumbers = [4,8,15,16,23,42]

-- list comprehensions
-- [ <output value> | <range value>, <predicate> ]
-- example : [x*2 | x <- [1..10], x*2 >= 12]
-- predicate is optional, main function of predicate is to filtering the value
-- example without predicate : [x*2 | x <- [1..10]]

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

data Thing = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

data FailableDouble = Failure
  | OK Double
  deriving Show

exD1 = Failure
exD2 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
-- head' []      = error "Can't call head on an empty list, dummy!"
-- head' (x : _) = x
head' xs = case xs of
  []      -> error "No head for empty list!"
  (x : _) -> x

-- Pattern matching with list
tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) =
  "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) =
  "This list is long. The first two elements are: "
    ++ show x
    ++ " and "
    ++ show y

-- This is how we use pattern matching in list with recursion
length' :: (Num b) => [a] -> b
length' []       = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

-- By using @ in front of pattern,
-- that pattern will exactly same as x:y:ys and you don't need to write like that in the function body.
capital :: String -> String
capital ""           = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
 where
  bmi                   = weight / height ^ 2
  (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b | a > b     = a
         | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b | a > b     = GT
                | a == b    = EQ
                | otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
 where
  (f : _) = firstName
  (l : _) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
-- calcBmis xs = [ bmi w h | (w, h) <- xs ]
--   where bmi weight height = weight / height ^ 2
calcBmis xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 ]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
  []  -> "empty."
  [x] -> "a singleton list."
  xs  -> "a longer list."

-- It can be use without guards or case of pattern
maximum' :: (Ord a) => [a] -> a
maximum' []       = error "maximum of empty list"
maximum' [x     ] = x
maximum' (x : xs) = max x (maximum' xs)

-- Recursive using guards because we're testing boolean patterns
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x | n <= 0    = []
               | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x : xs)   = x : take' (n - 1) xs

-- take' function will be evaluate like this
-- suppose n = 3 and sequence number is [4,3,2,1]
-- n = 3 :
-- it will check at the first pattern and it skipped because n >= 0.
-- next fall into second pattern and it skipped because list is not empty
-- fall through third pattern that's become
-- 4 : take' 2 [3, 2, 1]
--     3 : take' 1 [2, 1]
--         2 : take' 0 [1]
--             []
-- So, the result become [4,3,2]

reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs) | a == x    = True
                 | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [ a | a <- xs, a <= x ]
      biggerSorted  = quicksort [ a | a <- xs, a > x ]
  in  smallerSorted ++ [x] ++ biggerSorted


-- Curried Function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

sumWithTen :: (Num a) => a -> a
sumWithTen = (+) 10
