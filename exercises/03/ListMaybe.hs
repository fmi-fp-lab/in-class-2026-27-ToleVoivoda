{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{- HLINT ignore "Use foldr" -}

module ListMaybe where

-- Hoho, that's a lot of hidden functions
import Prelude hiding (all, and, concat, drop, filter, length, map, null, product, replicate, reverse, subtract, sum, take, zip, zipWith, (++))
import Text.XHtml (rev)
-- import Text.Parsec.Token (GenTokenParser(squares))
-- import Distribution.PackageDescription.Check (CheckExplanation(NoTarget))
-- import ADTs (x)

-- TODO:
-- ask about progress/problems with homework (see README, remind about formatter, etc.)
-- remind about pushing work as early as possible to get feedback (demonstrate feedback)
-- remind about new homework (maybe will be released before end of week)

-- TODO:
-- remind about currying
-- remind about ($), (.), types

-- TODO:
-- Lists
-- - ask about linked list in C/C++, explain what they are
-- - show definition, contrast with builtin (again, hoogle workflow)
-- - data List
-- - sumList, replicate
-- - explain naming convention (akin to scheme/lisp in general)
-- - list comprehension (!!!) (generators, `let`s, filters, compare with set builder notation)
-- - cartesianProd with list comprehension?
-- - brief mention of Strings (C mentioned??????) (for performance, Text exists)

-- data List a = EmptyList | Node a (List a)
--     deriving Show

-- data [] a = [] | a : [] a

-- >>> :t EmptyList :: List (List Bool)
-- EmptyList :: List (List Bool) :: List (List Bool)

-- TODO:
sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

replicate :: Int -> a -> [a]
replicate n _ | n < 0 = error "restarted"
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

-- >>> replicate (-1) 5

-- *** Exception: restarted

-- TODO:
-- Maybe
-- - explicit permission for a value to be missing
-- - usually called nullability (Kotlin, Java, C#) (why not js?)
-- - show definition
-- - kind of like pointers, except the compiler makes you always check for nullability
-- - mention how Rust Option works
-- - used for error handling

-- TODO:
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

-- >>> case safeDiv 5 0 of; Nothing -> 5; Just x -> x
-- 5

-- data Maybe a = Just a | Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- >>> Prelude.take 3 [ x | x <- [0..], let y = 1, (not . even) x ]
-- [1,3,5]

-- TODO:
-- try doing some exercises with recursion and some with list comprehension (or both)

-- TASK:
-- Generate all the numbers in the ("mathematical range") [n, m] in a list (inclusive).

-- >>> listFromRange 3 15
-- [3,4,5,6,7,8,9,10,11,12,13,14,15]
-- >>> listFromRange 8 8
-- [8]

listFromRange :: Integer -> Integer -> [Integer]
listFromRange x y 
  | x > y = []
  | otherwise = x : listFromRange (x+1) y


-- TASK:
-- Multiply all the elements of a list

-- >>> product [2,1,9]
-- 18
-- >>> product [-3]
-- -3

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

-- TASK:
-- Implement factorial with prod and listFromRange

-- >>> fact 9
-- 362880
fact :: Integer -> Integer
fact n = product $ listFromRange 1 n  

-- TASK:
-- Return a list of the numbers that divide the given number.

-- >>> divisors 0
-- []
-- >>> divisors 221
-- [1,13,17,221]
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]

divisors :: Integer -> [Integer]
divisors n = [ x | x <- listFromRange 1 n, n `mod` x == 0]

-- TASK:
-- Implement prime number checking using listFromRange and divisors

-- >>> isPrime 1
-- False
-- >>> isPrime 5
-- True
-- >>> isPrime 8
-- False

isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

-- TASK:
-- Get the last element in a list.

-- >>> lastMaybe ['a']
-- Just 'a'
-- >>> lastMaybe [1,2,4]
-- Just 4

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

-- TASK:
-- Calculate the length of a list.

-- >>> length [1,2,8,1,1,1,1,1]
-- 8
-- >>> length [4]
-- 1

length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs

-- TASK:
-- Return the nth element from a list (we count from 0).
-- If n >= length xs, return a Nothing

-- >>> ix 2 [1,42,66]
-- Just 66
-- >>> ix 1 [1,42,69,4,2]
-- Just 42

ix :: Integer -> [a] -> Maybe a
ix _ [] = Nothing
ix 0 (x:_) = Just x
ix n (_:xs) = if n<0 then Nothing else ix (n-1) xs

-- TASK:
-- "Drop" the first n elements of a list.
-- If n > length xs, then you should drop them all.

-- >>> drop 5 $ listFromRange 3 11
-- [8,9,10,11]
-- >>> drop 20 $ listFromRange 1 10
-- []

drop :: Integer -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_:xs) 
  | n > length xs = []
  | otherwise = drop (n-1) xs

-- TASK:
-- "Take" the first n elements of a list.
-- If n > length xs, then you should take as many as you can.

-- >>> take 4 $ listFromRange 6 10
-- [6,7,8,9]
-- >>> take (-20) $ listFromRange 1 3
-- []

take :: Integer -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n _ | n<0 = []
take n (x:xs) = x : take (n-1) xs

-- TASK:
-- Append one list to another. append [1,2,3] [4,5,6] == [1,2,3,4,5,6]
-- This is called (++) in the base library.
-- HINT: Of course, you can think in the classic "inductive" way - I've got the result - what do I need to do at this step.
-- Or alternatively, you can think about "placing the second list at the end of the first":
-- 0. how do you get to the end of the first one?
-- 1. what do you need to do at each step to "remember" the elements of the first one?

-- >>> append [1,2,3] [4,7,3,7,2]
-- [1,2,3,4,7,3,7,2]

-- >>> append [1] []
-- [1]

unwraped :: Maybe a -> a
unwraped (Just x) = x
unwraped Nothing = error "womp womp"

append :: [a] -> [a] -> [a]
append [] ys = ys
append xs ys = append (take len xs) ((unwraped (ix len xs)) : ys) 
  where len = (length xs)-1

-- TASK:
-- Concatenate all the lists together.

-- >>> concat [[1,2,3], [41,69], [2222222], [5,7,8,9]]
-- [1,2,3,41,69,2222222,5,7,8,9]
-- >>> concat [[1,3], [], [], [5,7,8,9]]
-- [1,3,5,7,8,9]
-- >>> concat []
-- []

concat :: [[a]] -> [a]
concat [] = []
concat [xs] = xs
concat (x:y:xs) = concat $ (append x y) : xs

-- TASK:
-- Reverse a list. It's fine to do this however you like.

-- >>> reverse [2,4,3]
-- [3,4,2]
-- >>> reverse []
-- []

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = append (reverse xs) [x]

-- TASK:
-- Square all the numbers in a list

-- >>> squareList [1,243,-3,50]
-- [1,59049,9,2500]

squareList :: [Integer] -> [Integer]
squareList [] = []
squareList (x:xs) = x*x : squareList xs
-- squareList xs = Prelude.map (\x -> x*x) xs

-- TASK:
-- Pair up the given element with each of the elements a list.

-- >>> megaPair 41 [69,7,42]
-- [(41,69),(41,7),(41,42)]

megaPair :: a -> [b] -> [(a, b)]
megaPair _ [] = []
megaPair x (y:ys) = (x,y) : megaPair x ys

-- TASK:
-- Both of those functions above have the same structure - apply a function to each element of a list.
-- We can abstract this and get one of the most useful functions over lists (and containers in general).

-- >>> map succ [1,2,4]
-- [2,3,5]
-- >>> map (\x -> x * x * x) [1,2,3] -- same as squareList
-- [1,8,27]
-- >>> map (\x -> (3,x)) [1,2,3,1111,0] -- same as megaPair 3
-- [(3,1),(3,2),(3,3),(3,1111),(3,0)]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- TASK:
-- Check if all the elements in a list are True.

-- >>> and []
-- True
-- >>> and [False]
-- False
-- >>> and [True, True, True]
-- True

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs
 
-- TASK:
-- Check if all the elements of a list satisfy a predicate
-- Implement this using map and and.

-- >>> all isPrime [211]
-- True
-- >>> all isPrime [2,3,11,7]
-- True
-- >>> all isPrime []
-- True

all :: (a -> Bool) -> [a] -> Bool
all f xs = and $ map f xs

-- TASK:
-- Implement the cartesian product of two lists.
-- Don't use a list comprehension!

-- >>> cartesian [1,2,3,8] [105,6,9]
-- [(1,105),(1,6),(1,9),(2,105),(2,6),(2,9),(3,105),(3,6),(3,9),(8,105),(8,6),(8,9)]
-- >>> cartesian [] [4,5,6]
-- []
-- >>> cartesian [1,2,3] []
-- []

cartesian :: [a] -> [b] -> [(a, b)]
cartesian [] _ = []
cartesian _ [] = []
cartesian (x:xs) ys = append (megaPair x ys) $ cartesian xs ys 

-- TASK:
-- We can generalise cartesian to work with arbitrary functions instead of just (,),
-- taking elements "each with each"
-- This is also the generalisation of cartesian, as seen in the examples.

-- >>> lift2List (+) [1] []
-- []
-- >>> lift2List (+) [1,2] [10]
-- [11,12]
-- >>> lift2List (*) [1,2,3] [16,23]
-- [16,23,32,46,48,69]
-- >>> lift2List (,) [1,2,3,8] [105,6,9] -- same as cartesian [1,2,3,8] [105,6,9]
-- [(1,105),(1,6),(1,9),(2,105),(2,6),(2,9),(3,105),(3,6),(3,9),(8,105),(8,6),(8,9)]

lift2List :: (a -> b -> c) -> [a] -> [b] -> [c]
lift2List _ [] _ = []
lift2List f (x:xs) ys = append (map (f x) ys) $ lift2List f xs ys 

-- TASK:
-- The "filtering" part of a list comprehension - leave only those elements, that satisfy the given predicate.

-- >>> even 3
-- False
-- >>> even 4
-- True
-- >>> filter even [1,4..100]
-- [4,10,16,22,28,34,40,46,52,58,64,70,76,82,88,94,100]
-- >>> filter isPrime [1..30]
-- [2,3,5,7,11,13,17,19,23,29]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

data Digit
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    deriving (Show, Enum, Eq)

-- TASK:
-- Parse a character into a digit.
-- The easiest way is to pattern match on all the cases.
--
-- OPTIONAL: You can try to use the LambdaCase extension here, in order to avoid writing parseDigit 10 times:
-- First, you need to add {-# LANGUAGE LambdaCase #-} to the top of the file (it's already enabled in this file)
-- Second, lambda case is used as follows:
-- Every time you write `\case ...`, that is desugared into `\x -> case x of ...`
-- For example:
-- safeDiv n = \case
--   0 -> Nothing
--   m -> ...
--

-- >>> parseDigit '5'
-- Just Five
-- >>> parseDigit ' '
-- Nothing
-- >>> parseDigit 'c'
-- Nothing

parseDigit :: Char -> Maybe Digit
parseDigit sym
  | sym == '0' = Just Zero
  | sym ==  '1' = Just One
  | sym ==  '2' = Just Two
  | sym ==  '3' = Just Three
  | sym ==  '4' = Just Four
  | sym ==  '5' = Just Five
  | sym ==  '6' = Just Six
  | sym ==  '7' = Just Seven
  | sym ==  '8' = Just Eight
  | sym ==  '9' = Just Nine
  |otherwise = Nothing

-- TASK:
-- See if all the values in a list xs are Just, returning Just xs only if they are.
-- We can think of this as all the computations in a list "succeeding",
-- and therefore the entire "computation list" has "succeeded.
-- Note that it is vacuously that all the elements in the empty list are Just.

-- >>> validateList []
-- Just []
-- >>> validateList [Just 41, Just 6, Just 8,  Just 2]
-- Just [41,6,8,2]
-- >>> validateList [Nothing, Just 7, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing

validateList :: Eq a => [Maybe a] -> Maybe [a]
validateList xs = if and $ map (\x -> if x /= Nothing then True else False) xs then Just $ map unwraped xs else Nothing

-- TASK:
-- You often have a collection (list) of things, for each of which you want to
-- perform some computation, that might fail (returning Maybe).
-- Let's implement a function to do exactly this -
-- execute a "failing computation" for all the items in a list,
-- immediately "aborting" upon a failure.
-- Think about how to reuse validateList.
-- This is called traverseListMaybe, because it's a special case of a generic function called traverse
-- that performs "actions" for each element of a "collection", specialised to List and Maybe

-- >>> traverseListMaybe (\x -> if even x then Just (x+1) else Nothing) [2,4,6]
-- Just [3,5,7]
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverseListMaybe (5 `safeDiv`) [0,2]
-- Nothing
-- >>> traverseListMaybe (8 `safeDiv`) [3,2]
-- Just [2,4]

traverseListMaybe :: Eq b => (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f xs = validateList $ map f xs

-- TASK:
-- Convert a list of digits to a number.
-- You can use `toEnum :: Digit -> Int` to convert digit into an `Int`
-- and `fromIntegral :: Int -> Integer` to convert an `Int` into an `Integer`
-- Assume that the empty list converts to 0.
-- HINT: It might be easier to first reverse the list and then operate on it with a helper.

-- >>> digitsToNumber [Six,Nine,Three]
-- 693
-- >>> digitsToNumber [One,Two,Zero,Four]
-- 1204
-- >>> digitsToNumber [Zero,One,Two,Zero]
-- 120

digitsToNumber :: [Digit] -> Integer
digitsToNumber [] = 0
digitsToNumber xs = go $ reverse xs
  where
    go [] = 0
    go (n:ns) = (fromIntegral $ fromEnum n) + 10 * go ns

-- TASK:
-- Combine the previous functions to parse a number.

-- >>> parseNumber "0"
-- Just 0
-- >>> parseNumber "3"
-- Just 3
-- >>> parseNumber "69"
-- Just 69
-- >>> parseNumber "01236"
-- Just 1236
-- >>> parseNumber "blabla"
-- Nothing
-- >>> parseNumber "1t33"
-- Nothing

parseNumber :: String -> Maybe Integer
parseNumber str = if arr /= Nothing then Just $ digitsToNumber $ unwraped arr else Nothing
  where
    arr = validateList (map parseDigit str)

-- TASK:
-- Notice how in parseNumber, in the Nothing case we returned Nothing,
-- and in the Just case, we returned Just again, with a "non-maybe" function inside.
-- This turns out to be very useful, and if you compare it to the map for lists, it's almost the same.
-- Let's write it now, so we don't have to do that pattern match again in the future.
-- Afterwards, you can reuse this function in parseNumber.

-- >>> maybeMap succ $ Just 7
-- Just 8
-- >>> maybeMap succ Nothing
-- Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x

-- TASK:
-- Another way to combine lists
-- Instead of "taking all possible combinations" we group the lists "pointwise"
-- If one list is shorter than the other, you can stop there.

-- >>> zip [1,7,3] [4,5,6]
-- [(1,4),(7,5),(3,6)]
-- >>> zip [] [1,3]
-- []
-- >>> zip [2] [4,5,6]
-- [(2,4)]

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- TASK:
-- And the generalised version of zip.

-- >>> zipWith (,) [1,2,3] [4,5]
-- [(1,4),(2,5)]
-- >>> zipWith (*) [1,2,3] [4,5,6]
-- [4,10,18]
-- >>> zipWith (:) [1,2,3] [[8],[5,7],[]]
-- [[1,8],[2,5,7],[3]]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- TASK:
-- Transpose a matrix. Assume all the inner lists have the same length.
-- HINT: zipWith and map might be useful here.

-- >>> transpose [[1]]
-- [[1]]
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- >>> transpose [[1],[2]]
-- [[1,2]]
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]

transpose :: [[a]] -> [[a]]
transpose = undefined

-- TASK:
-- Reverse a list, but in linear time (so if the input list has n elements, you should only be doing at most ~n operations, not n^2)
-- You will need a helper local definition.

-- >>> reverseLinear [1,2,3]
-- Prelude.undefined
-- >>> reverseLinear []
-- Prelude.undefined

reverseLinear :: [a] -> [a]
reverseLinear = undefined
