{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List

-- :r reload un fichier

{-
-- commentaires, multilignes

-}

-- constante entiere, identifiant, declaration typee, definition

c1 :: Int
c1 = 1

c2 :: Int
c2 = 2 + 1

c3 :: Int
c3 = (+) 2 1

f4 :: Int -> Int
f4 x = (+) 2 x

mySub :: Int -> (Int -> Int)
mySub x y = x - y

-- application partielle (et eta reduction)

myNeg :: Int -> Int
myNeg x = mySub 0 x

myNeg' :: Int -> Int
myNeg' = mySub 0

-- booleen et paresse

b1 :: Bool
b1 = True

b2 :: Bool
b2 = (b1 && False) || not b1

b3 :: Bool
b3 = 1 > 2

b4 :: Bool
b4 = 1 == 2

b5 :: Bool
b5 = 1 /= 2

-- liste d'entiers, nil, cons, liste en comprehension

l1 :: [Int]
l1 = []

l2 :: [Int]
l2 = 11 : 12 : l1

l3 :: [Int]
l3 = undefined

l4 :: [Int]
l4 = undefined

myNil :: [Int]
myNil = []

myCons :: Int -> [Int] -> [Int]
myCons = (:)

l5 :: [Int]
l5 = [1 .. 10]

l6 :: [Int]
l6 = [1, 3 .. 10]

l7 :: [Int]
l7 = [10, 8 .. 3]

-- pattern matching

myHead :: [Int] -> Int
myHead (x : xs) = x

myTail :: [Int] -> [Int]
myTail (_ : es) = es

-- fonction recursive

--myAppend xs ys = (head xs) : (myAppend (tail xs) ys)

myAppend :: [Int] -> [Int] -> [Int]
myAppend (x : xs) ys = x : myAppend xs ys
myAppend [] ys = ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys
  | not (null xs) = head xs : myAppend' (tail xs) ys
  | otherwise = ys

myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' xs ys
  | null xs = ys
  | not (null xs) = head xs : myAppend'' (tail xs) ys

myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x : xs) ys =
  let suite = myAppend4 xs ys
   in x : suite
myAppend4 [] ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 (x : xs) ys = x : suite where suite = myAppend5 xs ys
myAppend5 [] ys = ys

myAppend6 :: [Int] -> [Int] -> [Int]
myAppend6 xs ys = myAppend6' xs
  where
    myAppend6' :: [Int] -> [Int]
    myAppend6' (x : xs) = x : myAppend6' xs
    myAppend6' [] = ys

-- a vous...

myInit :: [Int] -> [Int]
myInit [x] = []
myInit (x : xs) = x : myInit xs

myLast :: [Int] -> Int
myLast [a] = a
myLast (_ : xs) = myLast xs

myNull :: [Int] -> Bool
myNull [] = True
myNull (_ : _) = False

--
myNull' :: [Int] -> Bool
myNull' x
  | null x = True
  | otherwise = False

myLength :: [Int] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x : xs) = myAppend (myReverse xs) [x]

-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' = undefined

myConcat :: [[Int]] -> [Int]
myConcat [] = []
myConcat (x : xs) = myAppend x (myConcat xs)

myAnd :: [Bool] -> Bool
myAnd [h] = h
myAnd (h : t) = h && myAnd t

myOr :: [Bool] -> Bool
myOr = undefined

myProduct :: [Int] -> Int
myProduct = undefined

-- pas d'element neutre pour max et min

myTake :: Int -> [Int] -> [Int]
myTake = undefined

myDrop :: Int -> [Int] -> [Int]
myDrop = undefined

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang = undefined

-- liste deja triee
myInsert :: Int -> [Int] -> [Int]
myInsert = undefined

mySort :: [Int] -> [Int]
mySort = undefined