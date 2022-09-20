{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

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
myNull' x = length x == 0

myLength :: [Int] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' list = myReverse'' list []
  where
    myReverse'' :: [Int] -> [Int] -> [Int]
    myReverse'' (x : xs) y = myReverse'' xs (x : y)
    myReverse'' [] y = y

myConcat :: [[Int]] -> [Int]
myConcat [] = []
myConcat (t : q) = t ++ myConcat q

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (t : q) = t && myAnd q

myOr :: [Bool] -> Bool
myOr [] = False
myOr (t : q) = t || myOr q

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (t : q) = t * myProduct q

-- pas d'element neutre pour max et min

myTake :: Int -> [Int] -> [Int]
myTake 0 list = []
myTake n [] = []
myTake n (t : q) = t : myTake (n -1) q

myDrop :: Int -> [Int] -> [Int]
myDrop 0 list = list
myDrop n [] = []
myDrop n (t : q) = myDrop (n - 1) q

myDrop' :: Int -> [Int] -> [Int]
myDrop' 0 list = list
myDrop' n list
  | null list = []
  | otherwise = myDrop' (n -1) (tail list)

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang (t : _) 0 = t
myBangBang (_ : q) n = myBangBang q (n -1)

-- liste deja triee
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (t : q)
  | x > t = t : myInsert x q
  | otherwise = x : t : q

mySort :: [Int] -> [Int]
mySort [] = []
mySort (t : q) = myInsert t (mySort q)