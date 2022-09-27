-- questions ?

-- tournois : n equipes et m matchs
-- ajouter une equipe qui rencontre toutes les autres : + n

-- matchs (n + 1) = m + n

-- delta
-- si pas d'equipe pas de match
-- si une seul equipe pas de match

--matchs :: Int -> Int
--matchs 0 = 0
--matchs n = matchs (n - 1) + (n - 1)

-- une correction (attention min(fond,forme)), rappel : beau !

-- on generalise (autant que possible) le type des fonctions du bloc1

myHead :: [a] -> a
myHead (x : _) = x

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs
  where
    myAppend' (x : xs) = x : myAppend' xs
    myAppend' [] = ys

myInit :: [a] -> [a]
myInit [_] = []
myInit (x : xs) = x : myInit xs

myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

l1 :: [a]
l1 = []

l2 :: [Int]
l2 = 1 : l1

l3 :: [Bool]
l3 = True : l1

myLength :: [a] -> Int
myLength (_ : xs) = 1 + myLength xs
myLength [] = 0

myNull' :: [a] -> Bool
myNull' xs = length xs == 0

myReverse :: [a] -> [a]
myReverse (x : xs) = myReverse xs ++ [x]
myReverse [] = []

myConcat :: [[a]] -> [a]
myConcat (bs : bss) = bs ++ myConcat bss
myConcat [] = []

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x : xs) = x : myTake (n -1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x : xs) = myDrop (n -1) xs

myBangBang :: [a] -> Int -> a -- (!!)
myBangBang (x : xs) 0 = x
myBangBang (x : xs) n = myBangBang xs (n -1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y : ys)
  | x > y = y : myInsert x ys
  | otherwise = x : y : ys

{-
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x/=y)
  (/=) :: a -> a -> Bool
  x /= y = not (x==y)

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

instance Eq Int where
  x == y = Processeur.cmp x y

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x==y && xs==ys
  _      == _      = False

class Eq a => Ord a where
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  x >= y = x==y || x>y
-}

-- classes : Eq, Ord, Show

mySort :: Ord a => [a] -> [a]
mySort (x : xs) = myInsert x (mySort xs)
mySort [] = []

myNull'' :: Eq a => [a] -> Bool
myNull'' xs = xs == []

-- NEW STUFF

-- ordre superieur

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x : xs)
  | f x = x : myTakeWhile f xs
  | otherwise = []
myTakeWhile f [] = []

-- section : (2>) plus partiellement appliquee a son premier argument
-- section : (>2) plus partiellement appliquee a son second  argument

-- myHead :: [Int] -> Int moins general myHead :: [a] -> a

-- donner le type le plus general de la fonction myCompose (aka (.))

myCompose :: (a -> b) -> (c -> a) -> c -> b
myCompose f g x = f (g x)

-- donner une definition de la fonction myMap

myMap :: (a -> b) -> [a] -> [b]
myMap f (x : xs) = f x : myMap f xs
myMap f [] = []

--myMap f xs = []

test1 = myMap odd [1 .. 10]

-- map reduce google

-- iterateur java

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x : xs) = sousListes xs ++ map (x :) (sousListes xs)
sousListes [] = [[]]

-- une fonction plus generale: foldr
-- inferer le type (le plus general) de foldr
-- forme graphique de la liste en peigne

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x : xs) = f x (myFoldr f k xs)
myFoldr f k [] = k

--(&&) :: Bool -> Bool -> Bool
--True && b2 = b2
--False && b2 = False

myAnd' :: [Bool] -> Bool
myAnd' = foldr (&&) True

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y

-- infix vers prefix ajouter des parentheses : (+) 1 2
i0 :: Int
i0 = 1 + 2

i1 :: Int
i1 = (+) 1 2

i2 :: Int
i2 = add' 1 2

i3 :: Int
i3 = 1 `add'` 2

-- prefix vers infix ajouter des backquotes : 1 `add'` 2

-- un "nouveau type" String

s1 :: String -- [Char]
s1 = "caracteres"

-- un nouveau type tuples

myFst :: (a, b) -> a
myFst (x, y) = x

-- et des triplets, etc...

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f (t : q)
  | f t = myDropWhile f q
  | otherwise = t : q

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (t : q)
  | x == t = True
  | otherwise = myElem x q

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem x [] = True
myNotElem x (t : q)
  | x == t = False
  | otherwise = myNotElem x q

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (t : q)
  | f t = t : myFilter f q
  | otherwise = myFilter f q

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n [] = ([], [])
mySplitAt 0 list = ([], list)
mySplitAt n (t : q) = (t : x, y) where (x, y) = mySplitAt (n -1) q

myZip :: [a] -> [b] -> [(a, b)]
myZip (t1 : q1) (t2 : q2) = (t1, t2) : myZip q1 q2
myZip [] _ = []
myZip _ [] = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (t1 : q1) (t2 : q2) = f t1 t2 : myZipWith f q1 q2

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y) : q) = (x : a, y : b) where (a, b) = myUnzip q

-- define myZipWith' NON recursively
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' f l1 l2 = map (\(x, y) -> f x y) (zip l1 l2)

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' = foldr (++) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x l -> f x : l) []

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\b bs -> f b || bs) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\b bs -> f b && bs) True

myProduct :: [Int] -> Int
myProduct = foldr (*) 1

mySum :: [Int] -> Int
mySum = foldr (+) 0

mySort' :: [Int] -> [Int]
mySort' = foldr myInsert []

myReverse' :: [a] -> [a]
myReverse' = foldr (\x l -> l ++ [x]) []

-- define recursively

myElem' :: Eq a => a -> [a] -> Bool
myElem' y = foldr (\x b -> x == y || b) False

myNotElem' :: Eq a => a -> [a] -> Bool
myNotElem' y = foldr (\x b -> x /= y && b) True

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

premiers :: [Int]
premiers = crible [2 ..]

crible :: [Int] -> [Int]
crible (x : xs) = undefined

test2 = take 50 premiers