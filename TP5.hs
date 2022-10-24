-- questions ?

-- purely functional data structures

-- what is a purely functional data structure? when you "modify" a
-- data structure, you get a modified copy of the data structure. the
-- original one has not changed. this is the only way to do in
-- haskell. remember the type list []. remember the list of random
-- integer in quickcheck (the new version is returned). in general it
-- can make programming simpler. because of local reasonning, where
-- side effects requires global reasonning (who has accessed the data
-- structure before me, between two of my accesses?)

-- in imperative languages, purely functionnal data stucture are also
-- used. in particular when it makes programming simpler. programming
-- is quite complex in concurrent programs...

-- list revisited

-- list promotes left access / construction

data List a = Nil | Cons a (List a) deriving (Show, Eq) -- the recursion is in the second arg of Cons

-- head/tail are O(1)

myHead :: List a -> a
myHead (Cons x xs) = x

myTail :: List a -> List a
myTail (Cons x xs) = xs

-- init/last are O(n) where n = length xs

myLast :: List a -> a
myLast (Cons x Nil) = x
myLast (Cons x xs) = myLast xs

myInit :: List a -> List a
myInit (Cons x Nil) = Nil
myInit (Cons x xs) = Cons x (myInit xs)

myAppend :: List a -> List a -> List a
myAppend (Cons x xs) ys = Cons x (myAppend xs ys)
myAppend Nil ys = ys

-- we can promote right access when an algorithm is more right
-- oriented that left oriented.

-- Tsil promotes right access / construction

data Tsil a = Lin | Snoc (Tsil a) a deriving (Show, Eq) -- the recursion is in the first param of Snoc

-- tsal/tini are O(1)

myTsal :: Tsil a -> a
myTsal (Snoc xs x) = x

myTini :: Tsil a -> Tsil a
myTini (Snoc xs x) = xs

-- but deah/liat are O(n)

myDeah :: Tsil a -> a
myDeah (Snoc Lin x) = x
myDeah (Snoc xs x) = myDeah xs

myLiat :: Tsil a -> Tsil a
myLiat (Snoc Lin x) = Lin
myLiat (Snoc xs x) = Snoc (myLiat xs) x

myDneppa :: Tsil a -> Tsil a -> Tsil a
myDneppa xs Lin = xs
myDneppa xs (Snoc ys y) = Snoc (myDneppa xs ys) y

-- purely functional queue

type Queue1 a = [a]

isEmpty1 :: Queue1 a -> Bool
isEmpty1 = null -- O(1)

enQueue1 :: a -> Queue1 a -> Queue1 a
enQueue1 = (:) -- O(1)

deQueue1 :: Queue1 a -> (a, Queue1 a)
deQueue1 xs = (last xs, init xs) -- O(n) where n = length xs

-- if we evaluate the complexity of a sequence: n * enqueue ; n * dequeue
-- queue1 : n*O(1) + O(n) + O(n-1) + ++ O(1) = O(n^2)

-- list zipper for editable list

type ListZ a = ([a], [a])

mkZipL :: [a] -> ListZ a
mkZipL xs = ([], xs)

unzipL :: ListZ a -> [a]
unzipL (ls, rs) = reverse ls ++ rs

goNext :: ListZ a -> ListZ a -- O(1)
goNext (ls, cursor : rs) = (cursor : ls, rs)

goPrevious :: ListZ a -> ListZ a -- O(1)
goPrevious (cursor : ls, rs) = (ls, cursor : rs)

transfoElt :: (a -> a) -> ListZ a -> ListZ a -- O(1)
transfoElt f (cursor : ls, rs) = (f cursor : ls, rs)

-- n-ary tree depth first

data Rose a = Rose a (Forest a) deriving (Show, Eq)

type Forest a = [Rose a]

label :: Rose a -> a
label (Rose a ts) = a

subTree :: Rose a -> Forest a
subTree (Rose a ts) = ts

dfs :: Rose a -> Forest a
dfs t = t : concat (map dfs (subTree t))

-- TODO 1: a third representation for list, Seq promotes concatenation

data Seq a = None | One a | Join (Seq a) (Seq a) deriving (Show, Eq) -- the recursion is in both params of Join

-- maintain a normal form: there is no None in a non empty Seq

myHeadS :: Seq a -> a
myHeadS (One h) = h
myHeadS (Join a b) = myHeadS a

myTailS :: Seq a -> Seq a
myTailS (One _) = None
myTailS (Join (One x) b) = b
myTailS (Join a b) = Join (myTailS a) b

myInitS :: Seq a -> Seq a
myInitS (One _) = None
myInitS (Join a (One x)) = a
myInitS (Join a b) = Join a (myInitS b)

myLastS :: Seq a -> a
myLastS (One x) = x
myLastS (Join _ b) = myLastS b

myAppendS :: Seq a -> Seq a -> Seq a
myAppendS None a = a
myAppendS a None = a
myAppendS a b = Join a b

myReverseS :: Seq a -> Seq a
myReverseS (Join a b) = Join (myReverseS b) (myReverseS a)
myReverseS (One x) = One x
myReverseS None = None

-- write a test
listS = Join (Join (One 1) (One 2)) (Join (One 3) (Join (One 4) (One 5)))

listS2 = Join (Join (One 6) (One 7)) (One 8)

test1 :: Bool
test1 = myHeadS listS == 1

test2 :: Bool
test2 = myTailS listS == Join (One 2) (Join (One 3) (Join (One 4) (One 5)))

test3 :: Bool
test3 = myInitS listS == Join (Join (One 1) (One 2)) (Join (One 3) (One 4))

test4 :: Bool
test4 = myLastS listS == 5

test5 :: Bool
test5 = myAppendS listS listS2 == Join (Join (Join (One 1) (One 2)) (Join (One 3) (Join (One 4) (One 5)))) (Join (Join (One 6) (One 7)) (One 8))

-- TODO 2: purely functional queue

type Queue2 a = ([a], [a])

isEmpty2 :: Queue2 a -> Bool -- O(1)
isEmpty2 (xs, ys) = null xs && null ys

enQueue2 :: a -> Queue2 a -> Queue2 a -- O(1)
enQueue2 x (xs, ys) = (x : xs, ys)

deQueue2 :: Queue2 a -> (a, Queue2 a) -- O(1) or O(n) where n = length xs
deQueue2 (xs, []) = deQueue2 ([], reverse xs)
deQueue2 (xs, y : ys) = (y, (xs, ys))

-- if we evaluate the complexity of a sequence: n * enqueue ; n * dequeue
-- queue2 : n * O(1) + O(n) + n * O(1) = O(n)

-- write a test
queue = ([1, 2, 3], [6, 5, 4])

queue2 = ([1, 2, 3], [])

test6 :: Bool
test6 = deQueue2 queue == (6, ([1, 2, 3], [5, 4]))

test7 :: Bool
test7 = deQueue2 queue2 == (3, ([], [2, 1]))

-- TODO 3: tree zipper for editable tree (zipper can be defined for any structure)

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

data Ctx a = NodeL (Ctx a) (Tree a) | NodeR (Tree a) (Ctx a) | Here deriving (Show, Eq)

type TreeZ a = (Tree a, Ctx a)

mkZipT :: Tree a -> TreeZ a
mkZipT t = (t, Here)

goLeft :: TreeZ a -> TreeZ a -- O(1)
goLeft (Node a b, ctx) = (a, NodeL ctx (Node a b))

goRight :: TreeZ a -> TreeZ a -- O(1)
goRight (Node a b, ctx) = (b, NodeR (Node a b) ctx)

goUp :: TreeZ a -> TreeZ a -- O(1)
goUp (ap, NodeL ctx (Node a b)) = (Node ap b, ctx)
goUp (ap, NodeR (Node b a) ctx) = (Node b ap, ctx)

transfoTree :: (Tree a -> Tree a) -> TreeZ a -> TreeZ a -- O(1)
transfoTree f (t, c) = (f t, c)

-- write a test
a = (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)), Here)

b = goRight a

f :: Tree Int -> Tree Int
f (Leaf a) = Leaf (2 * a)
f (Node a b) = Node (f a) (f b)

c = transfoTree f b

d = goUp c

testTreeZ :: Bool
testTreeZ =
  b == (Node (Leaf 2) (Leaf 3), NodeR (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) Here)
    && c == (Node (Leaf 4) (Leaf 6), NodeR (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) Here)
    && d == (Node (Leaf 1) (Node (Leaf 4) (Leaf 6)), Here)

-- TODO 4: a tree breadth first

bfs :: Forest a -> Forest a
bfs [] = []
bfs (t : ts) = t : bfs (ts ++ subTree t)

-- write a test
treeTest = Rose 1 [Rose 2 [Rose 4 [Rose 5 [], Rose 6 []], Rose 7 []], Rose 3 [Rose 8 [], Rose 9 [], Rose 10 []]]

x = map (\(Rose a _) -> a) (bfs [treeTest])

test9 :: Bool
test9 = x == [1, 2, 3, 4, 7, 8, 9, 10, 5, 6]

-- TODO 5: a Map from key to value
-- where keys are sorted in Fork l (k,a) r: (all keys of l <= k) && (k < all keys of r)

data Map k a = Tip | Fork (Map k a) (k, a) (Map k a)
  deriving (Show, Eq)

emptyMap :: Map k a
emptyMap = Tip

lookupMap :: Ord k => k -> Map k a -> a -- O(?) where n = size map (but ? if balanced trees)
lookupMap k1 (Fork l (k2, v) r)
  | k1 == k2 = v
  | k1 > k2 = lookupMap k1 r
  | k1 < k2 = lookupMap k1 l

insertMap :: Ord k => (k, a) -> Map k a -> Map k a -- update si k deja present
insertMap a Tip = Fork Tip a Tip
insertMap (k1, v) (Fork l (k2, a) r)
  | k1 == k2 = Fork l (k2, v) r
  | k1 > k2 = Fork l (k2, a) (insertMap (k1, v) r)
  | k1 < k2 = Fork (insertMap (k1, v) l) (k2, a) r

-- write test
mapTest = Fork (Fork (Fork Tip (1, "1") Tip) (3, "3") (Fork Tip (4, "4") Tip)) (5, "5") (Fork (Fork Tip (6, "6") Tip) (7, "7") (Fork Tip (8, "8") Tip))

-- TODO 6: study the code and the complexity of the following data structure
-- both a list and an array (purely functional random-access list, kris okasaki, 1995)

-- we assume trees are complete

data CTree a = CNode (CTree a) a (CTree a) | CLeaf a deriving (Show, Eq)

-- tree random access

lookupCTree :: Int -> CTree a -> Int -> a -- O(?)
lookupCTree size (CLeaf x) 0 = x
lookupCTree size (CNode l x r) 0 = x
lookupCTree size (CNode l x r) i
  | i <= div size 2 = lookupCTree (div size 2) l (i -1)
  | otherwise = lookupCTree (div size 2) r (i -1 - div size 2)

updateCTree :: Int -> CTree a -> Int -> a -> CTree a -- O(?)
updateCTree size (CLeaf x) 0 y = CLeaf y
updateCTree size (CNode l x r) 0 y = CNode l y r
updateCTree size (CNode l x r) i y
  | i <= div size 2 = CNode (updateCTree (div size 2) l (i -1) y) x r
  | otherwise = CNode l x (updateCTree (div size 2) r (i -1 - div size 2) y)

-- both a list and an array

type SizedTree a = (Int, CTree a)

type ListArray a = [SizedTree a]

lookupFA :: ListArray a -> Int -> a -- O(?) where n is the size/number of elements of the array
lookupFA ((s, t) : sts) i
  | i < s = lookupCTree s t i
  | otherwise = lookupFA sts (i - s)

updateFA :: ListArray a -> Int -> a -> ListArray a -- O(?) where n is the size/number of elements of the array
updateFA ((s, t) : sts) i y
  | i < s = (s, updateCTree s t i y) : sts
  | otherwise = (s, t) : updateFA sts (i - s) y

emptyFA :: ListArray a
emptyFA = [] -- O(1)

isEmptyFA :: ListArray a -> Bool
isEmptyFA = null -- O(1)

consFA :: a -> ListArray a -> ListArray a -- O(?)
consFA x ((s1, t1) : (s2, t2) : ts) | s1 == s2 = (1 + s1 + s2, CNode t1 x t2) : ts
consFA x ts = (1, CLeaf x) : ts

headFA :: ListArray a -> a -- O(?)
headFA ((1, CLeaf x) : ts) = x
headFA ((s, CNode l x r) : ts) = x

tailFA :: ListArray a -> ListArray a -- O(?)
tailFA ((1, CLeaf x) : ts) = ts
tailFA ((s, CNode l x r) : ts) = (div s 2, l) : (div s 2, r) : ts