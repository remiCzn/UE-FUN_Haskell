import Data.Char

-- import Test.QuickCheck

-- QUESTIONS ?

-- a property is a predicate

appendProp1 :: ([Bool], [Bool]) -> Bool
appendProp1 (xs, ys) = length (xs ++ ys) == length xs + length ys

-- the library quicheck randomly generate values to check a predicates

-- test = quickCheck appendProp1

-- the verbose version shows the generated values (nb: start with
-- simplest/shortest values)

-- testV = verboseCheck appendProp1

-- we are going to rebuild our own (simple) version of quickcheck

-- how to generate random numbers ?

-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_aléatoires
-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_pseudo-aléatoires
-- Générateurs congruentiels linéaires

-- a sequence of integers with a long period and a fair distribution

u :: Int -> Int
u n = (16807 * n) `mod` (2 ^ 31 -1)

-- a constant (we could pass the integer, but since list a lazy we
-- pass the (potenitaly) infinite list)

type Random = [Int]

random :: Random
random = drop 2 (iterate u 1)

test1 = take 10 random

-- a generator of random values uses (the begining of) a list of
-- integers and returns the rest of the list that has not yet been
-- used

type Generator a = Random -> (a, Random)

-- to generate one Bool uses one Int

rBool :: Generator Bool
rBool (i : is) = (even i, is)

-- to generate three Bool uses three Int

r3Bools :: Generator (Bool, Bool, Bool)
r3Bools random =
  let (b1, random1) = rBool random
      (b2, random2) = rBool random1
      (b3, random3) = rBool random2
   in ((b1, b2, b3), random3)

test2 = fst (r3Bools random)

-- generate a list of bools

rListBool :: Generator [Bool]
rListBool random =
  let (isNil, random1) = rBool random -- one Int is used to decide if the list is empty or not
   in if isNil
        then ([], random1)
        else
          let (head, random2) = rBool random1 -- if not empty, the another Int is used to generate the head of the list
              (tail, random3) = rListBool random2 -- and so on
           in (head : tail, random3)

test3 = fst (rListBool random)

-- more cons than nil to get longer lists

rPercent :: Generator Int
rPercent (i : is) = (i `mod` 100, is)

rListBool' :: Generator [Bool]
rListBool' random =
  let (isNil, random1) = rPercent random
   in if (isNil < 20) -- 20 percent
        then ([], random1)
        else
          let (head, random2) = rBool random1
              (tail, random3) = rListBool' random2
           in (head : tail, random3)

test3' = fst (rListBool' random)

-- generate other type of elements

rList :: Generator a -> Generator [a]
rList genElt gen =
  let (isNil, gen1) = rPercent gen
   in if (isNil < 20)
        then ([], gen1)
        else
          let (head, gen2) = genElt gen1 -- generate one element
              (tail, gen3) = rList genElt gen2
           in (head : tail, gen3)

test3'' = fst (rList rBool random)

rChar :: Generator Char
rChar (i : is) = (chr (ord 'a' + i `mod` 26), is)

test4 = fst (rList rChar random)

-- surcharge java :
-- int m(int x);
-- int m(boolean x);
-- void m(boolean x); -- interdit en java, mais autorise en haskell

-- a quickcheck like

-- reminder: type Generator a = Random -> (a,Random)

-- each generator has a different *return* type, we use a class type
-- to overload myGenerate (note that this is not possible in java:
-- overloard only on the arguments type, not on the return type).

class TestGenerator a where
  myGenerate :: Generator a

instance TestGenerator Int where
  myGenerate (r : rs) = (r, rs)

instance TestGenerator Bool where
  myGenerate (r : rs) = (even r, rs)

instance TestGenerator Char where
  myGenerate (r : rs) = (chr (ord 'a' + r `mod` 26), rs)

instance TestGenerator a => TestGenerator [a] where
  myGenerate random =
    let (isNil, random1) = rPercent random
     in if (isNil < 20)
          then ([], random1)
          else
            let (head, random2) = myGenerate random1 -- the type of head decice which myGenerate is called
                (tail, random3) = myGenerate random2
             in (head : tail, random3)

-- yet another type
instance (TestGenerator a, TestGenerator b) => TestGenerator (a, b) where
  myGenerate random =
    let (a, random1) = myGenerate random
        (b, random2) = myGenerate random1
     in ((a, b), random2)

-- type base selection
testB :: [([[Bool]], Int)]
testB = fst (myGenerate random)

-- simply change the type to get more (or less) complex values

--testB :: [Int]
--testB :: [Bool]
--testB :: [[Bool]]
--testB :: [(Int,[Bool])]
--testB :: [(Int,[(Bool,String)])]

-- an infinite list of values to make enough tests

infiniteGenerator :: TestGenerator a => Generator [a]
infiniteGenerator random =
  let (head, random2) = myGenerate random
      (tail, random3) = myGenerate random2
   in (head : tail, random3)

myQuickCheck :: TestGenerator a => (a -> Bool) -> Bool
myQuickCheck prop =
  let (tests, _) = infiniteGenerator random
   in all prop (take 100 tests)

myTest :: Bool
myTest = myQuickCheck appendProp1

-- quickCheck offers many more possibilities (e.g., how to generate a
-- tree, how to generate very complex values by filtering, etc.)

-- monad : declare des effets, composer des effets
-- M a
-- thenM   :: M a -> (a -> M b) -> M b -- associatif
-- returnM :: a -> M a  -- element neutre de thenM
-- monad etat, parallelism, echec, exception, list...
-- IO a = RealWorld -> (a,RealWorld)

main :: IO ()
main = do
  print "exterieur"
  print "bloc"
  print testB

-- o.m(o1.m1(),o2.m2())
-- m1() { print "A" ; }
-- m2() { print "B" ; }