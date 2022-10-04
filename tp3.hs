{-# LANGUAGE InstanceSigs #-}

import Data.List
import Data.Ord

-- questions ?

elem' :: Eq a => a -> [a] -> Bool
elem' e (x : xs) = e == x || elem' e xs
elem' e [] = False

elem'' :: Eq a => a -> [a] -> Bool
elem'' e (x : xs)
  | e == x = True
  | otherwise = elem'' e xs
elem'' e [] = False

-- ZF expressions / liste en comprehension

rectangle :: [(Int, Int)]
rectangle = [(x, y) | x <- [1 .. 4], y <- [1 .. 5]]

triangle :: [(Int, Int)]
triangle = [(x, y) | x <- [1 .. 4], y <- [1 .. 5], y <= x]

triangle' :: [(Int, Int)]
triangle' = [(x, y) | x <- [1 .. 4], y <- [1 .. x]]

myQSort :: Ord a => [a] -> [a]
myQSort (x : xs) = myQSort [y | y <- xs, y <= x] ++ [x] ++ myQSort [y | y <- xs, y > x]
myQSort [] = []

-- but du TP : trouver les solutions pour le compte est bon

-- VIDEO

-- sous listes et permutations pour considerer les nombres utilises dans le calcul

-- sous liste

-- deja vu au bloc 2
sousListes :: [a] -> [[a]]
sousListes [] = [[]]
sousListes (x : xs) = ys ++ map (x :) ys
  where
    ys = sousListes xs

injections :: a -> [a] -> [[a]]
injections e (x : xs) = (e : x : xs) : map (x :) (injections e xs)
injections e [] = [[e]]

permuts :: [a] -> [[a]]
permuts (x : xs) = [sx | s <- permuts xs, sx <- injections x s]
permuts [] = [[]]

permSousListes :: [a] -> [[a]]
permSousListes xs = [zs | ys <- sousListes xs, not (null ys), zs <- permuts ys]

partitionStricte :: [a] -> [([a], [a])]
partitionStricte [x1, x2] = [([x1], [x2])]
partitionStricte (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- partitionStricte xs]

-- I) generate and test (brute force)

data Op = Add | Sub | Mul | Div -- deriving Show

-- Op est un nouveau type

-- Add, Sub, Mul et Div sont quatre nouvelles constantes (constructeur) du type Op

--data MyBool = MyTrue | MyFalse

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

validOp :: Op -> Int -> Int -> Bool
validOp Sub n1 n2 = n1 > n2
validOp Div n1 n2 = n1 `mod` n2 == 0
validOp _ _ _ = True

evalOp :: Op -> Int -> Int -> Int
evalOp Add n1 n2 = n1 + n2
evalOp Sub n1 n2 = n1 - n2
evalOp Mul n1 n2 = n1 * n2
evalOp Div n1 n2 = n1 `div` n2

data Exp = Val Int | App Op Exp Exp

--   deriving (Show)

-- Exp est un nouveau type

-- Val est un nouveau constructeur du type :: Int -> Exp

-- App est un nouveau constructeur du type :: Op -> Exp -> Exp -> Exp

-- [] :: [a]
-- (:) :: a -> [a] -> [a]
--data MyList a = MyNil | MyCons a (MyList a)

-- step1: enumerate expressions
exps :: [Int] -> [Exp]
exps [n] = [Val n]
exps ns =
  [ App o g d | (gs, ds) <- partitionStricte ns, g <- exps gs, d <- exps ds, o <- [Add, Sub, Mul, Div]
  ]

-- step2: filter out invalid expressions

evalExp :: Exp -> Int
evalExp (App o g d) = evalOp o (evalExp g) (evalExp d)
evalExp (Val n) = n

validExp :: Exp -> Bool
validExp (Val n) = n > 0
validExp (App o g d) = validExp g && validExp d && validOp o (evalExp g) (evalExp d)

solutions :: [Int] -> Int -> [Exp]
solutions nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps ns)
      es' = filter validExp es
   in filter (\e -> evalExp e == cible) es'

test1 = solutions [1, 3, 7, 10, 25, 50] 765

-- II) fusionner la generation et le filtrage des expressions invalides

exps2 :: [Int] -> [Exp]
exps2 [n] = [Val n]
exps2 ns =
  [ App o g d
    | (gs, ds) <- partitionStricte ns,
      g <- exps2 gs,
      d <- exps2 ds,
      o <- [Add, Sub, Mul, Div],
      validOp o (evalExp g) (evalExp d)
  ]

solutions2 :: [Int] -> Int -> [Exp]
solutions2 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps2 ns)
   in filter (\e -> evalExp e == cible) es

test2 = solutions2 [1, 3, 7, 10, 25, 50] 765

-- III) memoiser l'evaluation

data Exp' = Val' Int | App' Op Exp' Exp' Int

--   deriving (Show)

evalExp' :: Exp' -> Int
evalExp' (Val' n) = n
evalExp' (App' _ _ _ n) = n

exps3 :: [Int] -> [Exp']
exps3 [n] = [Val' n]
exps3 ns =
  [ App' o g d n
    | (gs, ds) <- partitionStricte ns,
      g <- exps3 gs,
      d <- exps3 ds,
      o <- [Add, Sub, Mul, Div],
      validOp o (evalExp' g) (evalExp' d),
      n <- [evalOp o (evalExp' g) (evalExp' d)]
  ]

solutions3 :: [Int] -> Int -> [Exp']
solutions3 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps3 ns)
   in filter (\e -> evalExp' e == cible) es

test3 = solutions3 [1, 3, 7, 10, 25, 50] 765

-- IV) exploiter des proprietes arithmetiques

-- pour reduire l'espace de recherche on ajoute les regles :
-- - pas de multiplication par 1
-- - pas de division par 1
-- - addition et multiplication commutatives (ne considerer qu'un sens (quand les deux operandes sont differents))
validOp' :: Op -> Int -> Int -> Bool
validOp' Sub x y = x > y && y /= 0
validOp' Div x y = x `mod` y == 0 && y /= 1
validOp' Add x y = x /= 0 && y /= 0 && x >= y
validOp' Mul x y = x /= 1 && y /= 1 && x >= y

exps4 :: [Int] -> [Exp']
exps4 [n] = [Val' n]
exps4 ns =
  [ App' o g d n
    | (gs, ds) <- partitionStricte ns,
      g <- exps4 gs,
      d <- exps4 ds,
      o <- [Add, Sub, Mul, Div],
      validOp' o (evalExp' g) (evalExp' d),
      n <- [evalOp o (evalExp' g) (evalExp' d)]
  ]

solutions4 :: [Int] -> Int -> [Exp']
solutions4 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps4 ns)
   in filter (\e -> evalExp' e == cible) es

test4 = solutions4 [1, 3, 7, 10, 25, 50] 765

-- nombre de solutions

nombreDeSolutions3 = length test3

nombreDeSolutions4 = length test4

-- V) ne retourner qu'une solution exacte ou bien la plus proche

solutions5 :: [Int] -> Int -> [Exp']
solutions5 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps4 ns)
   in [close_sol es (head es)]
  where
    close_sol :: [Exp'] -> Exp' -> Exp'
    close_sol [] n = n
    close_sol (t : q) n
      | distance cible t < distance cible n = close_sol q t
      | otherwise = close_sol q n

distance :: Int -> Exp' -> Int
distance cible e = abs (cible - evalExp' e)

test5 = solutions5 [1, 3, 7, 10, 25, 50] 765

test6 = solutions5 [1, 3, 7, 10, 25, 50] 831

-- VI) affichez les expressions sous forme infixe en evitant des parentheses inutiles 1+(2*3) 1+2*3 (1+2)*3
instance Show Exp' where
  show :: Exp' -> String
  show (App' Add g d _) = "(" ++ show g ++ "+" ++ show d ++ ")"
  show (App' Mul g d _) = show g ++ "x" ++ show d
  show (App' Sub g d _) = "(" ++ show g ++ "-" ++ show d ++ ")"
  show (App' Div g d _) = show g ++ "/" ++ show d
  show (Val' e) = show e

instance Show Exp where
  show (App x g d)
    | x == Mul && x == Div = show g ++ show x ++ show d
    | otherwise = "(" ++ show g ++ show x ++ show d ++ ")"
  show (Val e) = show e

instance Eq Op where
  (==) Mul Mul = True
  (==) Add Add = True
  (==) Sub Sub = True
  (==) Div Div = True
  (==) _ _ = False

test7 = App Add (Val 1) (App Mul (Val 2) (Val 3))

test8 = App Mul (App Add (Val 1) (Val 2)) (Val 3)

-- VII) generalisez certaines fonctions avec de l'ordre superieur afin de reduire la duplication de code dans ce programme

-- misc : cherchez les solutions avec le moins d'operations en priorite
