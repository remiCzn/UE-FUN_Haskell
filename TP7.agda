
-- single ligne comment 

-- haskell + dependant types = agda 


-- natural 

data Nat : Set where 
  Z : Nat
  S : Nat -> Nat

-- C-C C-L = load 

-- how many types? 1 type, Nat
-- how many values? infinity

one : Nat 
one = S Z 

two : Nat 
two = S one

add : Nat -> Nat -> Nat 
add Z n2 = n2
add (S n1) n2 = S (add n1 n2) 

three : Nat 
three = add one two

-- INCREMENTAL PROGRAMMING:
-- C-C C-L = load 
-- ? = dig a hole 
-- C-C C-, = print a hole
-- C-C C-C = break a variable
-- C-C C-Space = fill a hole 
-- C-C C-F = forward hole
-- C-C C-B = backward hole
-- C-C C-R = refine hole 
-- C-C C-N = normalize

-- equal algorithm 

data Bool : Set where 
  true : Bool 
  false : Bool

-- how many types? 1 type, Bool
-- how many values? 2 values, true, false 

equal : Nat -> Nat -> Bool 
equal Z Z = true
equal Z (S n2) = false
equal (S n1) Z = false
equal (S n1) (S n2) = equal n1 n2 

test1 : Bool 
test1 = equal two one

test2 : Bool 
test2 = equal two (add one one)

-- equal type

data Equal : Nat -> Nat -> Set where 
  case0 : Equal Z Z 
  case1 : {n1 n2 : Nat} -> Equal n1 n2 -> Equal (S n1) (S n2)

-- how many types? infinty Equal Z Z, Equal (S Z) Z, Equal Z (S Z), Equal (S Z) (S Z), 
-- how many values? one or zero value

test6 : Equal Z Z 
test6 = case0 

test7 : Equal (S Z) (S Z)
test7 = case1 case0 

test8 : Equal (S Z) Z 
test8 = {!   !} 

test3 : Equal two two 
test3 = case1 (case1 case0) 

test4 : Equal two one 
test4 = case1 {!   !} 

test5 : Equal two (add one one)
test5 = case1 (case1 case0) 

-- properties of equal 

equal-refl : (n : Nat) -> Equal n n 
equal-refl Z = case0
equal-refl (S n) = case1 (equal-refl n)

equal-sym : {n1 n2 : Nat} -> Equal n1 n2 -> Equal n2 n1
equal-sym case0 = case0
equal-sym (case1 p) = case1 (equal-sym p)

equal-trans : {n1 n2 n3 : Nat} -> Equal n1 n2 -> Equal n2 n3 -> Equal n1 n3
equal-trans case0 p23 = p23
equal-trans (case1 p12) (case1 p23) = case1 (equal-trans p12 p23) 

-- properties of add

add-right : (n : Nat) -> Equal (add n Z) n 
add-right Z = case0
add-right (S n) = case1 (add-right n)

add-assoc : (n1 n2 n3 : Nat) -> Equal (add (add n1 n2) n3) (add n1 (add n2 n3))
add-assoc Z n2 n3 = equal-refl (add n2 n3)
add-assoc (S n1) n2 n3 = case1 (add-assoc n1 n2 n3)

add-S : (n1 n2 : Nat) -> Equal (add n1 (S n2)) (S (add n1 n2))
add-S Z n2 = equal-refl (S n2)
add-S (S n1) n2 = case1 (add-S n1 n2)

add-commut : (n1 n2 : Nat) -> Equal (add n1 n2) (add n2 n1)
add-commut Z n2 = equal-sym (add-right n2)
add-commut (S n1) n2 = equal-sym (equal-trans (add-S n2 n1) (case1 (add-commut n2 n1)))



-- TP7

-- iterative addI 

addI : Nat -> Nat -> Nat 
addI Z n2 = n2
addI (S n1) n2 = addI n1 (S n2) 

addI-S : (n1 n2 : Nat) -> Equal (addI n1 (S n2)) (S (addI n1 n2))
addI-S Z n2 = case1 (equal-refl n2)
addI-S (S n1) n2 = addI-S n1 (S n2)

addI-right : (n : Nat) -> Equal (addI n Z) n 
addI-right Z = case0
addI-right (S n) = equal-trans (addI-S n Z) (case1 (addI-right n))

addI-commut : (n1 n2 : Nat) -> Equal (addI n1 n2) (addI n2 n1)
addI-commut n1 Z = addI-right n1
addI-commut n1 (S n2) = equal-trans (addI-S n1 n2) (equal-trans (case1 (addI-commut n1 n2)) (equal-sym (addI-S n2 n1))) 


addI-n1 : (n1 n2 n3 : Nat) -> Equal n1 n2 -> Equal (addI n1 n3) (addI n2 n3)
addI-n1 n1 n2 Z p12 = equal-trans (addI-right n1) (equal-sym (equal-trans (addI-right n2) (equal-sym p12)))
addI-n1 n1 n2 (S n3) p12 = equal-trans (addI-S n1 n3) (equal-sym (equal-trans (addI-S n2 n3) (case1 (equal-sym (addI-n1 n1 n2 n3 p12))))) 

addI-assoc : (n1 n2 n3 : Nat) -> Equal (addI (addI n1 n2) n3) (addI n1 (addI n2 n3))
addI-assoc Z n2 n3 = equal-refl (addI n2 n3)
addI-assoc (S n1) n2 n3 = equal-trans (addI-n1 (addI n1 (S n2)) (S (addI n1 n2)) n3 (addI-S n1 n2)) (equal-trans (addI-S (addI n1 n2) n3) (equal-sym (equal-trans (addI-S n1 (addI n2 n3)) (case1 (equal-sym (addI-assoc n1 n2 n3))))))

-- equivalence of addition functions (add as a pivot for properties)

addI-add : (n1 n2 : Nat) -> Equal (addI n1 n2) (add n1 n2)
addI-add Z n2 = equal-refl n2
addI-add (S n1) n2 = equal-trans (addI-S n1 n2) (case1 (addI-add n1 n2))

addI-commut' : (n1 n2 : Nat) -> Equal (addI n1 n2) (addI n2 n1)
addI-commut' n1 n2 = equal-trans (addI-add n1 n2) (equal-trans (add-commut n1 n2) (equal-sym (addI-add n2 n1)))

-- another definition of addition

addR : Nat -> Nat -> Nat 
addR n1 Z = n1
addR n1 (S n2) = S (addR n1 n2) 

-- TODO : prove addR Z n == n, addR assoc, addR commut, addR == add 
addR-right : (n : Nat) -> Equal (addR Z n) n
addR-right Z = case0
addR-right (S n) = case1 (addR-right n)

addR-assoc : (n1 n2 n3 : Nat) -> Equal (addR (addR n1 n2) n3) (addR n1 (addR n2 n3))
addR-assoc n1 n2 Z = equal-refl (addR n1 n2)
addR-assoc n1 n2 (S n3) = case1 (addR-assoc n1 n2 n3)

addR-S : (n1 n2 : Nat) -> Equal (addR n1 (S n2)) (S (addR n1 n2))
addR-S n1 Z = equal-refl (S n1)
addR-S n1 (S n2) = case1 {! addR-S  !}

addR-commut : (n1 n2 : Nat) -> Equal (addR n1 n2) (addR n2 n1)
addR-commut Z n2 = addR-right n2
addR-commut (S n1) n2 = {!   !} 