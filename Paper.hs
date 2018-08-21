{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Paper where

import Data.List
import qualified Data.Vector.Unboxed as V
import Debug.Trace

data Cofree f a = Cofree a (f (Cofree f a)) deriving Functor

unit :: Functor f => (a -> f a) -> (a -> Cofree f a)
unit k a = Cofree a (fmap (unit k) (k a))


showCache :: Cofree Nat Int -> String
showCache (Cofree i k) = show i ++ case k of
                                      Zero -> ""
                                      (Next cf) -> showCache cf

data Fix f a = Fix (f (Fix f a))

newtype Term f = In { out :: f (Term f) }

histo :: forall f a .
         Functor f
      => (f (Cofree f a) -> a)
      -> Term f
      -> a
histo k f = k (fmap worker (out f))
  where
    worker :: Term f -> Cofree f a
    worker t = Cofree (histo k t) (fmap worker (out t))

histo2 :: forall f a .
         Functor f
      => (f (Cofree f a) -> a)
      -> Term f
      -> a
histo2 b = head_c . cata (cons_c . split b id)

cata :: Functor f => (f a -> a) -> Term f -> a
cata k = k . fmap (cata k) . out

dyna :: Functor f => (f (Cofree f a) -> a) -> (c -> f c) -> c -> a
dyna a c = x where x = a . fmap (fmap x . unit c ) . c

dyna2 :: Functor f => (f (Cofree f a) -> a) -> (c -> f c) -> c -> a
dyna2 a c = head_c . h
  where
    h = cons_c . split a id . fmap h . c

cons_c (a, b) = Cofree a b

head_c (Cofree a _) = a

split f g a = (f a, g a)

data Nat a
        = Zero
        | Next a
        deriving Functor

-- Convert from a natural number to its foldable equivalent, and vice
-- versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

expand_nn :: Int -> Term NegNat
expand_nn 0 = In ZeroNN
expand_nn n = In (NextNN (expand_nn (n - 1)))

compress :: Nat (Cofree Nat a) -> Int
compress Zero              = 0
compress (Next (Cofree _ tree)) = compress tree + 1

compress_nat :: Term Nat -> Int
compress_nat (In Zero) = 0
compress_nat (In (Next n)) = compress_nat n + 1

sub_nat :: Term Nat -> Term Nat -> Maybe (Term Nat)
sub_nat m (In Zero) = Just m
sub_nat (In Zero) (In (Next _)) = Nothing
sub_nat (In (Next m)) (In (Next n)) = sub_nat m n


index :: Show a => [a] -> Term Nat -> a
--index xs n | traceShow (xs, compress_nat n) False = undefined
index [] n = error ("Out of bounds" ++ show (compress_nat n))
index (x:xs) (In Zero) = x
index (x:xs) (In (Next r)) = index xs r

coins = [1,5,10,50]

change :: Int -> Int
change amt = histo goNaive (expand amt) where

change2 :: Int -> Int
change2 amt = histo2 goNaive (expand amt) where


goNaive :: Nat (Cofree Nat Int) -> Int
goNaive Zero = 1
goNaive curr@(Next tree) =
    let n = compress curr
        valid = filter (<= n) coins
        remaining = map (n -) valid
        (zeroes, toProcess) = partition (== 0) remaining
        toProcess' = map (subtract 1 . (n -)) toProcess
        results             = (sum (map (cflookup tree) toProcess'))
  in length zeroes + results

cflookup :: Cofree Nat Int -> Int -> Int
cflookup (Cofree a _) 0 = a
cflookup (Cofree a (Next more)) n = cflookup more (n - 1)
cflookup cf k = error (show k)

cflookup2 :: Cofree (NE v) Int -> Int -> Int
cflookup2 (Cofree a _) 0 = a
cflookup2 (Cofree a (Cons _ more)) n = cflookup2 more (n - 1)
cflookup2 cf k = error (show k)

coinsVector :: V.Vector Int
coinsVector = V.fromList [1,5,10, 50]

changeVector :: Int -> Int
changeVector amt = histo goVector (expand amt) where

changeVector2 :: Int -> Int
changeVector2 amt = histo2 goVector (expand amt) where


goVector :: Nat (Cofree Nat Int) -> Int
goVector Zero = 1
goVector curr@(Next tree) =
  let n = compress curr
      valid = V.filter (<= n) coinsVector
      remaining = V.map (n -) valid
      (zeroes, toProcess) = V.partition (== 0) remaining
      toProcess' = V.map (subtract 1 . (n -)) toProcess
      results             = V.sum (V.map (cflookup tree) toProcess')
  in V.length zeroes + results


changeSpecialised :: Int -> Int
changeSpecialised amt = histo go (expand amt) where

changeSpecialised2 :: Int -> Int
changeSpecialised2 amt = histo2 go (expand amt)



go :: Nat (Cofree Nat Int) -> Int
go Zero = 1
go curr@(Next tree) =
  let n = compress curr
  in
  traceShow n $
    (let v = n - 1
    in if v == 0 then 1
                 else if v > 0
                        then cflookup tree 0
                        else 0) +

    (let v = n - 5
    in if v == 0 then 1
                 else if v > 0
                        then cflookup tree 4
                        else 0) +
    (let v = n - 10
    in if v == 0 then 1
                 else if v > 0
                        then cflookup tree 9
                        else 0) +
    (let v = n - 50
    in if v == 0 then 1
                 else if v > 0
                        then cflookup tree 49
                        else 0)

data NE v x = Some v | Cons v x deriving (Show, Functor)

{-
changeSpecialised3 :: Int -> Int
changeSpecialised3 amt = dyna2 _ ping (expand amt)

type Coins = Nat

ping :: (Term Coins, Term Nat) -> NE ((Term Coins), (Term Nat)) ((Term Coins, Term Nat))
ping (In Zero, In Zero) = Some (In Zero, In Zero)
-}


count' :: Int -> [Int] -> Int
count' cents coins = aux coins !! cents
  where aux = foldr addCoin (1:repeat 0)
          where
            addCoin c oldlist = newlist
              where
                newlist = (take c oldlist) ++ zipWith (+) newlist (drop c oldlist)



{-
triangle :: (Term Nat, Term Nat) -> NE (Term Nat, Term Nat) (Term Nat, Term Nat)
triangle (In Zero, In Zero) = Some (In Zero, In Zero)
triangle (In Zero, In (Next (In Zero))) = Some (In Zero, In (Next (In Zero)))
triangle (In Zero, In (Next j)) = Cons (In Zero, In (Next j)) (j, j)
triangle (In (Next i), In (Next j)) = Cons (In (Next i), In (Next j)) (i, (In (Next j)))

coin_4 :: [Int] -> Int -> Double
coin_4 i j = dyna2 coin_alg triangle (expand i, expand j)

-- x = coins
coin_alg ::
  NE (Term Nat, Term Nat) (Cofree (NE (Term Nat, Term Nat)) Double)
   -> Double

coin_alg (Some (In Zero, In Zero)) = 0
coin_alg (Some (In Zero, In (Next (In Zero)))) = 1
coin_alg (Cons (i, j) table)
  | i < j - 1 = table !!
-}

data NegNat a
        = ZeroNN
        | NextNN a
        | Negative
        deriving Functor

coin_change_direct :: [Int] -> Int -> Int
coin_change_direct coins k = go (length coins, k)
  where
    go :: (Int, Int) -> Int
    go (_,0) = 1
    go (m,n)
      | n < 0 = 0
      | m == 0 && n > 0 = 0
      | otherwise = go (m-1, n) + go (m, n - coins !! (m - 1))

subtract_nn :: Term NegNat -> Term Nat -> Term NegNat
subtract_nn (In ZeroNN) (In Zero) = In ZeroNN
subtract_nn (In Negative) v = In Negative
subtract_nn (In (NextNN a)) (In Zero) = In (NextNN a)
subtract_nn (In (NextNN a)) (In (Next b)) = subtract_nn a b

-- (coins, value)
ping :: (Term Nat, Term Nat) -> NE (Term Nat, Term Nat) (Term Nat, Term Nat)
ping (In Zero, In Zero) = Some (In Zero , In Zero)
ping (In (Next m), In Zero) = Cons (In (Next m), In Zero) (m , In Zero)
ping (In Zero, (In (Next m))) = Cons (In Zero, (In (Next m))) (four, m)
ping (In (Next m), (In (Next n))) = Cons (In (Next m), In (Next n)) (m, In (Next n))




--ping (In (Next m), (In (NextNN n))) = Cons (m, In (NextNN n)) ((In (Next m)), (subtract_nn n (expand $ coins `index` (In (Next m)))))


getIndex (Some n) = n
getIndex (Cons n _) = n

instance Show (Term Nat) where
  show = show . compress_nat

zero = In Zero
one = In (Next zero)
two = In (Next one)
three = In (Next two)
four = In (Next three)

chain_dyna :: Int -> Int
chain_dyna n = dyna2 alg ping (four, expand n)
  where
    alg :: NE (Term Nat, Term Nat) (Cofree (NE (Term Nat, Term Nat)) Int)
            -> Int
    alg t | traceShow (getIndex t) False = undefined
    alg (Some (m , In Zero)) = 1
    alg (Some (In Zero, In (Next n))) = 0
    alg (Cons (In Zero, _) _) = 0
    alg (Cons ((In (Next m)), n) table) =
      let coin = coins `index` m
      in
      (case sub_nat n (expand coin)  of
        Nothing -> 0
        Just v  -> cflookup2 table ((coin  * 5) - 1))
      + cflookup2 table 0










