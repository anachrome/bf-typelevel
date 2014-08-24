-- basic type-level structures and functions used in the evaluator

{-# LANGUAGE TypeOperators
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , DataKinds
           , PolyKinds
           #-}

module BF.Types where

-- integers, because for some reason Int isn't autopromoted with -XDataKinds
data Int' = Zero | Succ Int' | Pred Int'

type family Incr (n :: Int') where
    Incr Zero = Succ Zero
    Incr (Succ n) = Succ (Succ n)
    Incr (Pred n) = n

type family Decr (n :: Int') where
    Decr Zero = Pred Zero
    Decr (Succ n) = n
    Decr (Pred n) = Pred (Pred n)

-- list
type family Reverse' (acc :: [k]) (ls :: [k]) :: [k] where
    Reverse' acc '[] = acc
    Reverse' acc (x ': xs) = Reverse' (x ': acc) xs

type family Reverse (ls :: [k]) :: [k] where
    Reverse ls = Reverse' '[] ls

-- array (basically ziplist)
data Array a = Array [a] a [a]

type family GetCur (a :: Array t) :: t where
    GetCur ('Array ls x rs) = x

type family PutCur (x :: t) (a :: Array t) :: Array t where
    PutCur x ('Array ls y rs) = 'Array ls x rs

type family Inc (a :: Array t) :: Array t where
    Inc ('Array ls x rs) = 'Array ls (Incr x) rs

type family Dec (a :: Array t) :: Array t where
    Dec ('Array ls x rs) = 'Array ls (Decr x) rs

type family Forth (a :: Array t) :: Array t where
    Forth ('Array ls x (r ': rs)) = 'Array (x ': ls) r rs

type family Back (a :: Array t) :: Array t where
    Back ('Array (l ': ls) x rs) = 'Array ls l (x ': rs)
