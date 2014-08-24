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

-- integer, because for some reason Int isn't autopromoted with -XDataKinds
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

-- ziplist
data ZipList a = ZipList [a] a [a]

type family GetCur (a :: ZipList t) :: t where
    GetCur ('ZipList ls x rs) = x

type family PutCur (x :: t) (a :: ZipList t) :: ZipList t where
    PutCur x ('ZipList ls y rs) = 'ZipList ls x rs

type family Inc (a :: ZipList t) :: ZipList t where
    Inc ('ZipList ls x rs) = 'ZipList ls (Incr x) rs

type family Dec (a :: ZipList t) :: ZipList t where
    Dec ('ZipList ls x rs) = 'ZipList ls (Decr x) rs

type family Forth (a :: ZipList t) :: ZipList t where
    Forth ('ZipList ls x (r ': rs)) = 'ZipList (x ': ls) r rs

type family Back (a :: ZipList t) :: ZipList t where
    Back ('ZipList (l ': ls) x rs) = 'ZipList ls l (x ': rs)
