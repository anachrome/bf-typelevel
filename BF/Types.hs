-- types used in the evaluator

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

import Data.Typeable
import Data.Typeable.Internal

--
-- numbers
--

-- type level integers
-- because for some reason Int isn't autopromoted with -XDataKinds
data Int' = Zero | Succ Int' | Pred Int'

--data Zero
--data Succ a
--data Pred a

--class Incr a b | a -> b
--instance Incr Zero (Succ Zero)
--instance Incr (Succ a) (Succ (Succ a))
--instance Incr (Pred a) a

type family Incr (n :: Int') where
    Incr Zero = Succ Zero
    Incr (Succ n) = Succ (Succ n)
    Incr (Pred n) = n

--class Decr a b | a -> b
--instance Decr Zero (Pred Zero)
--instance Decr (Succ a) a
--instance Decr (Pred a) (Pred (Pred a))

type family Decr (n :: Int') where
    Decr Zero = Pred Zero
    Decr (Succ n) = n
    Decr (Pred n) = Pred (Pred n)

--
-- list
--

--data Nil
--infixr :*
--data a :* b

--class Reverse' a b c | a b -> c
--instance Reverse' acc Nil acc
--instance Reverse' (x :* acc) xs xs' => Reverse' acc (x :* xs) xs'

type family Reverse' (acc :: [k]) (ls :: [k]) :: [k] where
    Reverse' acc '[] = acc
    Reverse' acc (x ': xs) = Reverse' (x ': acc) xs

--class Reverse a b | a -> b
--instance Reverse' Nil xs xs' => Reverse xs xs'

type family Reverse (ls :: [k]) :: [k] where
    Reverse ls = Reverse' '[] ls

--
-- array (basically ziplist)
--

data Array a = Array [a] a [a]

--data Array ls x rs

--class GetCur a x | a -> x
--instance GetCur (Array ls x rs) x

type family GetCur (a :: Array t) :: t where
    GetCur ('Array ls x rs) = x

--class PutCur x b c | x b -> c
--instance PutCur x (Array ls y rs) (Array ls x rs)

type family PutCur (x :: t) (a :: Array t) :: Array t where
    PutCur x ('Array ls y rs) = 'Array ls x rs

--class Inc a b | a -> b
--instance Incr x x' => Inc (Array ls x rs) (Array ls x' rs)

type family Inc (a :: Array t) :: Array t where
    Inc ('Array ls x rs) = 'Array ls (Incr x) rs

--class Dec a b | a -> b
--instance Decr x x' => Dec (Array ls x rs) (Array ls x' rs)

type family Dec (a :: Array t) :: Array t where
    Dec ('Array ls x rs) = 'Array ls (Decr x) rs

--class Forth a b | a -> b
--instance Forth (Array ls x (r :* rs)) (Array (x :* ls) r rs)

type family Forth (a :: Array t) :: Array t where
    Forth ('Array ls x (r ': rs)) = 'Array (x ': ls) r rs

--class Back a b | a -> b
--instance Back (Array (l :* ls) x rs) (Array ls l (x :* rs))

type family Back (a :: Array t) :: Array t where
    Back ('Array (l ': ls) x rs) = 'Array ls l (x ': rs)
