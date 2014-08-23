-- types used in the evaluator

{-# LANGUAGE TypeOperators
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           #-}

-- you mean I can't choose which instances to export myself?
-- well I was gonna export them all anyway, so there ! >~>
module BF.Types where

import Data.Typeable
import Data.Typeable.Internal

--
-- numbers
--

data Zero
data Succ a
data Pred a

--class Incr a b | a -> b
--instance Incr Zero (Succ Zero)
--instance Incr (Succ a) (Succ (Succ a))
--instance Incr (Pred a) a

type family Incr n where
    Incr Zero = Succ Zero
    Incr (Succ n) = Succ (Succ n)
    Incr (Pred n) = n

--class Decr a b | a -> b
--instance Decr Zero (Pred Zero)
--instance Decr (Succ a) a
--instance Decr (Pred a) (Pred (Pred a))

type family Decr n where
    Decr Zero = Pred Zero
    Decr (Succ n) = n
    Decr (Pred n) = Pred (Pred n)

--
-- list
--

data Nil
infixr :*
data a :* b

--class Reverse' a b c | a b -> c
--instance Reverse' acc Nil acc
--instance Reverse' (x :* acc) xs xs' => Reverse' acc (x :* xs) xs'

type family Reverse' acc ls where
    Reverse' acc Nil = acc
    Reverse' acc (x :* xs) = Reverse' (x :* acc) xs

--class Reverse a b | a -> b
--instance Reverse' Nil xs xs' => Reverse xs xs'

type family Reverse ls where
    Reverse ls = Reverse' Nil ls

--
-- array (basically ziplist)
--

data Array ls x rs

--class GetCur a x | a -> x
--instance GetCur (Array ls x rs) x

type family GetCur a where
    GetCur (Array ls x rs) = x

--class PutCur x b c | x b -> c
--instance PutCur x (Array ls y rs) (Array ls x rs)

type family PutCur x a where
    PutCur x (Array ls y rs) = Array ls x rs

--class Inc a b | a -> b
--instance Incr x x' => Inc (Array ls x rs) (Array ls x' rs)

type family Inc a where
    Inc (Array ls x rs) = Array ls (Incr x) rs

--class Dec a b | a -> b
--instance Decr x x' => Dec (Array ls x rs) (Array ls x' rs)

type family Dec a where
    Dec (Array ls x rs) = Array ls (Decr x) rs

--class Forth a b | a -> b
--instance Forth (Array ls x (r :* rs)) (Array (x :* ls) r rs)

type family Forth a where
    Forth (Array ls x (r :* rs)) = Array (x :* ls) r rs

--class Back a b | a -> b
--instance Back (Array (l :* ls) x rs) (Array ls l (x :* rs))

type family Back a where
    Back (Array (l :* ls) x rs) = Array ls l (x :* rs)
