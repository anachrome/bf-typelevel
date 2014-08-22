-- core evaluator

{-# LANGUAGE MultiParamTypeClasses
           , TypeOperators
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances #-}

module BF.Core where

import BF.Types
import BF.Reify

infixr :>
data a :> b
data EOBF
data Forward
data Backward
data IncBF
data DecBF
data Out
data In
data Loop loop

class Eval bf array inp out array' inp' out'
         | bf array inp out -> array' inp' out'

instance Eval EOBF a i o a i o

instance ( Forth a a'
         , Eval next a' i o a'' i' o'
         ) => Eval (Forward :> next) a i o a'' i' o'
instance ( Back a a'
         , Eval next a' i o a'' i' o'
         ) => Eval (Backward :> next) a i o a'' i' o'
instance ( Inc a a'
         , Eval next a' i o a'' i' o'
         ) => Eval (IncBF :> next) a i o a'' i' o'
instance ( Dec a a'
         , Eval next a' i o a'' i' o'
         ) => Eval (DecBF :> next) a i o a'' i' o'

-- Requesting input from an empty list results in a "runtime" error.  uncomment
-- the below to prevent this (and possibly -- allow infinite loops or context
-- stack overflows)
-- instance Eval (In :> next) a Nil o a Nil o
instance ( PutCur x a ax
         , Eval next ax inp o a' i' o'
         ) => Eval (In :> next) a (x :* inp) o a' i' o'

instance ( GetCur a x
         , Eval next a i (x :* o) a' i' o'
         ) => Eval (Out :> next) a i o a' i' o'

instance ( Eval next (Array ls Zero rs) i o a' i' o'
         ) => Eval (Loop loop :> next) (Array ls Zero rs) i o a' i' o'
instance ( Eval loop a i o a' i' o'
         , Eval (Loop loop :> next) a' i' o' a'' i'' o''
         ) => Eval (Loop loop :> next) a i o a'' i'' o''

-- these three functions roughly parallel their state monad nomenbrethren
run  :: ( Eval bf array inp out array' inp' out'
        , Reverse out' out''
        ) => bf -> (array,inp,out) -> (array',inp',out'')
run  = undefined

eval :: ( Eval bf array inp out array' inp' out'
        , Reverse out' out''
        ) => bf -> (array,inp,out) -> out''
eval = undefined

exec :: ( Eval bf array inp out array' inp' out'
        ) => bf -> (array,inp,out) -> array
exec = undefined

