-- core evaluator

{-# LANGUAGE MultiParamTypeClasses
           , TypeOperators
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
           , TypeFamilies
           , DataKinds
           , PolyKinds
           #-}

module BF.Core where

import Data.Proxy

import BF.Types

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

--data BF = EOBF
--        | Forward BF
--        | Backward BF
--        | IncBF BF
--        | DecBF BF
--        | Out BF
--        | In BF
--        | Loop BF BF
-- alternatively
--type BF = [BFElem]
--data BFElem = Forward | Backward | IncBF | DecBF | Out | In | Loop BF

--class Eval bf array inp out array' inp' out'
--         | bf array inp out -> array' inp' out'

type family Eval bf (aio :: (Array k, [k], [k])) :: (Array k, [k], [k]) where
    Eval EOBF '(a, i, o) = '(a, i, o)
    Eval (Forward :> next) '(a, i, o) = Eval next '(Forth a, i, o)
    Eval (Backward :> next) '(a, i, o) = Eval next '(Back a, i, o)
    Eval (IncBF :> next) '(a, i, o) = Eval next '(Inc a, i, o)
    Eval (DecBF :> next) '(a, i, o) = Eval next '(Dec a, i, o)

--instance Eval EOBF a i o a i o
--
--instance ( Forth a a'
--         , Eval next a' i o a'' i' o'
--         ) => Eval (Forward :> next) a i o a'' i' o'
--instance ( Back a a'
--         , Eval next a' i o a'' i' o'
--         ) => Eval (Backward :> next) a i o a'' i' o'
--instance ( Inc a a'
--         , Eval next a' i o a'' i' o'
--         ) => Eval (IncBF :> next) a i o a'' i' o'
--instance ( Dec a a'
--         , Eval next a' i o a'' i' o'
--         ) => Eval (DecBF :> next) a i o a'' i' o'

-- Requesting input from an empty list results in a "runtime" error.  uncomment
-- the below to prevent this (and possibly -- allow infinite loops or context
-- stack overflows)
-- instance Eval (In :> next) a Nil o a Nil o
--instance ( PutCur x a ax
--         , Eval next ax inp o a' i' o'
--         ) => Eval (In :> next) a (x :* inp) o a' i' o'

    Eval (In :> next) '(a, (x ': inp), o) = Eval next '(PutCur x a, inp, o)

--instance ( GetCur a x
--         , Eval next a i (x :* o) a' i' o'
--         ) => Eval (Out :> next) a i o a' i' o'

    Eval (Out :> next) '(a, inp, o) = Eval next '(a, inp, (GetCur a ': o))

--instance ( Eval next (Array ls Zero rs) i o a' i' o'
--         ) => Eval (Loop loop :> next) (Array ls Zero rs) i o a' i' o'
--instance ( Eval loop a i o a' i' o'
--         , Eval (Loop loop :> next) a' i' o' a'' i'' o''
--         ) => Eval (Loop loop :> next) a i o a'' i'' o''

    Eval (Loop loop :> next) '( 'Array ls Zero rs, i, o) = Eval next '( 'Array ls Zero rs, i, o)
    Eval (Loop loop :> next) '(a, i, o) = Eval (Loop loop :> next) (Eval loop '(a, i, o))

-- these three functions roughly parallel their state monad nomenbrethren
--run  :: ( Eval bf array inp out array' inp' out'
--        , Reverse out' out''
--        ) => bf -> (array,inp,out) -> (array',inp',out'')
--run  = undefined

run :: '(array', inp', out') ~ Eval bf '(array, inp, out) 
    => Proxy bf -> Proxy '(array, inp, out) -> Proxy '(array', inp', Reverse out')
run = undefined

--eval :: ( Eval bf array inp out array' inp' out'
--        , Reverse out' out''
--        ) => bf -> (array,inp,out) -> out''
--eval = undefined

eval :: '(array', inp', out') ~ Eval bf '(array, inp, out)
     => Proxy bf -> Proxy '(array, inp, out) -> Proxy (Reverse out')
eval = undefined

--exec :: ( Eval bf array inp out array' inp' out'
--        ) => bf -> (array,inp,out) -> array
--exec = undefined

exec :: '(array', inp', out') ~ Eval bf '(array, inp, out)
     => Proxy bf -> Proxy '(array, inp, out) -> Proxy array'
exec = undefined
