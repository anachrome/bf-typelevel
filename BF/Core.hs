-- type level bf-ast and eval function

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

type family Eval bf (aio :: (ZipList k, [k], [k])) :: (ZipList k, [k], [k]) where
    Eval EOBF '(a, i, o) = '(a, i, o)
    Eval (Forward :> next) '(a, i, o) = Eval next '(Forth a, i, o)
    Eval (Backward :> next) '(a, i, o) = Eval next '(Back a, i, o)
    Eval (IncBF :> next) '(a, i, o) = Eval next '(Inc a, i, o)
    Eval (DecBF :> next) '(a, i, o) = Eval next '(Dec a, i, o)

-- Requesting input from an empty list results in a type error.  uncomment
-- the below to prevent this (and possibly allow infinite loops or context
-- stack overflows)
--    Eval (In :> next) '(a, '[], o) = '(a, '[], o)

    Eval (In :> next) '(a, (x ': inp), o) = Eval next '(PutCur x a, inp, o)

    Eval (Out :> next) '(a, inp, o) = Eval next '(a, inp, (GetCur a ': o))

    Eval (Loop loop :> next) '( 'ZipList ls Zero rs, i, o) = Eval next '( 'ZipList ls Zero rs, i, o)
    Eval (Loop loop :> next) '(a, i, o) = Eval (Loop loop :> next) (Eval loop '(a, i, o))

-- these three functions roughly parallel their state monad nomenbrethren
run :: '(tape', inp', out') ~ Eval bf '(tape, inp, out) 
    => Proxy bf -> Proxy '(tape, inp, out) -> Proxy '(tape', inp', Reverse out')
run = undefined

eval :: '(tape', inp', out') ~ Eval bf '(tape, inp, out)
     => Proxy bf -> Proxy '(tape, inp, out) -> Proxy (Reverse out')
eval = undefined

exec :: '(tape', inp', out') ~ Eval bf '(tape, inp, out)
     => Proxy bf -> Proxy '(tape, inp, out) -> Proxy tape'
exec = undefined
