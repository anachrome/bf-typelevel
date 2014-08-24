-- type level bf-ast and eval function

{-# LANGUAGE UndecidableInstances
           , TypeFamilies
           , DataKinds
           , PolyKinds
           , TypeOperators
           #-}

module BF.Core where

import Data.Proxy

import BF.Types

-- this won't work until ghc implements kind synonyms
--type BF = [BFElem]
data BFElem = Forward | Backward | IncBF | DecBF | Out | In | Loop [BFElem]

type family Eval (bf :: [BFElem]) (aio :: (ZipList k, [k], [k])) :: (ZipList k, [k], [k]) where
    Eval '[] aio = aio
    Eval (bf ': rest) aio = Eval rest (EvalElem bf aio)

type family EvalElem (bf :: BFElem) (aio :: (ZipList k, [k], [k])) :: (ZipList k, [k], [k]) where
    EvalElem Forward '(a, i, o) = '(Forth a, i, o)
    EvalElem Backward '(a, i, o) = '(Back a, i, o)
    EvalElem IncBF '(a, i, o) = '(Inc a, i, o)
    EvalElem DecBF '(a, i, o) = '(Dec a, i, o)

-- Requesting input from an empty list results in a type error.  uncomment
-- the below to prevent this (and possibly allow infinite loops or context
-- stack overflows)
--    Eval In '(a, '[], o) = '(PutCur Zero a, '[], o)

    EvalElem In '(a, (x ': inp), o) = '(PutCur x a, inp, o)
    EvalElem Out '(a, inp, o) = '(a, inp, (GetCur a ': o))
    EvalElem (Loop loop) '( 'ZipList ls Zero rs, i, o ) = '( 'ZipList ls Zero rs, i, o )
    EvalElem (Loop loop) '(a, i, o) = EvalElem (Loop loop) (Eval loop '(a, i, o))

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
