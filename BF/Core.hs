-- type level bf-ast and eval functions.  RunBF, EvalBF, and ExecBF roughly
-- parallel their state-monad nomenbrethren.

{-# LANGUAGE UndecidableInstances
           , TypeFamilies
           , DataKinds
           , PolyKinds
           , TypeOperators
           #-}

module BF.Core where

import BF.Types

-- this won't work until ghc implements kind synonyms
--type BF = [BFElem]
data BFElem = Forward | Backward | IncBF | DecBF | Out | In | Loop [BFElem]

type family Eval (bf :: [BFElem]) (aio :: (ZipList Int', [Int'], [Int']))
    :: (ZipList Int', [Int'], [Int']) where
    Eval '[] aio = aio
    Eval (bf ': rest) aio = Eval rest (EvalElem bf aio)

type family EvalElem (bf :: BFElem) (aio :: (ZipList Int', [Int'], [Int'])) where
    EvalElem Forward '(a, i, o) = '(Forth a, i, o)
    EvalElem Backward '(a, i, o) = '(Back a, i, o)
    EvalElem IncBF '(a, i, o) = '(Inc a, i, o)
    EvalElem DecBF '(a, i, o) = '(Dec a, i, o)
    EvalElem In '(a, (x ': inp), o) = '(PutCur x a, inp, o)
-- uncomment below to allow taking input from an empty list
--    EvalElem In '(a, '[], o) = '(PutCur Zero a, '[], o)
    EvalElem Out '(a, inp, o) = '(a, inp, (GetCur a ': o))
    EvalElem (Loop loop) '( 'ZipList ls Zero rs, i, o ) = '( 'ZipList ls Zero rs, i, o )
    EvalElem (Loop loop) '(a, i, o) = EvalElem (Loop loop) (Eval loop '(a, i, o))

-- auxiliaries
type family RunBF (bf :: [BFElem]) (aio :: (ZipList Int', [Int'], [Int'])) where
    RunBF bf aio = ReverseThird (Eval bf aio)

type family EvalBF (bf :: [BFElem]) (aio :: (ZipList Int', [Int'], [Int'])) where
    EvalBF bf aio = Reverse (Third (Eval bf aio))

type family ExecBF (bf :: [BFElem]) (aio :: (ZipList Int', [Int'], [Int'])) where
    ExecBF bf aio = First (Eval bf aio)

-- helpers
type family First (tuple :: (a, b, c)) :: a where
    First '(a, b, c) = a
type family Third (tuple :: (a, b, c)) :: c where
    Third '(a, b, c) = c
type family ReverseThird (tuple :: (a, b, [c])) :: (a, b, [c]) where
    ReverseThird '(a, b, c) = '(a, b, Reverse c)
