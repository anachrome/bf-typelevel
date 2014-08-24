-- type level bf-ast and eval functions (RunBF, EvalBF, and ExecBF roughly
-- parallel their state-monad nomenbrethren)

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

type family RunBF (bf :: [BFElem]) (aio :: (ZipList k, [k], [k]))
    :: (ZipList k, [k], [k]) where
    RunBF '[] aio = aio
    RunBF (bf ': rest) aio = RunBF rest (RunBFElem bf aio)

type family RunBFElem (bf :: BFElem) (aio :: (ZipList k, [k], [k])) where
    RunBFElem Forward '(a, i, o) = '(Forth a, i, o)
    RunBFElem Backward '(a, i, o) = '(Back a, i, o)
    RunBFElem IncBF '(a, i, o) = '(Inc a, i, o)
    RunBFElem DecBF '(a, i, o) = '(Dec a, i, o)
    RunBFElem In '(a, (x ': inp), o) = '(PutCur x a, inp, o)
-- uncomment below to allow taking input from an empty list
--    RunBFElem In '(a, '[], o) = '(PutCur Zero a, '[], o)
    RunBFElem Out '(a, inp, o) = '(a, inp, (GetCur a ': o))
    RunBFElem (Loop loop) '( 'ZipList ls Zero rs, i, o ) = '( 'ZipList ls Zero rs, i, o )
    RunBFElem (Loop loop) '(a, i, o) = RunBFElem (Loop loop) (RunBF loop '(a, i, o))

-- auxiliaries
type family EvalBF (bf :: [BFElem]) (aio :: (ZipList k, [k], [k])) :: [k] where
    EvalBF bf aio = Third (RunBF bf aio)

type family ExecBF (bf :: [BFElem]) (aio :: (ZipList k, [k], [k])) :: ZipList k where
    ExecBF bf aio = First (RunBF bf aio)

-- helpers
type family First (tuple :: (a, b, c)) :: a where
    First '(a, b, c) = a
type family Third (tuple :: (a, b, c)) :: c where
    Third '(a, b, c) = c
