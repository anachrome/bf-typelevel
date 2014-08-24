-- functions to convert final type structures into normal haskell values; the
-- counterpart to BF.Typify

{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables 
           , TypeFamilies
           , DataKinds
           , PolyKinds
           #-}

module BF.Reify where

import Data.Proxy

import BF.Types

-- the main typeclass/function
class Reify (t :: k) a | t -> a where
    reify :: Proxy t -> a

-- integer
instance Reify Zero Int where
    reify _ = 0
instance Reify n Int => Reify (Succ n) Int where
    reify _ = reify (undefined :: Proxy n) + 1
instance Reify n Int => Reify (Pred n) Int where
    reify _ = reify (undefined :: Proxy n) - 1

-- list (of ints.  thanks liberal coverage condition)
instance Reify '[] [Int] where
    reify _ = []
instance (Reify t Int, Reify u [Int]) => Reify (t ': u) [Int] where
    reify _ =  reify (undefined :: Proxy t) : reify (undefined :: Proxy u) 

-- ziplist
instance ( Reify ls [a]
         , Reify x   a
         , Reify rs [a]
         , Show a
         , res ~ String
         ) => Reify ('ZipList ls x rs) res where
    reify _ = (showLeft . reify $ (undefined :: Proxy ls))
           ++ "*" ++ (show $ reify (undefined :: Proxy x ))
           ++ (showRight . reify $ (undefined :: Proxy rs))

showLeft :: Show a => [a] -> String
showLeft xs = case xs of
    [] -> "["
    xs -> init (show xs) ++ ","

showRight :: Show a => [a] -> String
showRight xs = case xs of
    [] -> "]"
    xs -> "," ++ tail (show xs)

-- threeples (the input and output of Eval)
instance ( Reify t a
         , Reify u b
         , Reify v c
         , res ~ (a,b,c)
         ) => Reify (t,u,v) res where
    reify _ = ( reify (undefined :: Proxy t)
              , reify (undefined :: Proxy u)
              , reify (undefined :: Proxy v) )
