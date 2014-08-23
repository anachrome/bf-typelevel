{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
           , ScopedTypeVariables 
           , TypeSynonymInstances
           , TypeFamilies
           #-}

module BF.Reify where

import BF.Types

class Reify t a | t -> a where
    reify :: t -> a

--
-- numbers
--

instance Reify Zero Int where
    reify _ = 0
instance Reify n Int => Reify (Succ n) Int where
    reify _ = 1 + reify (undefined :: n)

--
-- list (of ints.  thanks liberal coverage condition)
--

instance Reify Nil [Int] where
    reify _ = []
instance (Reify t Int, Reify u [Int]) => Reify (t :* u) [Int] where
    reify _ =  reify (undefined :: t) : reify (undefined :: u) 

--
-- array (ziplist, remember?)
--

instance ( Reify ls [a]
         , Reify x   a
         , Reify rs [a]
         , Show a
         , res ~ String
         ) => Reify (Array ls x rs) res where
    reify _ = (showLeft . reify $ (undefined :: ls))
           ++ "*" ++ (show $ reify (undefined :: x ))
           ++ (showRight . reify $ (undefined :: rs))

showLeft :: Show a => [a] -> String
showLeft xs = case xs of
    [] -> "["
    xs -> init (show xs) ++ ","

showRight :: Show a => [a] -> String
showRight xs = case xs of
    [] -> "]"
    xs -> "," ++ tail (show xs)

--
-- the final output of Eval is a threeple
--

instance ( Reify t a
         , Reify u b
         , Reify v c
         , res ~ (a,b,c)
         ) => Reify (t,u,v) res where
    reify _ = ( reify (undefined :: t)
              , reify (undefined :: u)
              , reify (undefined :: v) )
