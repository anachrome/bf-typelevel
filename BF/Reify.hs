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

class Peano t where
    toInt :: t -> Int
instance Peano Zero where
    toInt _ = 0
instance Peano a => Peano (Succ a) where
    toInt _ = 1 + toInt (undefined :: a)


--instance a ~ Int => Reify Zero a where
--    reify = toInt
--instance (Peano n, a ~ Int) => Reify (Succ n) a where
--    reify = toInt

instance (Peano n, res ~ Int) => Reify n res where
    reify = toInt

--instance Reify Zero Int where
--    reify _ = 0
--instance Reify (Succ n) Int where
--    reify _ = 1 + reify (undefined :: n)

--
-- list
--

instance Reify Nil [Int] where
    reify _ = []
instance (Peano t, Reify u [Int]) => Reify (t :* u) [Int] where
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
--instance res ~ String => Reify (Array ls x rs) res where
    -- reify _ = Z (reify (undefined :: ls))
    --             (reify (undefined :: x ))
    --             (reify (undefined :: rs))
    --reify _ = (showLeft . reify $ (undefined :: ls))
    --       ++ "*" ++ (show $ reify (undefined :: x ))
    --       ++ (showRight . reify $ (undefined :: rs))
    reify = undefined

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
