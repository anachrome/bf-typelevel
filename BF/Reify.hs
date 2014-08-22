{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables 
           , TypeSynonymInstances#-}

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


instance Reify Zero Int where
    reify = toInt
instance Peano a => Reify (Succ a) Int where
    reify = toInt

--
-- list
--

instance Reify Nil [a] where
    reify _ = []
instance (Reify t a , Reify u [a]) => Reify (t :* u) [a] where
    reify _ =  reify (undefined :: t) : reify (undefined :: u) 

--
-- array (ziplist, remember?)
--

instance ( Reify ls [a]
         , Reify x   a
         , Reify rs [a]
         , Show a
         ) => Reify (Array ls x rs) String where
    -- reify _ = Z (reify (undefined :: ls))
    --             (reify (undefined :: x ))
    --             (reify (undefined :: rs))
    reify _ = (showLeft  . reify $ (undefined :: ls))
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
         ) => Reify (t,u,v) (a,b,c) where
    reify _ = ( reify (undefined :: t)
              , reify (undefined :: u)
              , reify (undefined :: v) )

