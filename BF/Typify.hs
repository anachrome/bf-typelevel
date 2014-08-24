-- template haskell functions for preprocessing and generating types from normal
-- haskell values; the counterpart to BF.Reify

{-# LANGUAGE TemplateHaskell
           , FlexibleInstances
           , DataKinds
           , TypeOperators
           #-}

module BF.Typify where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Text.Parsec.String (Parser)
import Text.Parsec (many, manyTill, char, anyChar, optionMaybe, parse)

import Language.Haskell.TH (TypeQ, runIO , promotedT, appT, mkName)
import Language.Haskell.TH.Syntax (addDependentFile)

import BF.Core
import BF.Types

-- template haskell function to load a .bf file into a string and typify it
load :: FilePath -> TypeQ
load file = do
    addDependentFile file -- any files we load, we depend on
    bf <=< runIO . readFile $ file

-- typifying functions: do essentially the opposite of reify
bf :: String -> TypeQ
bf bf = case parse ast "bf" (clean bf) of
    Right t -> t
    -- literally the only possible syntax error
    Left  _ -> error "unmatching brackets"

int :: Int -> TypeQ
int i = foldr (const $ appT (promotedT 'Succ)) (promotedT 'Zero) [1 .. i]

list :: [Int] -> TypeQ
list = foldr (appT . appT (promotedT '(:)) . int) (promotedT '[])

ziplist :: [Int] -> TypeQ
ziplist (x:xs) = promotedT 'ZipList `appT` list [] `appT` int  x `appT` list xs

-- parsers
ast :: Parser TypeQ
ast = combine <$> many astchar

loop :: Parser TypeQ
loop = combine <$> manyTill astchar (char ']')

combine :: [TypeQ] -> TypeQ
combine = foldr (appT . appT (promotedT '(:))) (promotedT '[]) where

astchar :: Parser TypeQ
astchar = do
    c <- anyChar
    case c of
        '>' -> return $ promotedT 'Forward
        '<' -> return $ promotedT 'Backward
        '+' -> return $ promotedT 'IncBF
        '-' -> return $ promotedT 'DecBF
        '.' -> return $ promotedT 'Out
        ',' -> return $ promotedT 'In
        '[' -> appT (promotedT 'Loop) <$> loop
        _   -> astchar

clean :: String -> String
clean = filter (`elem` "<>+-.,[]")
