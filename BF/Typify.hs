{-# LANGUAGE TemplateHaskell
           , FlexibleInstances #-}

-- template haskell functions for preprocessing and generating types from easier
-- to work with values; the counterpart to BF.Reify
module BF.Typify where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Text.Parsec.String (Parser)
import Text.Parsec (many, manyTill, char, anyChar, optionMaybe, parse)

import Language.Haskell.TH (TypeQ, runIO , conT, appT, mkName)
import Language.Haskell.TH.Syntax (addDependentFile)

import BF.Core
import BF.Types

--
-- template haskell function to load a .bf file into a string and typify it
--

load :: FilePath -> TypeQ
load file = do
    addDependentFile file -- any files we load, we depend on
    bf <=< runIO . readFile $ file

-- instead of from a file
loadfromstdin :: TypeQ
loadfromstdin = do
    bf =<< runIO getContents

--
-- typifying functions: do essentially the opposite of reify
--

-- there is not general typify class because it would be unnecessarily
-- ambiguous and superfluous.

bf :: String -> TypeQ
bf bf = case parse parseAST "bf" (clean bf) of
    Right t -> t
    -- literally the only possible syntax error
    Left  _ -> error "unmatching brackets"

int :: Int -> TypeQ
int i = foldr (const $ appT (conT ''Succ)) (conT ''Zero) [1 .. i]

list :: [Int] -> TypeQ
list = foldr (appT . appT (conT ''(:*)) . int) (conT ''Nil)

array :: [Int] -> TypeQ
array (x:xs) = conT ''Array `appT` list [] `appT` int  x `appT` list xs

--
-- parsers
--

parseAST :: Parser TypeQ
parseAST = combine <$> many parseASTChar

parseLoop :: Parser TypeQ
parseLoop = combine <$> manyTill parseASTChar (char ']')

combine :: [TypeQ] -> TypeQ
combine = foldr (appT . appT (conT ''(:>))) (conT ''EOBF) where

parseASTChar :: Parser TypeQ
parseASTChar = do
    c <- anyChar
    case c of
        '>' -> return $ conT ''Forward
        '<' -> return $ conT ''Backward
        '+' -> return $ conT ''IncBF
        '-' -> return $ conT ''DecBF
        '.' -> return $ conT ''Out
        ',' -> return $ conT ''In
        '[' -> appT (conT ''Loop) <$> parseLoop
        _   -> parseASTChar

-- clear out all the irrelevant characters so we can parse properly
clean :: String -> String
clean = filter (`elem` "<>+-.,[]")

