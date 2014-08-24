-- Brainfuck interpreter
-- :3

{-# LANGUAGE TemplateHaskell
           , DataKinds
           , PolyKinds
           #-}

module Main where

import Data.Char (ord, chr)
import Data.Proxy

import BF.Types
import BF.Core
import BF.Reify
import BF.Typify

-- feel free to use a full 30 000 cell tape.  let me know if it compiles before
-- the heat death of the universe.
type Tape = $(array $ replicate 10 0)
type Input = $(list $ map ord "hello, world\n\0")
type Output = '[]

prog = undefined :: Proxy $(bf ",[.,]")
--prog = undefined :: Proxy $(load "rot13.bf")
--prog = undefined :: Proxy $(loadfromstdin)
main = putStr . map chr . reify . eval prog
     $ (undefined :: Proxy '(Tape, Input, Output))
