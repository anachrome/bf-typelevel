-- Brainfuck interpreter
-- :3

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Char (ord, chr)

import BF.Types
import BF.Core
import BF.Reify
import BF.Typify

-- feel free to use a full 30 000 cell tape.  let me know if it compiles before
-- the heat death of the universe.
tape   = undefined :: $(array $ replicate 100 0)
-- input  = undefined :: $(list $ map ord "lily\n\0")
input  = undefined :: $(list $ map ord "hello, world\n\0")
output = undefined :: Nil

-- prog = undefined :: $(load "rot13.bf")

prog = undefined :: $(bf ",[+.-,]")
main = putStr . map chr . reify . eval prog $ (tape, input, output)
