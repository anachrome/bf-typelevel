#!/bin/bash

if [[ -z "$TAPELENGTH" ]]
then
    TAPELENGTH=100
fi

if [[ -z "$CONTEXTSTACK" ]]
then
    CONTEXTSTACK=100
fi

echo '{-# LANGUAGE TemplateHaskell'                            >  Makemain.hs
echo '           , DataKinds'                                  >> Makemain.hs
echo '           , PolyKinds'                                  >> Makemain.hs
echo '           #-}'                                          >> Makemain.hs
echo ''                                                        >> Makemain.hs
echo 'import Data.Proxy'                                       >> Makemain.hs
echo 'import Data.Char (ord, chr)'                             >> Makemain.hs
echo ''                                                        >> Makemain.hs
echo 'import BF.Types'                                         >> Makemain.hs
echo 'import BF.Core'                                          >> Makemain.hs
echo 'import BF.Typify'                                        >> Makemain.hs
echo 'import BF.Reify'                                         >> Makemain.hs
echo ''                                                        >> Makemain.hs
echo 'type T = $(ziplist $ replicate' "$TAPELENGTH" '0)'       >> Makemain.hs
echo 'type I = $(list . map ord $' "\"$2\")"                   >> Makemain.hs
echo "type O = '[]"                                            >> Makemain.hs
echo ''                                                        >> Makemain.hs
echo 'type Prog = $(load' "\"$1\")"                            >> Makemain.hs
echo ''                                                        >> Makemain.hs
echo 'main = putStr . map chr . reify'                         >> Makemain.hs
echo "     $ (undefined :: Proxy (EvalBF Prog '(T,I,O)))"      >> Makemain.hs
ghc Makemain.hs -o Makemain -odir=obj -hidir=obj -fcontext-stack=$CONTEXTSTACK\
    > /dev/null && ./Makemain && rm Makemain
rm -r Makemain.hs obj
