#!/bin/sh
rm out/*
ghc -Wall -fglasgow-exts --make -o wf-console -hidir out -odir out src/ConsoleMain.hs `find src -name "*.lhs"`

ghc -Wall -fglasgow-exts --make -o wf-loader -hidir out -odir out src/LoaderMain.hs `find src -name "*.lhs"`
