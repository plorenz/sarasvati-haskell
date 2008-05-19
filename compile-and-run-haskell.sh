#!/bin/sh
rm out/*
ghc -fglasgow-exts --make -o wf-console -hidir out -odir out `find src -name "*.lhs"`
