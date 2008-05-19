#!/bin/sh
ghc --make -o wf-console -hidir out -odir out `find src -name "*.lhs"`
