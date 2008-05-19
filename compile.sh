#!/bin/sh
rm -r out/*

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-xml-console -hidir out -odir out src/ConsoleXmlMain.hs

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-db-console -hidir out -odir out src/ConsoleDbMain.hs

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-loader -hidir out -odir out src/LoaderMain.hs
