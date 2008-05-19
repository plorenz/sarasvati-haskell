#!/bin/sh
rm -r out/*

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-xml-console -hidir out -odir out src/ConsoleXmlMain.hs `find src -name "*.lhs"`

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-db-console -hidir out -odir out src/ConsoleDbMain.hs `find src -name "*.lhs"`

ghc -Wall -fglasgow-exts --make -isrc -o bin/wf-loader -hidir out -odir out src/LoaderMain.hs `find src -name "*.lhs"`
