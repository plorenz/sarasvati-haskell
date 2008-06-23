#!/bin/sh
rm -r out/*

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-xml-console -hidir out -odir out src/Workflow/Example/ConsoleXmlFileUI.hs

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-db-console -hidir out -odir out src/Workflow/Example/ConsoleDatabaseUI.hs

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-loader -hidir out -odir out src/Workflow/Example/LoaderMain.hs
