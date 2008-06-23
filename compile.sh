#!/bin/sh
rm -r out/*

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-mem-example -hidir out -odir out src/Workflow/Example/MemoryWfEngineExample.hs

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-db-example -hidir out -odir out src/Workflow/Example/DatabaseWfEngineExample.hs

ghc -Wall -fno-warn-name-shadowing -fglasgow-exts --make -isrc -o bin/wf-db-loader-example -hidir out -odir out src/Workflow/Example/DatabaseLoaderExample.hs
