#!/usr/bin/env bash

FRONTEND_FOLDER="$HOME/projects/irc-log/frontend"
OUTPUT_FOLDER="$FRONTEND_FOLDER/output"
INPUT_FOLDER="$FRONTEND_FOLDER/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/irc-dom/irc-dom.jsexe"
COMPILER_FOLDER="$HOME/Downloads/compiler-latest"

cd "$INPUT_FOLDER" || exit 1
for i in ./*.js; do
    echo "optimizing $i"
    # Advanced optimizations break it :/
    java -jar "$COMPILER_FOLDER/compiler.jar" \
         --js_output_file "$OUTPUT_FOLDER/$i" \
         "$i"
done
