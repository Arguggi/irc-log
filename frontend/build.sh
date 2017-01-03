#!/usr/bin/env bash

FRONTEND_FOLDER="$HOME/projects/irc-log/frontend"
OUTPUT_FOLDER="$FRONTEND_FOLDER/output"
INPUT_FOLDER="$FRONTEND_FOLDER/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/irc-dom/irc-dom.jsexe"
COMPILER_FOLDER="$HOME/Downloads/compiler-latest"

stack --stack-yaml stack-ghcjs.yaml build

case "$1" in
    "copy" ) ACTION="cp ./all.js $OUTPUT_FOLDER/all.min.js";;
    "dev" ) ACTION="java -jar $COMPILER_FOLDER/compiler.jar --js_output_file $OUTPUT_FOLDER/all.min.js ./all.js";;
    "production" ) ACTION="java -jar "$COMPILER_FOLDER/compiler.jar" --compilation_level=ADVANCED_OPTIMIZATIONS --js_output_file "$OUTPUT_FOLDER/all.min.js" ./all.js";;
    *) echo "Invalid action"; exit 1;;
esac


cd "$INPUT_FOLDER" || exit 1
#for i in ./*.js; do
#    echo "optimizing $i"
#    # Advanced optimizations break it :/
#    java -jar "$COMPILER_FOLDER/compiler.jar" \
#         --js_output_file "$OUTPUT_FOLDER/$i" \
#         "$i"
#done

$ACTION
#java -jar "$COMPILER_FOLDER/compiler.jar" \
#     --compilation_level=ADVANCED_OPTIMIZATIONS \
#     --js_output_file "$OUTPUT_FOLDER/all.min.js" \
#     ./all.js
