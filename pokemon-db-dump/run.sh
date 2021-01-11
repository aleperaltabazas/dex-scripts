#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Illegal number of arguments"
    echo "Run as ./run.sh <first> <last>"
    exit 1
fi

rm -rf pokeapi.js types.js

tsc pokeapi.ts types.ts
node pokeapi.js $1 $2
