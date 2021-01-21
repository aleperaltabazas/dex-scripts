#!/bin/bash

if [ $# -eq 0 ]
  then
    echo "Error: Missing user and password"
    echo "Run like ./insert.sh <user> <password>"
fi

mongoimport --jsonArray --db dextracker --collection pokemon --file pokemon.json -u "$1" -p "$2"
