#!/bin/bash

# Creates the documentation from source comments

echo "build documentation"

./info.sh $1

docpath=`cat ./info/docpath.txt`
echo 'docpath:"'$docpath'"'
mkdir -p -v $docpath
cp -v -r ./media $docpath

name=`cat ./info/name.txt`

rm -v "./"$name".cabal"
stack haddock --open $name
