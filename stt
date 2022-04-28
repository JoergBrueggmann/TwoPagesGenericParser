#!/bin/bash

# Builds the test executable, executes the tests
# test results will be available in test/TestResult.txt
# coverage report will be available under .stack-work/install/<nnnnnnnn>/hpc/index.html

echo "build and execute test"

./info.sh $1

name=`cat ./info/name.txt`

echo "application:"$name

rm -v "./"$name".cabal"
stack test --coverage > "./test/TestResult.txt"
cat "./test/TestResult.txt"
