#!/bin/bash

# create info folder if not
mkdir -p info

# os.txt
if [ -z "$1" ]
then
# unix like
    echo -n "unx" > ./info/os.txt
else
# other
    echo -n $1 > ./info/os.txt
fi

# name.txt
name=`cat package.yaml | grep "name:" | awk -F' ' '{ OFS=""; $1=""; print }'`
echo -n $name > ./info/name.txt

# version.txt
version=`cat package.yaml | grep "version:" | awk -F' ' '{ OFS=""; $1=""; print }'`
echo -n $version > ./info/version.txt

# paths.txt
stack path > ./info/paths.txt

# docpath.txt
docpath=`cat ./info/paths.txt | grep local-doc-root | awk -F'local-doc-root: ' '{ OFS=""; $1=""; print }'`
if [ -z "$1" ]
then
# unix like
docpath=$docpath'/'$name'-'$version
else
# other
docpath=$docpath'\'$name'-'$version
fi
echo -n $docpath > ./info/docpath.txt

# docindexpath.txt
if [ -z "$1" ]
then
# unix like
docpathindex=$docpath'/index.html'
else
# other
docpath=$docpath'\'$name'-'$version
fi
echo -n $docpathindex > ./info/docpathindex.txt
