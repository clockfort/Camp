#!/bin/sh

# In this test, we get 2 patches in sequence which commute, but we
# make them conflictors with the same patch and in current camp they
# no longer commute.

set -e

HERE=`pwd`
CAMP="${CAMP:-$HERE/../../camp-bin/dist/build/camp/camp}"

cleanup() {
    cd "$HERE"
    rm -rf a
    rm -rf b
    rm -rf c
}

cleanup

if [ "$1" = "clean-only" ]
then
    exit 0
fi

if [ "$1" != "no-clean-after" ]
then
    trap cleanup EXIT
fi

mkdir a
cd a
"$CAMP" init
printf 'A\nB\nC\nD\n' > file
"$CAMP" add file
"$CAMP" rec -a -m "Initial patch"
cd ..

"$CAMP" -v0 get a b
"$CAMP" -v0 get a c

cd a
printf 'A\nX\nB\nC\nD\n' > file
"$CAMP" rec -a -m "Add X"
printf 'A\nX\nB\nY\nC\nD\n' > file
"$CAMP" rec -a -m "Add Y"
printf 'A\nX\nB\nY\nC\nZ\nD\n' > file
"$CAMP" rec -a -m "Add Z"
cd ..

cd b
printf 'RRR' > file
"$CAMP" rec -a -m "Rewrite with R"
"$CAMP" pull ../a -ap "Add X"
"$CAMP" pull ../a -ap "Add Y"
"$CAMP" pull ../a -ap "Add Z"
cd ..

cd c
"$CAMP" pull ../b -ap "Add Z"
cd ..

