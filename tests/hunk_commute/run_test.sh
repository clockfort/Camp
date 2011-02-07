#!/bin/sh

# This test tests the basic functionality, i.e.:
# * Initialising a repo works
# * Adding a file works
# * Recording a patch works
# * Pulling from a repo works
# * Trivial non-conflicting merges work

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

# Set up the base repo in "a"
mkdir a
cd a
"$CAMP" init
printf 'line1\nline2\nline3\n' > f
"$CAMP" add f
"$CAMP" record -a -m base
printf 'line1\nx1\nx2\nline2\nline3\n' > f
"$CAMP" record -a -m x
printf 'line1\nx1\nx2\nline2\ny1\n2y\ny3\ny4\nline3\n' > f
"$CAMP" record -a -m y
cd ..

mkdir b
cd b
"$CAMP" init
"$CAMP" pull ../a base
"$CAMP" pull ../a y
"$CAMP" pull ../a x
cd ..

mkdir c
cd c
"$CAMP" init
"$CAMP" pull ../b base
"$CAMP" pull ../b x
"$CAMP" pull ../b y
cd ..

# Now check that the file content is what we expect
diff -u a/f b/f
diff -u a/f c/f

