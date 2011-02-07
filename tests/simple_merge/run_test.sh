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
    rm -f  f
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
cd ..

# Make a copy in "b" and record a change
mkdir b
cd b
"$CAMP" init
"$CAMP" pull ../a -a
printf 'line1\nline2\nbetween 2 and 3\nline3\n' > f
"$CAMP" record -a -m change23
cd ..

# Make a change in "a" too
cd a
printf 'line1\nbetween 1 and 2\nline2\nline3\n' > f
"$CAMP" record -a -m change12
cd ..

# Now pull in both directions, so both repos have both patches
cd a
"$CAMP" pull ../b -a
cd ..
cd b
"$CAMP" pull ../a -a
cd ..

# Now check that the file content is what we expect
printf 'line1\nbetween 1 and 2\nline2\nbetween 2 and 3\nline3\n' > f
diff -u a/f f
diff -u b/f f

