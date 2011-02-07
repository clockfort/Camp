#!/bin/sh

# This tests that camp works in the second of two tests for a darcs bug,
# issue 1043. I'm not sure what broke exactly - conflict marking code?

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
printf 'original - apple\noriginal - banana\n' > f
"$CAMP" add f
"$CAMP" record -a -m base
cd ..

# Make a copy in "b" and record a change
mkdir b
cd b
"$CAMP" init
"$CAMP" pull ../a -a
cd ..

# Make a change in "a"
cd a
printf 'original - apple\nconflict 1 - brocolli\n' > f
"$CAMP" record -a -m brocolli
printf 'conflict 1 - artichoke\noriginal - banana\n' > f
"$CAMP" record -a -m artichoke
cd ..

# Make a change in "b"
cd b
printf 'conflict 2 - aardvark\noriginal - banana\nconflict 2 - cougar\n' > f
"$CAMP" record -a -m cougar
cd ..

# Merge and resolve in b
cd b
"$CAMP" pull ../a -a
printf 'resolution\noriginal - apple\noriginal - banana\n' > f
"$CAMP" record -a -m resolved
cd ..

# Make another change in "a"
cd a
printf 'original - apple\n' > f
"$CAMP" record -a -m apple
cd ..

# Merge in b
cd b
"$CAMP" pull ../a -a
cd ..

# Now check that the file content is what we expect
printf 'original - apple\noriginal - banana\n' > f
diff -u b/f f

