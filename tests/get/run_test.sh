#!/bin/sh

# This test tests that the "get" command works

set -e

HERE=`pwd`
CAMP="${CAMP:-$HERE/../../camp-bin/dist/build/camp/camp}"

cleanup() {
    cd "$HERE"
    rm -rf a
    rm -rf b
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
printf 'This is the file\n' > f
"$CAMP" add f
"$CAMP" record -a -m base
printf 'Change the file\n' > f
"$CAMP" record -a -m changed
cd ..

"$CAMP" -v0 get a b

# Now check that the file content is what we expect
diff -u a/f b/f

