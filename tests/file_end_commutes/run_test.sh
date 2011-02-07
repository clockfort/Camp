#!/bin/sh

# This test tests commutes of patches around the ends of files

set -e

HERE=`pwd`
CAMP="${CAMP:-$HERE/../../camp-bin/dist/build/camp/camp}"

cleanup() {
    cd "$HERE"
    rm -rf base
    rm -rf r123 r132 r213 r231 r312 r321
}

info() {
    if [ "$VERBOSE" -eq 1 ]
    then
        echo "$@"
    fi
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

VERBOSE=0
if [ "$1" = "v" ]
then
    VERBOSE=1
fi

# Set up the base repo
mkdir base
cd base
"$CAMP" init
printf 'line1\nline2\n' > f
"$CAMP" add f
"$CAMP" record -a -m o
printf 'start\nline1\nline2\n' > f
"$CAMP" record -a -m p1
printf 'start\nline1\nmiddle\nline2\n' > f
"$CAMP" record -a -m p2
printf 'start\nline1\nmiddle\nline2\nend\n' > f
"$CAMP" record -a -m p3
cd ..

# Now try pulling the patches in the 6 possible orders
info Doing 123
mkdir r123
cd r123
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p1
"$CAMP" pull ../base p2
"$CAMP" pull ../base p3
diff -u ../base/f f
cd ..

info Doing 132
mkdir r132
cd r132
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p1
"$CAMP" pull ../base p3
"$CAMP" pull ../base p2
diff -u ../base/f f
cd ..

info Doing 213
mkdir r213
cd r213
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p2
"$CAMP" pull ../base p1
"$CAMP" pull ../base p3
diff -u ../base/f f
cd ..

info Doing 231
mkdir r231
cd r231
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p2
"$CAMP" pull ../base p3
"$CAMP" pull ../base p1
diff -u ../base/f f
cd ..

info Doing 312
mkdir r312
cd r312
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p3
"$CAMP" pull ../base p1
"$CAMP" pull ../base p2
diff -u ../base/f f
cd ..

info Doing 321
mkdir r321
cd r321
"$CAMP" init
"$CAMP" pull ../base o
"$CAMP" pull ../base p3
"$CAMP" pull ../base p2
"$CAMP" pull ../base p1
diff -u ../base/f f
cd ..

