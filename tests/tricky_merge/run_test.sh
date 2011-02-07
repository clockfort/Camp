#!/bin/sh

# This test tests a case that caused darcs to give an "Inconsistent patch"
# error. http://bugs.darcs.net/issue1829

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

mkdir a
cd a
"$CAMP" init
echo Line BB    >  file
"$CAMP" add file
"$CAMP" rec -a -m "Main patch 1"
echo Line DDDD  >> file
"$CAMP" rec -a -m "Main patch 2"
echo Line A     >  file
echo Line BB    >> file
echo Line DDDD  >> file
"$CAMP" rec -a -m "Main patch 3"
echo Line A     >  file
echo Line BB    >> file
echo Line CCC   >> file
echo Line DDDD  >> file
"$CAMP" rec -a -m "Main patch 4"
echo Line A     >  file
echo Line BB    >> file
echo Line CCC   >> file
echo Line DDDD  >> file
echo Line EEEEE >> file
"$CAMP" rec -a -m "Main patch 5"
cd ..

mkdir b
cd b
"$CAMP" init
"$CAMP" pull ../a -a -p "Main patch 1"
echo Line TTTTTTT   >> file
"$CAMP" rec -a -m "Alternate patch 1"
"$CAMP" pull ../a -a -p "Main patch 2"
echo Line BB        >  file
echo Line XXXXXXXXX >> file
"$CAMP" rec -a -m "Alternate patch 2"
echo Line XXXXXXXXX >  file
"$CAMP" rec -a -m "Alternate patch 3"
cd ..

cd a
"$CAMP" pull ../b -ap "Alternate patch 1"
"$CAMP" pull ../b -ap "Alternate patch 2"
"$CAMP" pull ../b -ap "Alternate patch 3"
cd ..

