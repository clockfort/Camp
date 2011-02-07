#!/bin/sh

set -e

CAMP="${CAMP:-`pwd`/../camp-bin/dist/build/camp/camp}"
export CAMP

FAILED=0
for TEST in */
do
    echo Running $TEST
    cd $TEST
    if ! sh run_test.sh
    then
        FAILED=$(($FAILED + 1))
    fi
    cd ..
done

echo
if [ $FAILED -eq 0 ]
then
    echo All tests successful
else
    echo $FAILED tests failed
    exit 1
fi

