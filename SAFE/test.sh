#/usr/bin/bash

gf --run < tests/query.gfs | diff -u - tests/query.GOLD

gf --run < tests/negation.gfs | diff -u - tests/negation.GOLD

echo "If this is the first line you see, it means success!"

if [ $# -eq 0 ]
then
    echo ""
else
    echo "Short pieces"
    gf --run < tests/query.gfs

    echo ""
    echo "------"
    echo ""
    echo "Negations"
    gf --run < tests/negation.gfs
fi
