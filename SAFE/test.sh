#/usr/bin/bash

#gf --run < tests/query.gfs | diff -u - tests/query.GOLD

#echo "If this is the first line you see, it means success!"

echo "Short pieces"
gf --run < tests/query.gfs

echo ""
echo "------"
echo ""
echo "Negations"
gf --run < tests/negation.gfs
