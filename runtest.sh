#!/bin/bash

echo "Normal cases:"
for sourcepath in `ls test/*.src`;
do
  sourcefile=$(basename $sourcepath)
  testname=${sourcefile%.*}
  outfile=${sourcepath%.*}.out

  if [ -e $outfile ]
  then
    diff <(./compiler.native $sourcepath) $outfile || (>&2 echo "Failed test '${testname}'!"; exit 1)
  else
      (>&2 echo "Can't find '${outfile}'"; exit 1)
  fi
  echo "Pass '${testname}' test!"
done