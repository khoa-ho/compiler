#!/bin/bash

echo "Normal cases:"

for source_path in `ls test/*.src`; do
  source_file=$(basename $source_path)
  testname=${source_file%.*}
  outfile=${source_path%.*}.out
  lexfile=${source_path%.*}.lex.out
  parsefile=${source_path%.*}.parse.out  

  if [ -e $outfile ]; then
    if ! diff <(./compiler.native $source_path) $outfile; then
      >&2 echo "Failed test '${testname}'!"
      exit 1
    fi
  else
    >&2 echo "Can't find '${outfile}'"
    exit 1
  fi

    if [ -e $lexfile ]; then
    if ! diff <(./compiler.native -lex $source_path) $lexfile; then
      >&2 echo "Failed test '${testname}'!"
      exit 1
    fi
  else
    >&2 echo "Can't find '${lexfile}'"
    exit 1
  fi

    if [ -e $parsefile ]; then
    if ! diff <(./compiler.native -parse $source_path) $parsefile; then
      >&2 echo "Failed test '${testname}'!"
      exit 1
    fi
  else
    >&2 echo "Can't find '${parse}'"
    exit 1
  fi
done
echo "All tests passed!"


echo "Error cases:"

for source_path in `ls test/*.err`; do
  source_file=$(basename $source_path)
  testname=${source_file%.*}

  ./compiler.native $source_path &> /dev/null
  if [ $? != 1 ]; then
    >&2 echo "Failed to handle '${testname}' error properly!"
    exit 1
  fi
done
echo "All tests passed!"
