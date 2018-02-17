#!/bin/bash

echo "Normal cases:"
num_file=0

for source_path in `ls test/*.src`; do
  source_file=$(basename $source_path)
  test_name=${source_file%.*}
  out_file=${source_path%.*}.out

  if [ -e $out_file ]; then
    if ! diff <(./compiler.native $source_path) $out_file; then
      >&2 echo "Failed test '${test_name}'!"
      exit 1
    fi
  else
    >&2 echo "Can't find '${out_file
}'"
    exit 1
  fi
  let num_file++
done
echo "${num_file} tests passed!"


echo "Error cases:"
num_file=0

for source_path in `ls test/*.err`; do
  source_file=$(basename $source_path)
  test_name=${source_file%.*}

  ./compiler.native $source_path &> /dev/null
  if [ $? == 0 ]; then
    >&2 echo "Failed test '${test_name}'!"
    exit 1
  fi
  let num_file++
done
echo "${num_file} tests passed!"
