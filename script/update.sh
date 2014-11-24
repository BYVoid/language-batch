#!/bin/bash
bin=dist/build/batch-beautifier/batch-beautifier
inputDir=test/testcase
for filename in ${inputDir}/*.bat; do
  caseName=$(basename -s .bat $filename)
  echo $caseName
  input=${inputDir}/${caseName}.bat
  output=${inputDir}/out/${caseName}.bat
  $bin $input $output --ast --tokens
done
