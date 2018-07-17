#!/bin/bash
mkdir -p resources/cat resources/cat2
#for file in resources/Monsters/*.PNG; do
#  echo $file
#  bname=$(basename $file)
#  convert $file -transparent magenta temp.png &&
#  node node_modules/.bin/hqx temp.png 4 >| resources/cat/$bname
#done
i=0
find resources/cat/*.PNG |
xargs -n 32 echo | while read line; do
  i=$(($i + 1))
  is=$(printf "%02d" $i)
  echo $i
  convert $line +append resources/cat2/temp$is.png
done
convert resources/cat2/temp*.png -append tree.png
