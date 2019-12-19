#! /bin/sh
for i in `ls $1/*.gz`
do
    zgrep "<AbstractText" $i | sed -e 's/<[^>]*>//g' > $i.abstr 
done
