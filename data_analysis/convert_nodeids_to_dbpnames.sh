#! /bin/bash

echo "Usage: convert_nodeids_to_dbpnames.sh <similarities_file.gz> <node_alphabet_file.gz>";
echo "Similarity input file (format: nodeid nodeid sim): $1"
echo "DBpedia Edge alphabet (format: nodeid dbpname): $2"

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters! Existing script";
    exit 1;
fi

OUTFILE=$1.readable.gz
ALPHABET_TMP=alphabet_dbpedia.tmp
SF1=similarities_file_1.tmp

# Process nodes alphabet file
pigz -cd $2 | \
sed 's/ <=> / /g' | \
sort -S 80% -s -k1,1 > \
$ALPHABET_TMP;
echo "Node alphabet sorted"

# Process similarities file
pigz -cd $1 | \
sort -S 80% -s -k2,2 > \
$SF1;

join -1 2 -2 1 --check-order $SF1 $ALPHABET_TMP > $OUTFILE;
rm $SF1;
cp $OUTFILE $SF1;
join -1 2 -2 1 --check-order <(sort -S 80% -s -k2,2 $SF1) $ALPHABET_TMP | \
awk '{print $1,$2,$3,$5,$4}' | \
sort -S 80% -s -k3,3n | \
pigz > $OUTFILE;

# Cleaning up
rm $SF1 $SF2 $ALPHABET_TMP;
