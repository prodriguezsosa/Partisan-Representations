#!/bin/bash
set -e

cat W2VKerasHPC.txt | sed "s/SOURCE/$1/g" | sed "s/FOLD/$2/g" > W2VKerasHPC.${1}-${2}.txt

sbatch W2VKerasHPC.${1}-${2}.txt

rm W2VKerasHPC.${1}-${2}.txt

