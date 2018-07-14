#!/bin/bash
set -e

cat ComputeLossHPC.txt | sed "s/MODEL/$1/g" | sed "s/TEST/$2/g" > ComputeLossHPC.${1}-${2}.txt

sbatch ComputeLossHPC.${1}-${2}.txt

rm ComputeLossHPC.${1}-${2}.txt

