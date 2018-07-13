#!/bin/bash
set -e

cat ComputeLossFoldSpecificHPC.txt | sed "s/MODEL/$1/g" | sed "s/TEST/$2/g" | sed "s/FOLDM/$3/g" | sed "s/FOLDT/$4/g" > ComputeLossFoldSpecificHPC.${1}-${2}-${3}-${4}.txt

sbatch ComputeLossFoldSpecificHPC.${1}-${2}-${3}-${4}.txt

rm ComputeLossFoldSpecificHPC.${1}-${2}-${3}-${4}.txt

