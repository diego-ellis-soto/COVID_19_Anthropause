#!/bin/bash

#SBATCH -t 12:00:00
#SBATCH --mail-type ALL
#SBATCH --mail-user scott.yanco@yale.edu
#SBATCH -c 12
#SBATHC --mem-per-cpu 25G
#SBATCH -J bears

cd ~/project/covid

module load miniconda
conda activate covid

Rscript ./src/par_cougar.r
