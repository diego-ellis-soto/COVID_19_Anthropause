#!/bin/bash

#SBATCH -t 12:00:00
#SBATCH --mail-type ALL
#SBATCH --mail-user scott.yanco@yale.edu
#SBATCH -c 8
#SBATHC --mem-per-cpu 25G
#SBATCH -J cougars ftw

cd ~/project/covid

Rscript ./src/par_cougar.r