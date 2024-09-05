#!/bin/bash
#SBATCH --job-name MergeThetaPopLogistic
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 1:00:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_MergeThetaPopLogistic1_%j.out
#SBATCH -e ClusterMessages/error/myscript_MergeThetaPopLogistic_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/MergeThetaPop.R \
    --TypeSetting 'Logistic' \
