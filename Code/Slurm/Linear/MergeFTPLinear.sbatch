#!/bin/bash
#SBATCH --job-name MergeThetaPopLinear
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 1:00:00
#SBATCH --mem-per-cpu=300
#SBATCH -o ClusterMessages/out/myscript_MergeThetaPopLinear1_%j.out
#SBATCH -e ClusterMessages/error/myscript_MergeThetaPopLinear_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/MergeThetaPop.R \
    --TypeSetting 'Linear' \
