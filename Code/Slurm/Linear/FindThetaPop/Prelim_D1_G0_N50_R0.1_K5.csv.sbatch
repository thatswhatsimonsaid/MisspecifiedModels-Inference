#!/bin/bash
#SBATCH --job-name Prelim_D1_G0_N50_R0.1_K5.csv
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 1-00:00
#SBATCH --mem-per-cpu=1000
#SBATCH -o ClusterMessages/out/myscript_Prelim_D1_G0_N50_R0.1_K5.csv_%j.out
#SBATCH -e ClusterMessages/error/myscript_Prelim_D1_G0_N50_R0.1_K5.csv_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPop.R \
    --Delta 1 \
    --Gamma 0 \
    --N 50 \
    --Rho 0.1 \
    --K 5 \
    --TypeSetting 'Linear' \
    --Output data/Linear/EstimatedThetaPop/D1_G0_N50_R0.1_K5.csv \
