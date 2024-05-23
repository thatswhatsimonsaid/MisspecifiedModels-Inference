#!/bin/bash
#SBATCH --job-name Prelim_D1_G0.5_N2000_R0_K1.csv
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 12:00:00
#SBATCH --mem-per-cpu=2000
#SBATCH -o ClusterMessages/out/myscript_Prelim_D1_G0.5_N2000_R0_K1.csv_%j.out
#SBATCH -e ClusterMessages/error/myscript_Prelim_D1_G0.5_N2000_R0_K1.csv_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPop.R \
    --Delta 1 \
    --Gamma 0.5 \
    --N 2000 \
    --Rho 0 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output data/Linear/EstimatedThetaPop/D1_G0.5_N2000_R0_K1.csv \
