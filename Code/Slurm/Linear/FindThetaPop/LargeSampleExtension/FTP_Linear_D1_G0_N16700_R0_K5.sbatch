#!/bin/bash
#SBATCH --job-name FTP_Linear_D1_G0_N16700_R0_K5
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 12:00:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_FTP_Linear_D1_G0_N16700_R0_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_FTP_Linear_D1_G0_N16700_R0_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPop.R \
    --Delta 1 \
    --Gamma 0 \
    --N 16700 \
    --Rho 0 \
    --K 5 \
    --TypeSetting 'Linear' \
    --Output data/Linear/EstimatedThetaPop/D1_G0_N16700_R0_K5.csv \
