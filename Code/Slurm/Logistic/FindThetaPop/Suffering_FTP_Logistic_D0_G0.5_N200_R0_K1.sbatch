#!/bin/bash
#SBATCH --job-name Suffering_FTP_Logistic_D0_G0.5_N200_R0_K1
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 12:00:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_Suffering_FTP_Logistic_D0_G0.5_N200_R0_K1_%j.out
#SBATCH -e ClusterMessages/error/myscript_Suffering_FTP_Logistic_D0_G0.5_N200_R0_K1_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPop.R \
    --Delta 0 \
    --Gamma 0.5 \
    --N 200 \
    --Rho 0 \
    --K 1 \
    --TypeSetting 'Logistic' \
    --Output data/Logistic_D0_G0.5_N200_R0_K1.csv \
