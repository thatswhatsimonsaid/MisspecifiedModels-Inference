#!/bin/bash
#SBATCH --job-name SimResultsLargeLinear_D1_G0.5_N500_R0_K1
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 2-00:00:00
#SBATCH --mem-per-cpu=10000
#SBATCH -o ClusterMessages/out/myscript_SimResultsLargeLinear_D1_G0.5_N500_R0_K1_%j.out
#SBATCH -e ClusterMessages/error/myscript_SimResultsLargeLinear_D1_G0.5_N500_R0_K1_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 1 \
    --Rho 0 \
    --N 500 \
    --Gamma 0.5 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N500_2000/D1_G0.5_N500_R0_K1.rds \
