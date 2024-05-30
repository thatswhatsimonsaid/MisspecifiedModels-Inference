#!/bin/bash
#SBATCH --job-name SimResultsSmallLinear_D1_R0.1_N50_G0_K1
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 2-00:00
#SBATCH --mem-per-cpu=6000
#SBATCH -o ClusterMessages/out/myscript_SimResultsSmallLinear_D1_R0.1_N50_G0_K1_%j.out
#SBATCH -e ClusterMessages/error/myscript_SimResultsSmallLinear_D1_R0.1_N50_G0_K1_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 1 \
    --Rho 0.1 \
    --N 50 \
    --Gamma 0 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N50_200/_D1_R0.1_N50_G0_K1.rds \
