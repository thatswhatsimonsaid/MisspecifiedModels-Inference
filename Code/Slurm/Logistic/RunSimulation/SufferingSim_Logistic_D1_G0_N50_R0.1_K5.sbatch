#!/bin/bash
#SBATCH --job-name SufferingSim_Logistic_D1_G0_N50_R0.1_K5
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 7-00:00:00
#SBATCH --mem-per-cpu=6000
#SBATCH -o ClusterMessages/out/myscript_SufferingSim_Logistic_D1_G0_N50_R0.1_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_SufferingSim_Logistic_D1_G0_N50_R0.1_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudy.R \
    --Delta 1 \
    --Rho 0.1 \
    --N 50 \
    --Gamma 0 \
    --K 5 \
    --TypeSetting 'Logistic' \
    --Output Results/Logistic/N50_200/sim_D1_G0_N50_R0.1_K5.rds \
