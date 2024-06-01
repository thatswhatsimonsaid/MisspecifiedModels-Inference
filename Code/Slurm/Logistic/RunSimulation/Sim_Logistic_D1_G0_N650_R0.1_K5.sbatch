#!/bin/bash
#SBATCH --job-name Sim_Logistic_D1_G0_N650_R0.1_K5
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 7-00:00:00
#SBATCH --mem-per-cpu=6000
#SBATCH -o ClusterMessages/out/myscript_Sim_Logistic_D1_G0_N650_R0.1_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_Sim_Logistic_D1_G0_N650_R0.1_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudy.R \
    --Delta 1 \
    --Rho 0.1 \
    --N 650 \
    --Gamma 0 \
    --K 5 \
    --TypeSetting 'Logistic' \
    --Output Results/Logistic/LargeSampleExtension/sim_D1_G0_N650_R0.1_K5.rds \
