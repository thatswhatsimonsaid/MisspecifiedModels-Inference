#!/bin/bash
#SBATCH --job-name SufferingSim_Logistic_D0_G0.5_N2000_R0.1_K5.rds
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 1-00:00:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_SufferingSim_Logistic_D0_G0.5_N2000_R0.1_K5.rds_%j.out
#SBATCH -e ClusterMessages/error/myscript_SufferingSim_Logistic_D0_G0.5_N2000_R0.1_K5.rds_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 0 \
    --Rho 0.1 \
    --N 2000 \
    --Gamma 0.5 \
    --K 5 \
    --TypeSetting 'Logistic' \
    --Output Results/Logistic/N500_2000/sim_D0_G0.5_N2000_R0.1_K5.rds \
