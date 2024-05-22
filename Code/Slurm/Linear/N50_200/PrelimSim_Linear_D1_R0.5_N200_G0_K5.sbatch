#!/bin/bash
#SBATCH --job-name PrelimSim_Linear_D1_R0.5_N200_G0_K5
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 2-00:00
#SBATCH --mem-per-cpu=6000
#SBATCH -o ClusterMessages/out/myscript_PrelimSim_Linear_D1_R0.5_N200_G0_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_PrelimSim_Linear_D1_R0.5_N200_G0_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 1 \
    --Rho 0 \
    --N 200 \
    --Gamma 0.5 \
    --K 5 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N50_200/sim_D1_R0.5_N200_G0_K5.rds \
