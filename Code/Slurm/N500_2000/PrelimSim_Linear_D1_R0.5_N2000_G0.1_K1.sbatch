#!/bin/bash
#SBATCH --job-name PrelimSim_Linear_D1_R0.5_N2000_G0.1_K1
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 2-00:00
#SBATCH --mem-per-cpu=6000
#SBATCH -o ClusterMessages/out/myscript_PrelimSim_Linear_D1_R0.5_N2000_G0.1_K1_%j.out
#SBATCH -e ClusterMessages/error/myscript_PrelimSim_Linear_D1_R0.5_N2000_G0.1_K1_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 1 \
    --Rho 0.5 \
    --N 2000 \
    --Gamma 0.1 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N500_2000/sim_D1_R0.5_N2000_G0.1_K1.rds \
