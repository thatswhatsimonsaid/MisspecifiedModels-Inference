# Read CSV file
rm(list=ls())
dir = "/Users/simondn/Documents/Stats572"
typeSetting= "Logistic"
ParameterVector <- read.csv(paste0(dir,"/data/",typeSetting,"/Parameters/ParameterVectorFindThetaPop.csv"))

# Loop through each row
for (i in 1:nrow(ParameterVector)) {
  # Extract values from the current row
  job_name <- ParameterVector[i, "JobName"]
  misspec_vec <- ParameterVector[i, "MisspecVec"]
  homosked_vec <- ParameterVector[i, "HomoskedVec"]
  size_vec <- ParameterVector[i, "SizeVec"]
  leverage_vec <- ParameterVector[i, "LeverageVec"]
  k_vec <- ParameterVector[i, "KVec"]
  type_setting <- ParameterVector[i, "TypeSetting"]
  output <- ParameterVector[i, "Output"]

  # Create .sbatch file for the current simulation
  sbatch_file <- file(paste0(dir,"/Code/Slurm/",typeSetting,"/FindThetaPop/",job_name, ".sbatch"), "w")
  writeLines(
    c(
      "#!/bin/bash",
      paste("#SBATCH --job-name", job_name),
      "#SBATCH --partition short",
      "#SBATCH --ntasks 1",
      "#SBATCH --time 1-00:00",
      "#SBATCH --mem-per-cpu=300",
      paste("#SBATCH -o ClusterMessages/out/myscript_", job_name, "_%j.out", sep=""),
      paste("#SBATCH -e ClusterMessages/error/myscript_", job_name, "_%j.err", sep=""),
      "#SBATCH --mail-type=ALL",
      "#SBATCH --mail-user=simondn@uw.edu",
      "",
      "cd ~/Stats572",
      "module load R",
      "Rscript Code/FindThetaPop.R \\",
      paste("    --Delta ", misspec_vec, " \\", sep=""),
      paste("    --Rho ", homosked_vec, " \\", sep=""),
      paste("    --N ", size_vec, " \\", sep=""),
      paste("    --Gamma ", leverage_vec, " \\", sep=""),
      paste("    --K ", k_vec, " \\", sep=""),
      paste("    --TypeSetting '", type_setting, "' \\", sep=""),
      paste("    --Output ", output, " \\", sep="")
    ),
    con=sbatch_file
  )
  close(sbatch_file)
}

print("Sbatch files generated successfully.")
