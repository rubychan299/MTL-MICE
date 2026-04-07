#!/bin/sh

#SBATCH --job-name=k_1se_1
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=12GB
#SBATCH --cpus-per-task=5
#SBATCH --array=1-100
#SBATCH --account=torch_pr_365_general

export OMP_NUM_THREADS=1
module purge
module load r/4.5.1
R CMD BATCH --no-save --no-restore k_1se_1.R script_$SLURM_ARRAY_TASK_ID