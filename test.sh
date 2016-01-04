#!/bin/sh
#SBATCH --cpus-per-task=16
#SBATCH --job-name=fisher-MCMC
#SBATCH --ntasks=1
#SBATCH --time=0-00:15:00
#SBATCH --mail-user=wilmar.cardona@unige.ch
#SBATCH --mail-type=ALL
#SBATCH --partition=debug
#SBATCH --output=slurm-%J.out

srun ./fisher