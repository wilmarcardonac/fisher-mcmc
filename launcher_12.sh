#!/bin/sh
#SBATCH --cpus-per-task=12
#SBATCH --job-name=tophat-only-auto
#SBATCH --ntasks=1
#SBATCH --time=07-00:00:00
#SBATCH --mail-user=wilmar.cardona@unige.ch
#SBATCH --mail-type=ALL
#SBATCH --partition=dpt
#SBATCH --clusters=baobab
#SBATCH --output=slurm-%J.out

srun ./fisher