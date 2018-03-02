#!/bin/sh
#SBATCH --cpus-per-task=12
#SBATCH -J MCMC-only-auto-no-lensing
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 5-20:00:00
#SBATCH --mail-user=wilmar.cardona@csic.es
#SBATCH --mail-type=ALL
#SBATCH -o job-%J.out

l=`uname -n`
h=`squeue -w $l -O jobid -h`
srun ./fisher $h $1
