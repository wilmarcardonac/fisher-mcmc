#!/bin/sh
#SBATCH --partition=batch3
#SBATCH --cpus-per-task=12
#SBATCH -J MCMC-no-lensing
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 2-22:00:00
#SBATCH --mail-user=wilmar.cardona@csic.es
#SBATCH --mail-type=ALL
#SBATCH -o job-%J.out
#SBATCH --reservation=wcardona_6

l=`uname -n`
h=`squeue -w $l -O jobid -h`
srun ./fisher $h $1

