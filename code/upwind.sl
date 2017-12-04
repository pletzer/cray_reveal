#!/bin/bash

#SBATCH --partition=Debug
#SBATCH --time=00:10:00

time srun upwind 256 256 256

