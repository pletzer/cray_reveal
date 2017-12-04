#!/bin/bash

#SBATCH --partition=Debug
#SBATCH --time=00:10:00

export PAT_RT_SUMMARY=0
time srun upwind+apa 256 256 256

