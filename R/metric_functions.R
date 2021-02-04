# This script contains functions which uses the PSUT matrices
# in the PSUT_useful target to calculate the following metrics:
# Total energy supply (TES) of primary energy
# Total final consumption (TFC) of final and useful energy
# Primary-Final, Final-Useful, and Primary-Final efficiencies

# Loads required packages
library(matsbyname)
library(matsindf)
library(tidyverse)
library(IEATools)
