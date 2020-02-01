rm(list=ls())

# Load libraries
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(microplot)
library(RColorBrewer)
library(viridis)
library(wesanderson)

# Load functions
source("R/setup_matrix.R")
source("R/setup_guide_df.R")
source("R/join_matrices.R")
source("R/plot_mitred_square_pattern.R")

#----- Plot examples -----------------------------------------------------------

# Built in patterns include:
# - stripe
# - corner
# - circle
# - diagonal_cross
# - diagonal_diamond
# - checkered

# Using own colour palette
plot_mitred_square_pattern(colour_palette = c("#1942C0", "#447198", "#70A070", "#9BCF48", "#C7FF21", "#FFF621", "#DFC62F", "#C0973E", "#A0674D", "#81385C"),
                           pattern="stripe")

# Using brewer palettes
plot_mitred_square_pattern(colour_palette = brewer.pal(name="PiYG", n=10),
                           pattern="corner")

plot_mitred_square_pattern(colour_palette = brewer.pal(name="Spectral", n=10),
                           pattern="circle")

# Using viridis palettes
plot_mitred_square_pattern(colour_palette = viridis(10),
                           pattern="diagonal_cross")

plot_mitred_square_pattern(colour_palette = inferno(10),
                           pattern="diagonal_diamond")

# Using wes anderson palettes
plot_mitred_square_pattern(colour_palette = wes_palette("Moonrise1", 10, type="continuous"),
                           pattern="checkered")
