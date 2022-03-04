
###   Read in data tables, models, function   ###


# Full tables:
fold <- read.csv("outputs/table_full_f.csv")
bind <- read.csv("outputs/table_full_b.csv")

# FoldX-only tables:
foldx <- read.csv("outputs/table_fxonly_f.csv")
bindx <- read.csv("outputs/table_fxonly_b.csv")

# selected models from stepwise & best subset selection
mods <- readRDS("outputs/models.rds")

# source LOsysOCV function
source("scripts/fn_loocv.R")


