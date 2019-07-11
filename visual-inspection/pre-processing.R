# XXX
# 11 April 2019

# Purpose: To collect data on visual classification of monk parakeet contact calls across the individual, pair, flock and site social scales. We will assess interobserver reliability of call classifications by visual inspection, and compare to classification reults based on spectrographic cross-correlation (SPCC) and random forests (RF) acoustic similarity.

# General Approach: Modify an existing Shiny app that presents human observers with multiple pages of spectrograms and collects observers' classification data. 

# Pre-processing: Select calls for 4 groups for each social scale. These groups will be randomly chosen with the exception of the individual scale. At the individual scale, we are restricted to using the 4 repeatedly sampled individuals used for random forests model validation. The overall aim is to compare human classification with classification based on SPCC and random forests. We must therefore use the same "test" data sets for classification across approaches per social scale, so using the random forests training data set for human visual classification at the individual scale is not useful. 

# Random sampling and classes: Human visual classification will be blind not only to spectrogram labels but also to social scales, to ensure fair comparison to the other classification methods. Also ensure that not only are spectrograms per social scale randomly sampled prior to being presented to each observer, but also that social scales (one page per social scale) are randomly presented as well. Each classes within each social scale should contain the same number of signals as all other classes.

############################### Load libraries #########################

rm(list = ls())

X <- c("warbleR", "pbapply", "tidyverse", "magrittr")

invisible(lapply(X, library, character.only = TRUE))

# use Projects instead of setting working directories, which automatically sets the working directory to there
# then set in and out path variables and work from there
# see package here
ipath <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/Data/Focal_Individuals"
spath <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/Data/Site_Recordings"

out_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/Data/visual-inspection/www"

############################### Read in data ###########################

# read in repeatedly sampled individual data set
ccs_fi <- read.csv(file.path(ipath, "focal_individual_sels_preprocessed.csv"), header = TRUE)
str(ccs_fi)

# read in site-level data set
ccs <- read.csv(file.path(spath, "site-level_calls_preprocessed_final.csv"), header = TRUE)
str(ccs)

# read in pair and flock IDs generated in Supplementary Methods 1
pair_IDs <- readRDS(file.path(spath, "pair_IDs.RDS"))
str(pair_IDs) # corresponds to IDs in the Pair_ID column

flock_IDs <- readRDS(file.path(spath, "flock_IDs.RDS"))
str(flock_IDs) # corresponds to IDs in the Flock_ID column

# initialize cores and random seed 
cores <- parallel::detectCores() - 2
seed <- 401

########################### Individual scale ##########################

# initialize individuals used for random forests model validation
shiny_indivs <- c("UM3", "UM5", "RAW", "ZW8")

ccs_fi_shiny <- droplevels(ccs_fi[grep(paste(paste("^", shiny_indivs, "$", sep = ""), collapse = "|"), ccs_fi$indiv), ])
str(ccs_fi_shiny)

# stick to 4 calls per individual here (the lowest number across these 4 birds)
table(ccs_fi_shiny$indiv)

# randomly sample 4 calls per individual
indices <- unlist(tapply(seq(1, nrow(ccs_fi_shiny), 1), as.character(ccs_fi_shiny$indiv), FUN = sample, 4, replace = FALSE))

ccs_fi_shiny <- ccs_fi_shiny[indices, ]
str(ccs_fi_shiny)

# process the selection table data frame for ease of parsing in global.R
ccs_fi_shiny_meta <- ccs_fi_shiny %>%
  as_tibble() %>%
  
  # select the most important columns
  select(
    sound.files, selec, start, end, indiv
  ) %>%
  
  # rename the individual identity column 
  rename(ID = indiv) %>%
  
  # add a column for the social scale
  add_column(scale = "Individual") %>%
  
  droplevels()

str(ccs_fi_shiny_meta) # looks great

# convert to an extended selection table for ease of sharing and replicability data used for the Shiny app
indiv_shiny_tbl <- selection_table(X = ccs_fi_shiny_meta, whole.recs = FALSE, extended = TRUE, confirm.extended = TRUE, mar = 0.1, parallel = cores, path = ipath)
# str(indiv_shiny_tbl)

# write out extended selection table as an RDS object
saveRDS(indiv_shiny_tbl, file.path(path = out_path, "indiv_scale_shiny_extended_seltable.RDS"))

########################### Pair scale ################################

# randomly sample 4 pairs
set.seed(seed)
rsamp_pairs <- sample(pair_IDs, size = 4, replace = FALSE)
rsamp_pairs

# subset by calls for the randomly sampled pairs
ccs_pairs_shiny <- droplevels(ccs[grep(paste(paste("^", rsamp_pairs, "$", sep = ""), collapse = "|"), ccs$Pair_ID), ])
str(ccs_pairs_shiny)

# check that pair IDs are indeed correct
unique(ccs_pairs_shiny$Pair_ID) %in% rsamp_pairs

# process the selection table data frame for ease of parsing in global.R
ccs_pairs_shiny_meta <- ccs_pairs_shiny %>%
  as_tibble() %>%
  
  # select the most important columns
  select(
    sound.files, selec, start, end, Pair_ID
  ) %>%
  
  # rename the individual identity column 
  rename(ID = Pair_ID) %>%
  
  # add a column for the social scale
  add_column(scale = "Pair") %>%
  
  droplevels()
  
  # add column of call order 
  # add_column(call_order = seq(1, nrow(ccs_pairs_shiny), 1))

str(ccs_pairs_shiny_meta) # looks great

# convert to an extended selection table for ease of sharing and replicability data used for the Shiny app
pair_shiny_tbl <- selection_table(X = ccs_pairs_shiny_meta, whole.recs = FALSE, extended = TRUE, confirm.extended = TRUE, mar = 0.1, parallel = cores, path = spath)
# str(pair_shiny_tbl)

# # write out extended selection table as an RDS object
saveRDS(pair_shiny_tbl, file.path(path = out_path, "pair_scale_shiny_extended_seltable.RDS"))

########################### Flock scale ################################

# repeat the process above for the flock scale

# check the number of calls per flock
table(ccs$Flock_ID)

# proceed by obtaining flocks with 3 calls
flock_IDs <- names(table(ccs$Flock_ID))[which(table(ccs$Flock_ID) == 3)]

# randomly sample 4 flocks among these
set.seed(seed)
rsamp_flocks <- sample(flock_IDs, size = 4, replace = FALSE)
rsamp_flocks

# subset by calls for the randomly sampled flocks
ccs_flocks_shiny <- droplevels(ccs[grep(paste(paste("^", rsamp_flocks, "$", sep = ""), collapse = "|"), ccs$Flock_ID), ])
str(ccs_flocks_shiny)

# check that flock IDs are indeed correct
unique(ccs_flocks_shiny$Flock_ID) %in% rsamp_flocks

# check that there are indeed 3 calls per flock
table(ccs_flocks_shiny$Flock_ID)

# process the selection table data frame for ease of parsing in global.R
ccs_flocks_shiny_meta <- ccs_flocks_shiny %>%
  as_tibble() %>%
  
  # select the most important columns
  select(
    sound.files, selec, start, end, Flock_ID
  ) %>%
  
  # rename the individual identity column 
  rename(ID = Flock_ID) %>%
  
  # add a column for the social scale
  add_column(scale = "Flock") %>%
  
  droplevels()
  
  # add column of call order 
  # add_column(call_order = seq(1, nrow(ccs_flocks_shiny), 1))

str(ccs_flocks_shiny_meta) # looks great

# convert to an extended selection table for ease of sharing and replicability data used for the Shiny app
flock_shiny_tbl <- selection_table(X = ccs_flocks_shiny_meta, whole.recs = FALSE, extended = TRUE, confirm.extended = TRUE, mar = 0.1, parallel = cores, path = spath)
str(flock_shiny_tbl)

# write out extended selection table as an RDS object
saveRDS(flock_shiny_tbl, file.path(out_path, "flock_scale_shiny_extended_seltable.RDS"))

########################### Site scale ################################

# GSV CONTINUE

# repeat the process above for the site scale
ccs <- read.csv(file.path(spath, "site-level_calls_preprocessed_final.csv"), header = TRUE)
# str(ccs)
# nrow(ccs)

# also exclude calls used for pairs and flocks
ccs <- ccs[!grepl(paste(paste("^", rsamp_pairs, "$", sep = ""), collapse = "|"), ccs$Pair_ID), ]
nrow(ccs)

ccs <- ccs[!grepl(paste(paste("^", rsamp_flocks, "$", sep = ""), collapse = "|"), ccs$Flock_ID), ]
nrow(ccs)

# filter by sites that have the highest mean SNR
mean_SNR <- tapply(ccs$SNR, ccs$General_site, mean)[order(tapply(ccs$SNR, ccs$General_site, mean), decreasing = TRUE)]

# take the top half sites with the highest SNR
sites <- names(mean_SNR[order(mean_SNR, decreasing = TRUE)][1:(length(mean_SNR)/2)])

# randomly sample these sites
set.seed(seed)
rsamp_sites <- sample(sites, size = 4, replace = FALSE)
rsamp_sites

# subset by calls for the randomly sampled sites
ccs_sites_shiny <- droplevels(ccs[grep(paste(paste("^", rsamp_sites, "$", sep = ""), collapse = "|"), ccs$General_site), ])
str(ccs_sites_shiny)
unique(ccs_sites_shiny$General_site)

# per site, randomly sample 4 calls
set.seed(seed)
tapply(ccs_sites_shiny$Row, ccs_sites_shiny$General_site, sample, size = 4, replace = FALSE)

set.seed(seed)
rsamp_calls <- as.vector(unlist(tapply(ccs_sites_shiny$Row, ccs_sites_shiny$General_site, sample, size = 4, replace = FALSE)))

# subset by the randomly sampled calls within these sites
ccs_sites_shiny2 <- droplevels(ccs_sites_shiny[grep(paste(paste("^", rsamp_calls, "$", sep = ""), collapse = "|"), ccs_sites_shiny$Row), ])
str(ccs_sites_shiny2)

# check that site IDs are indeed correct
unique(ccs_sites_shiny2$Row) %in% rsamp_calls

# process the selection table data frame for ease of parsing in global.R
ccs_sites_shiny_meta <- ccs_sites_shiny2 %>%
  as_tibble() %>%
  
  # select the most important columns
  select(
    sound.files, selec, start, end, General_site
  ) %>%
  
  # rename the individual identity column 
  rename(ID = General_site) %>%
  
  # add a column for the social scale
  add_column(scale = "Site") %>%
  
  droplevels()

str(ccs_sites_shiny_meta) # looks great

# convert to an extended selection table for ease of sharing and replicability data used for the Shiny app
site_shiny_tbl <- selection_table(X = ccs_sites_shiny_meta, whole.recs = FALSE, extended = TRUE, confirm.extended = TRUE, mar = 0.1, parallel = cores, path = spath)
str(site_shiny_tbl)

# # write out extended selection table as an RDS object
saveRDS(site_shiny_tbl, file.path(path = out_path, "site_scale_shiny_extended_seltable.RDS"))

##################### Combine selection tables #########################

# combine the selection table data frames per social scale into a single data frame for easier parsing in global.R

ccs_fi_shiny_meta[, c("sound.files", "ID")] %<>% lapply(function(x) as.character(x))
str(ccs_fi_shiny_meta)

ccs_pairs_shiny_meta[, c("sound.files", "ID")] %<>% lapply(function(x) as.character(x))
str(ccs_pairs_shiny_meta)

ccs_flocks_shiny_meta[, c("sound.files", "ID")] %<>% lapply(function(x) as.character(x))
str(ccs_flocks_shiny_meta)

ccs_sites_shiny_meta[, c("sound.files", "ID")] %<>% lapply(function(x) as.character(x))
str(ccs_sites_shiny_meta)

ccs_social_scales_meta <- bind_rows(ccs_fi_shiny_meta, ccs_pairs_shiny_meta, ccs_flocks_shiny_meta, ccs_sites_shiny_meta)
str(ccs_social_scales_meta)

# generate a column of unique call IDs
ccs_social_scales_meta$spectrogram_ID <- sapply(1:nrow(ccs_social_scales_meta), function(x){
  
  paste(ccs_social_scales_meta$sound.files[x], "_", ccs_social_scales_meta$selec[x], "-", 1, ".jpeg", sep = "")
  
})
str(ccs_social_scales_meta)

# make a numeric column for social scale to facilitate ordering in global.R
ccs_social_scales_meta$social_scale_num <- as.numeric(factor(ccs_social_scales_meta$scale, levels = unique(ccs_social_scales_meta$scale)))

saveRDS(ccs_social_scales_meta, "ccs_social_scales_meta.RDS")

##################### Generate spectrograms ############################

# these need to be generated without any information about social scale or bird identity to facilitate blind classification

ccs_social_scales_meta <- readRDS("ccs_social_scales_meta.RDS") 

# read in the extended selection tables after moving them into the Shiny app working directory
indiv_shiny_tbl <- readRDS(file.path(path = out_path,"indiv_scale_shiny_extended_seltable.RDS"))
pair_shiny_tbl <- readRDS(file.path(path = out_path,"pair_scale_shiny_extended_seltable.RDS"))
flock_shiny_tbl <- readRDS(file.path(path = out_path,"flock_scale_shiny_extended_seltable.RDS"))
site_shiny_tbl <- readRDS(file.path(path = out_path,"site_scale_shiny_extended_seltable.RDS"))

# iterate over extended selection tables to make spectrograms for each social scale using the same parameters as for SPCC and random forests
# make spectrograms without labels (including axes) for image feature extraction by WND-CHRM, which takes only tiff or ppm files
# moved image files into ~/img_processing manually
call_list <- list(indiv_shiny_tbl, pair_shiny_tbl, flock_shiny_tbl, site_shiny_tbl)

out_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/visual-inspection/images"

# x <- 1
invisible(pblapply(1:length(unique(ccs_social_scales_meta$scale)), function(x){
  
  specreator(call_list[[x]], wn = "hanning", wl = 378, collev = seq(-53, 0, 1), flim = c(0.5, 9), ovlp = 90, line = FALSE, mar = 0.01, title = FALSE, res = 200, axisX = FALSE, axisY = FALSE, inner.mar = c(0,0,0,0), it = "jpeg", path = out_path)
  
}))

# also generate spectrograms with axes for general instructions in the app
# will choose just one for the app
out_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/visual-inspection/www"

specreator(call_list[[4]], wn = "hanning", wl = 378, collev = seq(-53, 0, 1), flim = c(0.5, 9), ovlp = 90, line = FALSE, mar = 0.01, title = FALSE, res = 200, axisX = TRUE, axisY = TRUE, it = "jpeg", sc = TRUE, path = out_path)

# write the image files as a single column .csv
ccs_social_scales_meta <- readRDS("ccs_social_scales_meta.RDS") 
str(ccs_social_scales_meta)

names(ccs_social_scales_meta) <- gsub("spectrogram_ID", "images", names(ccs_social_scales_meta))

out_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/visual-inspection"

write.csv(ccs_social_scales_meta, file.path(out_path, "images.csv"), row.names = FALSE)
