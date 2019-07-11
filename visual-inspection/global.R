# Drag and drop images code from Dr. Geovany A. Ramirez, April 28, 2019
# Modified by XXX 06 May 2019

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

in_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/Code/visual-inspection"
out_path <- "/media/owner/MYIOPSITTA/R/Uruguay2017_MonkParakeet_CallAnalysis/Code/visual-inspection/classifications"

ccs_social_scales_meta <- readRDS(file.path(in_path, "ccs_social_scales_meta.RDS"))

# get a random sample of the unique social scales
# since this is in global.R, it's run only once when the app starts
social_scales <- sample(unique(ccs_social_scales_meta$scale), replace = FALSE)

# give the name (without extension) of the .csv files that should never be removed from in_path
do_not_remove <- "images"

is_logged <- FALSE

# function to check login information, parts may not be necessary since not planning to use sqlite
check_login_credentials <- function(username, password, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  if(password == "mnk") {
    TRUE
    } else {
      FALSE
  }
}
