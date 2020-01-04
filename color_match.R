library(imager)
library(magick)
library(dplyr)
options(scipen = 999)

project_dir <- "C:/users/keith/projects/flags"

#Saved the CIA factbook flags page as a complete web page to fetch the flags to save time instead of scraping
webpage_docs <- file.path(project_dir, "References   Flags of the World - The World Factbook - Central Intelligence Agency_files")

flags <- grep("-flag.gif",list.files(webpage_docs), value = TRUE)

# read in the flags and determine how much of a flag is a given color
flag.color.percentage <- function(flag, flag_path = webpage_docs) {
  raw_flag <- image_read(file.path(flag_path, flag))
  raw_flag <- magick2cimg(raw_flag)
  flag_colors <- as.raster(raw_flag)
  colors_grouped <- as.data.frame(table(c(flag_colors)))
  colors_grouped <- colors_grouped %>% mutate(proportion = round(Freq / sum(Freq),4))
  colors_grouped_rank <- top_n(colors_grouped,5, wt = proportion)
  colors_grouped_rank <- arrange(colors_grouped_rank, -proportion)
  return(colors_grouped_rank)
  
}

all_flags <- vector(mode = "list", length = length(flags))

for( ii in 1:length(flags)) {
  all_flags[[ii]] <- flag.color.percentage(flags[[ii]])
}

#the scrambled flags
unknowns <- grep("flag_.\\.png",list.files(project_dir), value = TRUE)
test_flags <- vector(mode = "list", length = length(unknowns))

for( jj in 1:3) {
  test_flags[[jj]] <- flag.color.percentage(unknowns[[jj]], flag_path = project_dir)
}

#Unforunately the colors weren't an exact match. 
#I first looked at the scrambled flags and translated hex codes to colors to get a sense of what flags to look for.
#Then I had to scroll through and pick out likely candidates.
#These are what I picked as the most likely
all_flags[81] #France
test_flags[1]

all_flags[33] #Brazil
test_flags[2]

all_flags[242] #Namibia
test_flags[3]


