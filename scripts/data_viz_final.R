#Data Viz Final Project
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-05-02

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
#library(countcolors)

###############################################################################
#load in images and name them based on file path
my_path_photos <-"data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)

i <- 1

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_photos, folder_names,"/Final/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df <- data.frame(text = file_path) %>% 
    separate(text, sep = "/", 
             into = c("Data_raw", "Final_project", "Photos", "Date", "Stage", "Event")) %>% 
    separate(Event, sep = " ",
             into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
    separate(Suffix, sep = "_",
             into = c("PhotoID","Segmented","Suffix")) %>% 
    separate(Suffix, into = c("Cropped", "FileType")) %>% 
    mutate(FileType = toupper(FileType)) %>% 
    mutate(Date = parse_datetime(Date,
                                 format = "%B %d %Y"))
  file_add <- file_names_df %>% 
    select(-c("Data_raw", "Final_project", "Photos", "Stage", "PhotoID", "Segmented", "Cropped", "FileType"))
}

#create condensed version to add to color data, we don't need all this other stuff
file_add <- file_names_df %>% 
  select(-c("Data_raw", "Final_project", "Photos", "Stage", "PhotoID", "Segmented", "Cropped", "FileType"))

#Make a df of picture colors, include file info for the specific tree
#Make sure that row in file_add is equal to number in file path...
for(i in 1:length(file_path)) {
  pic_crop <- file_path[i]
  file_add_1 <- file_add[i,]
  tree <- data.frame(get_colors(pic_crop, exclude_col = "black", exclude_rad = 50))
  rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
  rownames(rgb) <- c(1:nrow(tree))
  tree_rgb <- cbind(file_add_1, rgb, tree)
}


################################################################################
# 
##load in pic_segmented_crop, for practice
# pic_crop <- "data_raw/final_project/Photos/August 26 2021/Final/PIPO10 Ambient+HW Drought DSC00273_segmented_crop.jpg"
# 
#
################################################################################
##explore colors, figure out which exclusion methods is best
#
##define colors (black, grey)
# grey <- gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)
# black1 <- rgb(0:40, 0, 0, maxColorValue = 255)
# black2 <- rgb(0, 0:40, 0, maxColorValue = 255)
# black3 <- rgb(0, 0, 0:40, maxColorValue = 255)
# black1_2 <- append(black1, black2)
# black <- append(black1_2, black3)
# black_grey <- append(black, grey)
#
# black.ctr <- c(0,0,0)
# black.radii <- 0.15
# black.select <- countcolors::countColors(pic_crop,
#                                          center = black.ctr,
#                                          radius = black.radii,
#                                          bg.lower = NULL,
#                                          plotting = TRUE)
#
# pic_crop %>%
#   get_colors(exclude_col = "black", exclude_rad = 50) %>%
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
#
#
################################################################################
# 
# tree <- data.frame(get_colors(file_path, exclude_col = "black"))
# 
# #add RGB data into data frame from the hex codes column
# rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
# rownames(rgb) <- c(1:nrow(tree))
# tree_rgb <- cbind(file_add, rgb, tree)
# 

################################################################################
#reducing the data frame

#bining
# 8x8x8 = 526
# 8 blocks for each (rgb), each 32 pixels wide

#bin red
tree_rgb <- tree_rgb %>% 
  mutate(red_class = ifelse(red <= 32, "1-32", 
                            ifelse(red <= 64, "33-64", 
                                   ifelse(red <= 96, "65-96",
                                          ifelse(red <= 128, "97-128",
                                                 ifelse(red <= 160, "129-160",
                                                        ifelse(red <= 192, "161-192",
                                                               ifelse(red <= 224, "193-224", "225-256"))))))))
#bin green
tree_rgb <- tree_rgb %>% 
  mutate(green_class = ifelse(green <= 32, "1-32", 
                              ifelse(green <= 64, "33-64", 
                                     ifelse(green <= 96, "65-96",
                                            ifelse(green <= 128, "97-128",
                                                   ifelse(green <= 160, "129-160",
                                                          ifelse(green <= 192, "161-192",
                                                                 ifelse(green <= 224, "193-224", "225-256"))))))))
#bin blue
tree_rgb <- tree_rgb %>% 
  mutate(blue_class = ifelse(blue <= 32, "1-32", 
                             ifelse(blue <= 64, "33-64", 
                                    ifelse(blue <= 96, "65-96",
                                           ifelse(blue <= 128, "97-128",
                                                  ifelse(blue <= 160, "129-160",
                                                         ifelse(blue <= 192, "161-192",
                                                                ifelse(blue <= 224, "193-224", "225-256"))))))))
#summarize by bins
tree_rgb_sum <- tree_rgb %>% 
  group_by(red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red)),
            green = round(mean(green)),
            blue = round(mean(blue)),
            col_freq = sum(col_freq))

#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = tree_rgb_sum$red, 
                             g = tree_rgb_sum$green, 
                             b = tree_rgb_sum$blue))
colnames(hex) <- "col_hex"
tree_rgb_sum <- cbind(tree_rgb_sum, hex)
#reorder columns
tree_rgb_sum <- tree_rgb_sum[, c(1,2,3,4,5,6,8,7)]
#add file ID to summary df
tree_rgb_sum <- cbind(file_add, tree_rgb_sum)
#calculate total # pixels and percent of each color, add to summary df
tree_rgb_sum <- tree_rgb_sum %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

################################################################################


#filter to exclude more grey and to most frequent colors
#grey colors: r=g=b
#here we can play around with thresholds for col_freq (# pixels in that bin)
tree_rgb_sum_filter <- tree_rgb_sum %>% 
  filter(!(red_class == green_class & red_class == blue_class)) %>% 
  filter(col_freq >= 1000) %>% 
  arrange(desc(col_share))

#remove some columns if you want
tree_rgb_sum_2 <- tree_rgb_sum %>% 
  select(-c(red_class, green_class, blue_class))

#colorfindr make_palette
tree_rgb_sum_filter %>% 
  make_palette(n = 3)

#visualize
colors <- tree_rgb_sum_filter$col_hex
as.double(tree_rgb_sum_filter$col_share)

tree_rgb_sum_filter %>% 
  ggplot(aes(x = SpeciesID,
             y = col_share,
             fill = col_hex)) +
  geom_col(fill = colors) +
  ylab("Color Percent") +
  xlab("August - PIPO30")

#try to find a way to arrange the y-axis by most frequent colors to least frequent


