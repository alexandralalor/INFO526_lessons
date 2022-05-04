#Data Viz Final Project
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-05-02

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
library(countcolors) #for masking and reducing black colors to one point
library(tools) #for file naming

################################################################################
#file paths

#names_df_August_26_2021 <- read.csv("data_raw/final_project/file_names/names_df_August_26_2021.csv")
#names_df_November_11_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_11_2021.csv")
#names_df_November_19_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_19_2021.csv")
#names_df_November_5_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_5_2021.csv")
#names_df_October_15_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_15_2021.csv")
#names_df_October_21_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_21_2021.csv")
#names_df_October_29_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_29_2021.csv")
#names_df_October_7_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_7_2021.csv")
#names_df_September_16_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_16_2021.csv")
#names_df_September_2_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_2_2021.csv")
#names_df_September_24_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_24_2021.csv")
#names_df_September_30_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_30_2021.csv")
#names_df_September_9_2021 <-read.csv("data_raw/final_project/file_names/names_df_September_9_2021.csv")




#create condensed version to add to color data, we don't need all this other stuff
file_add <- names_df_September_9_2021 %>% 
  select(-c("Data_raw", "Final_project", "Photos", "Stage", "PhotoID", "Segmented", "Cropped", "FileType")) %>% 
  mutate(Date = parse_datetime(Date, format = "%B %d %Y"))

#define black color
black <- c(25, 25, 25)

#make empty files names df
tree_rgb <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(tree_rgb) <- c("Date", "SpeciesID", "Treatment_temp", "Treatment_water",
                        "red", "green", "blue",
                        "col_hex", "col_freq", "col_share")


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#file paths
#change date per iteration
my_path_photos <- "data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "data_raw/final_project/Photos/September 9 2021/Final/"
file_names_final <- list.files(my_path_final)
file_names <- paste0(my_path_final, file_names_final)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Make a df of picture colors, include file info for the specific tree
#Make sure that row in file_add is equal to number in file path...
for(i in 1:length(file_names)) {
  pic_crop <- file_names[i]
  file_add_1 <- file_add[i,]
  tree <- data.frame(get_colors(pic_crop, exclude_col = black, exclude_rad = 60))
  rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
  rownames(rgb) <- c(1:nrow(tree))
  tree_rgb_1 <- cbind(file_add_1, rgb, tree)
  tree_rgb <- rbind(tree_rgb, tree_rgb_1)
}

#check that all plants are added
summarize(tree_rgb, species = unique(SpeciesID))


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
  group_by(Date, SpeciesID, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
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
tree_rgb_sum <- tree_rgb_sum[, c(1,2,3,4,5,6,7,8,9,10,12,11)]
#calculate total # pixels and percent of each color, add to summary df
tree_rgb_sum <- tree_rgb_sum %>% 
  group_by(Date, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#here we can play around with thresholds for col_freq (# pixels in that bin)
tree_rgb_sum <- tree_rgb_sum %>% 
  filter(col_freq >= 100) %>% 
  arrange(desc(col_share))

#reality check
#each species should add up to 100%
tree_rgb_sum %>% 
  group_by(Date, SpeciesID) %>%
  summarize(percent = sum(col_share))

#save compressed file
write.csv(tree_rgb_sum, "data_raw/final_project/tree_rgb/tree_rgb_sum_September_9_2021.csv", quote=FALSE, row.names = FALSE)
