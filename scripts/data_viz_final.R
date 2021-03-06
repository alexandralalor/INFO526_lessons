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
# This section might need to be done first. cropped pictures in Crop folder, then masked and put into Final folder

#color definition!

# #set black upper and lower bounds
# black.lower <- c(0,0,0)
# black.upper <- c(0.2, 0.2, 0.2)
# 
# #naming!
# #adjust file path for each date
# my_path_photos <- "data_raw/final_project/Photos/"
# folder_names_list <- list.files(my_path_photos)
# my_path_final <- "data_raw/final_project/Photos/August 26 2021/Final/"
# my_path_crop <- "data_raw/final_project/Photos/August 26 2021/Crop/"
# file_names_crop_jpg <- list.files(my_path_crop)
# file_names_crop <- file_path_sans_ext(file_names_crop_jpg)
# file_names_final <- paste0(file_names_crop,"_mask",".jpg")
# 
# 
# #for loop! go through every folder, and for every file in the folder, count colors and save to new folder
# 
# #list file names in each folder and add to date frame
# for(i in 1:length(file_names_crop_jpg)) {
#   file_name <- file_names_crop_jpg[i]
#   file_name_new <- file_names_final[i]
#   pic <- paste0(my_path_crop, file_name)
#   file_path_final <- paste0(my_path_final, file_name_new)
#   countcolors::countColors(pic, color.range="rectangular",
#                            upper = black.upper,
#                            lower = black.lower,
#                            target.color=c("black"),
#                            save.indicator = file_path_final)
}


# #look at 3D graph again, make sure black color is reduced to single point
# pic_crop <- "data_raw/final_project/Photos/August 26 2021/Crop/PIPO49 Ambient Drought DSC00295_segmented_crop.jpg"
# pic_mask <- "data_raw/final_project/Photos/August 26 2021/Final/PIPO49 Ambient Drought DSC00295_segmented_crop_mask.png"
# 
# black <- c(25, 25, 25)
# 
# pic_crop %>%
#   get_colors(exclude_col = black, exclude_rad = 60) %>%
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 
# 
# pic_mask %>%
#   get_colors(exclude_col = "black") %>%
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 


###############################################################################
#load in images and name them based on file path
my_path_photos <-"data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)

#make empty files names df
file_names_df <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(file_names_df) <- c("Data_raw", "Final_project", "Photos", "Date", "Stage", 
                             "SpeciesID","Treatment_temp","Treatment_water","PhotoID",
                             "Segmented", "Cropped", "FileType")

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_photos, folder_names,"/Final/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(text = file_path) %>% 
    separate(text, sep = "/", 
             into = c("Data_raw", "Final_project", "Photos", "Date", "Stage", "Event")) %>% 
    separate(Event, sep = " ",
             into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
    separate(Suffix, sep = "_",
             into = c("PhotoID","Segmented", "Suffix")) %>% 
    separate(Suffix, into = c("Cropped", "FileType")) %>% 
    mutate(FileType = toupper(FileType))
    # mutate(Date = parse_datetime(Date,
    #                              format = "%B %d %Y"))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}

file_names_df %>% 
  summarize(date = unique(Date))

names_df_August_26_2021 <- file_names_df %>% 
  filter(Date == "August 26 2021")
#write.csv(names_df_August_26_2021, "data_raw/final_project/file_names/names_df_August_26_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_11_2021 <- file_names_df %>% 
  filter(Date == "November 11 2021")
#write.csv(names_df_November_11_2021, "data_raw/final_project/file_names/names_df_November_11_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_19_2021 <- file_names_df %>% 
  filter(Date == "November 19 2021")
#write.csv(names_df_November_19_2021, "data_raw/final_project/file_names/names_df_November_19_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_5_2021 <- file_names_df %>% 
  filter(Date == "November 5 2021")
#write.csv(names_df_November_5_2021, "data_raw/final_project/file_names/names_df_November_5_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_15_2021 <- file_names_df %>% 
  filter(Date == "October 15 2021")
#write.csv(names_df_October_15_2021, "data_raw/final_project/file_names/names_df_October_15_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_21_2021 <- file_names_df %>% 
  filter(Date == "October 21 2021")
#write.csv(names_df_October_21_2021, "data_raw/final_project/file_names/names_df_October_21_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_29_2021 <- file_names_df %>% 
  filter(Date == "October 29 2021")
#write.csv(names_df_October_29_2021, "data_raw/final_project/file_names/names_df_October_29_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_7_2021 <- file_names_df %>% 
  filter(Date == "October 7 2021")
#write.csv(names_df_October_7_2021, "data_raw/final_project/file_names/names_df_October_7_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_16_2021 <- file_names_df %>% 
  filter(Date == "September 16 2021")
#write.csv(names_df_September_16_2021, "data_raw/final_project/file_names/names_df_September_16_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_2_2021 <- file_names_df %>% 
  filter(Date == "September 2 2021")
#write.csv(names_df_September_2_2021, "data_raw/final_project/file_names/names_df_September_2_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_24_2021 <- file_names_df %>% 
  filter(Date == "September 24 2021")
#write.csv(names_df_September_24_2021, "data_raw/final_project/file_names/names_df_September_24_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_30_2021 <- file_names_df %>% 
  filter(Date == "September 30 2021")
#write.csv(names_df_September_30_2021, "data_raw/final_project/file_names/names_df_September_30_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_9_2021 <- file_names_df %>% 
  filter(Date == "September 9 2021")
#write.csv(names_df_September_9_2021, "data_raw/final_project/file_names/names_df_September_9_2021.csv", quote=FALSE, row.names = FALSE)


################################################################################
#file paths
#change date per iteration
my_path_photos <- "data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "data_raw/final_project/Photos/August 26 2021/Final/"
file_names <- list.files(my_path_final)

names_df_August_26_2021 <- read.csv("data_raw/final_project/file_names/names_df_August_26_2021.csv")
names_df_November_11_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_11_2021.csv")
names_df_November_19_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_19_2021.csv")
names_df_November_5_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_5_2021.csv")
names_df_October_15_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_15_2021.csv")
names_df_October_21_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_21_2021.csv")
names_df_October_29_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_29_2021.csv")
names_df_October_7_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_7_2021.csv")
names_df_September_16_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_16_2021.csv")
names_df_September_2_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_2_2021.csv")
names_df_September_24_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_24_2021.csv")
names_df_September_30_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_30_2021.csv")
names_df_September_9_2021 <-read.csv("data_raw/final_project/file_names/names_df_September_9_2021.csv")




#create condensed version to add to color data, we don't need all this other stuff
file_add <- names_df_August_26_2021 %>% 
  select(-c("Data_raw", "Final_project", "Photos", "Stage", "PhotoID", "Segmented", "Cropped", "FileType")) %>% 
  mutate(Date = parse_datetime(Date, format = "%B %d %Y"))

#define black color
black <- c(25, 25, 25)

#make empty files names df
tree_rgb <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(tree_rgb) <- c("Date", "SpeciesID", "Treatment_temp", "Treatment_water",
                             "red", "green", "blue",
                             "col_hex", "col_freq", "col_share")

#file paths
#change date per iteration
my_path_photos <- "data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "data_raw/final_project/Photos/August 26 2021/Final/"
file_names_final <- list.files(my_path_final)
file_names <- paste0(my_path_final, file_names_final)


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
#add file ID to summary df
tree_rgb_sum <- cbind(file_add, tree_rgb_sum)


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
#pixels added together should be roughly the same
tree_rgb_sum %>% 
  group_by(Date, SpeciesID) %>%
  summarize(pixels = mean(col_total))

#here we can play around with thresholds for col_freq (# pixels in that bin)
tree_rgb_sum <- tree_rgb_sum %>% 
  filter(col_freq >= 100) %>% 
  arrange(desc(col_share))


write.csv(tree_rgb_sum, "data_raw/final_project/tree_rgb/tree_rgb_sum_August_26_2021.csv", quote=FALSE, row.names = FALSE)

################################################################################
#Filter and visualize

#read in df_rgb_sum
tree_rgb_sum_August_26_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_sum_August_26_2021.csv")
tree_rgb_sum_November_11_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_sum_November_11_2021.csv")
tree_rgb_sum <- tree_rgb_sum_November_11_2021

#filter to exclude more grey and to most frequent colors

tree_rgb_sum_filter %>% 
  group_by(Date, SpeciesID) %>% 
  filter(col_share > 10) %>% 
  make_palette()

tree_rgb_sum_filter <- tree_rgb_sum %>% 
  filter(col_hex != "#707150") %>% 
  filter(col_hex != "#909170") %>% 
  filter(col_hex != "#4f5131") %>% 
  filter(col_hex != "#505130") %>% 
  filter(col_hex != "#707150") %>% 
  filter(col_hex != "#8f9270") %>% 
  filter(col_hex != "#505130") %>% 
  filter(col_hex != "#707250") %>%
  filter(col_hex != "#707151") %>%
  filter(col_hex != "#8e9370") %>%
  filter(col_hex != "#505230") %>% 
  filter(col_hex != "#8e9270") %>%
  filter(col_hex != "#4f5230") %>% 
  filter(col_hex != "#8f9071")


#grey colors: r=g=b
#there seems to be a good threshold of green under 150 t0 180 which filters out
#most grey, I just have to figure out how to make this into a list

tree_rgb_sum_filter <- tree_rgb_sum %>% 
  group_by(Date, SpeciesID) %>% 
  filter(green < 180) %>% 
  filter(!(red_class == green_class & red_class == blue_class))

tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
  group_by(Date, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))


tree_rgb_sum_filter <- tree_rgb_sum %>% 
  filter(col_hex != "#525649") %>% 
  filter(col_hex != "#323629") %>% 
  filter(col_hex != "#555749") %>% 
  filter(col_hex != "#545649") %>% 
  filter(col_hex != "#565749") %>% 
  filter(col_hex != "#333429") %>% 
  filter(col_hex != "#555549") %>% 
  filter(col_hex != "#323429") %>% 
  filter(col_hex != "#343429") %>% 
  filter(col_hex != "#333529") %>% 
  filter(col_hex != "#747669") %>% 
  filter(col_hex != "#727669") %>% 
  filter(col_hex != "#737669") %>% 
  filter(col_hex != "#747769") %>% 
  filter(col_hex != "#757669") %>% 
  filter(col_hex != "#545749") %>% 
  filter(col_hex != "#333629") %>% 
  filter(col_hex != "#929789") %>%
  filter(col_hex != "#929689") %>%
  filter(col_hex != "#343529") %>%
  filter(col_hex != "#939689") %>%
  filter(col_hex != "#949689") %>%
  filter(col_hex != "#939789") %>%
  filter(col_hex != "#949789") %>% 
  filter(col_hex != "#505130") %>%
  filter(col_hex != "#949789") %>% 
  filter(col_hex != "#707150")

#remove some columns if you want
# tree_rgb_sum_2 <- tree_rgb_sum %>% 
#   select(-c(red_class, green_class, blue_class))

#colorfindr make_palette
tree_rgb_sum_filter %>% 
  make_palette(n = 3)

#visualize
colors <- tree_rgb_sum$col_hex
#as.double(tree_rgb_sum_filter$col_share)

tree_rgb_sum %>% 
  ggplot(aes(x = SpeciesID,
             y = col_share,
             fill = col_hex)) +
  geom_col(fill = colors) +
  ylab("Color Percent") +
  xlab("")

#try to find a way to arrange the y-axis by most frequent colors to least frequent


