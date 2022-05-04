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


#you can split up the df to keep things organized by date...

names_df_August_26_2021 <- file_names_df %>% 
  filter(Date == "August 26 2021")
write.csv(names_df_August_26_2021, "data_raw/final_project/file_names/names_df_August_26_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_11_2021 <- file_names_df %>% 
  filter(Date == "November 11 2021")
write.csv(names_df_November_11_2021, "data_raw/final_project/file_names/names_df_November_11_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_19_2021 <- file_names_df %>% 
  filter(Date == "November 19 2021")
write.csv(names_df_November_19_2021, "data_raw/final_project/file_names/names_df_November_19_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_5_2021 <- file_names_df %>% 
  filter(Date == "November 5 2021")
write.csv(names_df_November_5_2021, "data_raw/final_project/file_names/names_df_November_5_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_15_2021 <- file_names_df %>% 
  filter(Date == "October 15 2021")
write.csv(names_df_October_15_2021, "data_raw/final_project/file_names/names_df_October_15_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_21_2021 <- file_names_df %>% 
  filter(Date == "October 21 2021")
write.csv(names_df_October_21_2021, "data_raw/final_project/file_names/names_df_October_21_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_29_2021 <- file_names_df %>% 
  filter(Date == "October 29 2021")
write.csv(names_df_October_29_2021, "data_raw/final_project/file_names/names_df_October_29_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_7_2021 <- file_names_df %>% 
  filter(Date == "October 7 2021")
write.csv(names_df_October_7_2021, "data_raw/final_project/file_names/names_df_October_7_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_16_2021 <- file_names_df %>% 
  filter(Date == "September 16 2021")
write.csv(names_df_September_16_2021, "data_raw/final_project/file_names/names_df_September_16_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_2_2021 <- file_names_df %>% 
  filter(Date == "September 2 2021")
write.csv(names_df_September_2_2021, "data_raw/final_project/file_names/names_df_September_2_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_24_2021 <- file_names_df %>% 
  filter(Date == "September 24 2021")
write.csv(names_df_September_24_2021, "data_raw/final_project/file_names/names_df_September_24_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_30_2021 <- file_names_df %>% 
  filter(Date == "September 30 2021")
write.csv(names_df_September_30_2021, "data_raw/final_project/file_names/names_df_September_30_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_9_2021 <- file_names_df %>% 
  filter(Date == "September 9 2021")
write.csv(names_df_September_9_2021, "data_raw/final_project/file_names/names_df_September_9_2021.csv", quote=FALSE, row.names = FALSE)

