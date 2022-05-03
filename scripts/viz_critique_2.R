#viz critique 2
#Alexandra Lalor
#allielalor@email.arizona.edu
#first created: 2022-05-01
#last updated: 2022-05-01

##############################################################
# Figure 2 / UPDATE
##############################################################


# code from...
# https://zenodo.org/record/3833928#.Ym9XNNrMI2w

# description of Policies...
# https://asanchez-tojar.github.io/code_in_ecology/supporting_information.html

# The authors randomly sampled articles published in ecological journals for which code-sharing 
# has been either mandatory or encouraged since June 2015 at the latest.
# A total of 14 out of 96 (15%) ecological journals encouraged code-sharing policies
# A random sample of 400 articles was taken from all the articles published in these
# 14 journals. Articles were further screened for relevancy, leaving 346 studies in this review.

# Based on the information collected, we scored the journals’ code-sharing policies as:
#‘encouraged’ (publication of the code is explicitly encouraged, but not required), 
#‘mandatory’ (code must be published together with the article), or
#‘encouraged/mandatory’ (when the wording made it difficult to judge if code 
# publication is encouraged or required).


#load packages
library(tidyverse)
library(ggpubr)

#read csv
full.journal.info <- read_csv("data_raw/viz_critique/code_availability.csv")

#mutate policy column, add count column, merge
code_availability_1 <- full.journal.info %>% 
  mutate(Policy_sort = ifelse(Policy == "Encouraged/Mandatory", "Encouraged_total", 
                              ifelse(Policy == "Encouraged", "Encouraged_total", "Mandatory_total"))) %>% 
  mutate(count = total) %>% 
  mutate(Policy = "Total") %>% 
  mutate(percentage = NA)

code_availability_2 <- full.journal.info %>% 
  mutate(Policy_sort = ifelse(Policy == "Encouraged/Mandatory", "Encouraged_published", 
                         ifelse(Policy == "Encouraged", "Encouraged_published", "Mandatory_published"))) %>% 
  mutate(count = codepublished) %>% 
  mutate(Policy = ifelse(Policy_sort == "Encouraged_published", "Encouraged", "Mandatory"))

code_availability <- rbind(code_availability_1,code_availability_2)


#visualize
code_availability %>%
  ggdotchart(x = "abbreviations", 
             y = "count",
             color = "Policy",
             palette = c("#00AFBB", "#E7B800", "#D3D3D3"),
             sorting = "descending",
             rotate = TRUE,
             group = "Policy",
             add = "segments",
             title = "Ecology journals with code-sharing policies still have low availability of code",
             xlab = "",
             ylab = "Number of articles with published code (color) of those reviewed (grey)",
             dot.size = 8,
             label = paste0(code_availability$percentage,"%"),
             label.select = list(criteria = "Policy %in% c('Encouraged','Mandatory')"),
             font.label = list(color = "black", size = 7,vjust = 0.5),
             ggtheme = theme_pubr())



# creating figure 2: ORIGINAL
ggdotchart(full.journal.info, x = "abbreviations", y = "percentage",
                      color = "Policy",
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                      sorting = "descending",
                      add = "segments",
                      rotate = TRUE,
                      group = "Policy",
                      xlab = "",
                      ylab = "Percentage (%) of articles publishing some code",
                      dot.size = 8,
                      label = paste0(round(full.journal.info$codepublished,0),"/",
                                     round(full.journal.info$total,0)),
                      font.label = list(color = "black", size = 7,vjust = 0.5),
                      ggtheme = theme_pubr())

