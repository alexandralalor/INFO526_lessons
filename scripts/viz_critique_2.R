#viz critique 2
#Alexandra Lalor
#allielalor@email.arizona.edu
#first created: 2022-05-01
#last updated: 2022-05-01

##############################################################
# Figure 2 / UPDATE
##############################################################

#load packages
library(tidyverse)
library(ggpubr)

# code from...
# https://zenodo.org/record/3833928#.Ym9XNNrMI2w

# description of Policies...
# https://asanchez-tojar.github.io/code_in_ecology/supporting_information.html

# Based on the information collected, we scored the journals’ code-sharing policies as:
#‘encouraged’ (publication of the code is explicitly encouraged, but not required), 
#‘mandatory’ (code must be published together with the article), or
#‘encouraged/mandatory’ (when the wording made it difficult to judge if code 
# publication is encouraged or required).

#read csv
full.journal.info <- read_csv("data_raw/viz_critique/code_availability.csv")

#group and summarize
full.journal.info <- full.journal.info %>% 
  mutate(Policy = ifelse(Policy == "Encouraged/Mandatory", "Encouraged", 
                              ifelse(Policy == "Encouraged", "Encouraged", "Mandatory")))
code_availability <- full.journal.info %>% 
  group_by(Policy) %>%
  summarize(total_comb = sum(total),
            codepublished_comb = sum(codepublished),
            percentage_comb = round((codepublished_comb/total_comb)*100))


class(code_availability$percentage_comb)

code_availability %>%
  ggdotchart(y = "Policy", x = "percentage_comb",
             color = "Policy",
             palette = c("#00AFBB", "#E7B800"),
             sorting = "descending",
             group = "Policy",
             scale_x_continuous(limits = c(0,30)))
             xlab = "",
             ylab = "Percentage (%) of articles publishing some code",
             dot.size = 10,
             label = paste0(round(code_availability$codepublished_comb,0)," / ",
                            round(code_availability$total_comb,0)),
             font.label = list(color = "black", size = 10,vjust = -2.5),
             ggtheme = theme_pubr())


code_availability %>%
  ggdotchart(x = "Policy", y = "percentage_comb",
             color = "Policy",
             palette = c("#00AFBB", "#E7B800"),
             sorting = "descending",
             rotate = TRUE,
             xlim = 40,
             add = "segments",
             group = "Policy",
             xlab = "",
             ylab = "Percentage (%) of articles publishing some code",
             dot.size = 10,
             label = paste0(round(code_availability$codepublished_comb,0)," / ",
                            round(code_availability$total_comb,0)),
             font.label = list(color = "black", size = 10,vjust = -2.5),
             ggtheme = theme_pubr())

orientation = c("vertical", "horizontal", "reverse")
?desc_statby
?add
# creating figure 2: ORIGINAL
figure2 <- ggdotchart(full.journal.info, x = "abbreviations", y = "percentage",
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

figure2 <- ggpar(figure2,legend.title = "Code-sharing policy")
