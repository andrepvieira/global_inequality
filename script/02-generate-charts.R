# clean up workspace

rm(list=ls())

# close all figure windows created with x11()

graphics.off()

# LOAD PACKAGES ----

library(tidyverse)
library(RColorBrewer)

# LOAD DATA ----

hum_right_clean_tbl <- readRDS("D:/global_inequality/data/hum_right_clean_tbl.rds")
v_dem_clean_tbl
wbl_clean_tbl
v_dem_gender_clean_tbl

# MAKE CHARTS ----

nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

hum_right_clean_tbl %>% 
      group_by(year, sub_region2) %>% 
      summarize(mean_human_rights_scores = mean(human_rights_scores, na.rm = T)) %>% 
      ungroup() %>% 
      # spread(year, mean_human_rights_scores) %>% 
      # add_row(sub_region2 = "World",
      #         `1970` = -0.3,
      #         `1980` = -0.5,
      #         `1990` = -0.1,
      #         `2000` = 0.4,
      #         `2010` = 0.5,
      #         `2017` = 0.8) %>% 
      # gather(year, mean_human_rights_scores, -sub_region2) %>% 
      # count(year)
      mutate(year = as.numeric(year)) %>% 
      ggplot(aes(year, mean_human_rights_scores, group = sub_region2, color = sub_region2)) +
      geom_line(size = 1) + 
      geom_point(size = 3) +
      # scale_y_continuous(limits = c(-3.8, 5.4)) +
      scale_y_continuous(limits = c(-3, 3)) +
      scale_x_continuous(limits = c(1970, 2017), breaks = c(1970, 1980, 1990, 2000, 2010, 2017)) +
      ggthemes::theme_gdocs() +
      # scale_color_brewer(palette = "Dark2") +
      scale_color_manual(values = mycolors) +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Scores of Human Rights’ Protection") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black"),
            legend.text = element_text(color = "black"),
            text = element_text(size = 19),
            legend.spacing.x = unit(1, 'cm'))+
      guides(color = guide_legend(label.position = "bottom", ncol = 3))
