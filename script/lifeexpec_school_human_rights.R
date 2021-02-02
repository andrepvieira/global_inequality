library(tidyverse)

code <- read_csv("./data/cnt-code.csv")
lifexpec <- read_csv("./data/life-expectancy.csv")
yrs_school <- read_csv("./data/mean-years-of-schooling-1.csv")
hum_right <- read_csv("./data/human-rights-scores.csv")

child_mortal <- read_csv("./data/child-mortality-igme.csv") # Para teste

code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

# Child mortality ----

child_mortal %>% 
      janitor::clean_names() %>% 
      rename(mortal_rate = mortality_rate_under_5_per_1_000_live_births) %>% 
      # mutate(mortal_rate = mortal_rate/10) %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      # filter(year == 2017) %>%
      # filter(entity == "World")
      filter(!is.na(region)) %>% 
      # filter(!is.na(region)) %>%
      group_by(year, region) %>% 
      summarize(mean_mortal_rate = mean(mortal_rate, na.rm = T)) %>% 
      spread(year, mean_mortal_rate) %>% 
      mutate(abs_change = `2017`-`1970`,
             rel_change = round(100*((`2017`-`1970`)/`1970`), 1)
             # mean_world_70 = mean(`1970`),
             # mean_world_17 = mean(`2017`),
             # mean_world_abs_change = mean_world_17 - mean_world_70,
             # mean_world_rel_change = round(100*((mean_world_17-mean_world_70)/mean_world_17), 1),
             ) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/tab_child_mortal.xlsx", row.names = F, sheetName = "bruto")
      
child_mortal %>% 
      janitor::clean_names() %>% 
      rename(mortal_rate = mortality_rate_under_5_per_1_000_live_births) %>% 
      # mutate(mortal_rate = mortal_rate/10) %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      # filter(year == 2017) %>%
      # filter(entity == "World")
      filter(!is.na(region)) %>% 
      # filter(!is.na(region)) %>%
      group_by(year, region) %>% 
      summarize(mean_mortal_rate = mean(mortal_rate, na.rm = T)) %>% 
      ggplot(aes(year, mean_mortal_rate, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 250)) +
      scale_x_continuous(limits = c(1970, 2017), breaks = c(1970, 1980, 1990, 2000, 2010, 2017)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Taxa de mortalidade infantil") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            text = element_text(size = 17),
            legend.spacing.x = unit(1, 'cm'))+
      guides(color = guide_legend(label.position = "bottom", ncol = 6))
      
# Years of schooling ----

yrs_school %>% 
      janitor::clean_names() %>% 
      rename(avg_yrs_school = average_total_years_of_schooling_for_adult_population_lee_lee_2016_barro_lee_2018_and_undp_2018) %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(year, region) %>% 
      summarize(mean_avg_yrs_school = mean(avg_yrs_school, na.rm = T)) %>% 
      spread(year, mean_avg_yrs_school) %>% 
      mutate(abs_change = `2017`-`1970`,
             rel_change = round(100*((`2017`-`1970`)/`1970`), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/tab_yrs_school.xlsx", row.names = F, sheetName = "bruto")

yrs_school %>% 
      janitor::clean_names() %>% 
      rename(avg_yrs_school = average_total_years_of_schooling_for_adult_population_lee_lee_2016_barro_lee_2018_and_undp_2018) %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(year, region) %>% 
      summarize(mean_avg_yrs_school = mean(avg_yrs_school, na.rm = T)) %>%
      # group_by(year) %>%
      # mutate(avg_world = mean(mean_avg_yrs_school)) %>% 
      # distinct(avg_world, .keep_all = T) %>% 
      # data.frame
      spread(year, mean_avg_yrs_school) %>% 
      add_row(region = "World",
              `1970` = 4.4,
              `1980` = 5.3,
              `1990` = 6.4,
              `2000` = 7.5,
              `2010` = 8.5,
              `2017` = 9.1) %>% 
      gather(year, mean_avg_yrs_school, -region) %>% 
      mutate(year = as.numeric(year)) %>% 
      ggplot(aes(year, mean_avg_yrs_school, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 12)) +
      scale_x_continuous(limits = c(1970, 2017), breaks = c(1970, 1980, 1990, 2000, 2010, 2017)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Anos de escolarização da população adulta") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            text = element_text(size = 19),
            legend.spacing.x = unit(1, 'cm'))+
      guides(color = guide_legend(label.position = "bottom", ncol = 6))


# Human rights ----

hum_right %>% 
      janitor::clean_names() %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(year, region) %>% 
      summarize(mean_human_rights_scores = mean(human_rights_scores, na.rm = T)) %>% 
      spread(year, mean_human_rights_scores) %>% 
      mutate(abs_change = `2017`-`1970`,
             rel_change = round(100*((`2017`-`1970`)/`1970`), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/tab_human_rights.xlsx", row.names = F, sheetName = "bruto")

hum_right %>% 
      janitor::clean_names() %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(entity == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
   group_by(year, region) %>% 
   summarize(mean_human_rights_scores = mean(human_rights_scores, na.rm = T)) %>% 
   ungroup() %>% 
   spread(year, mean_human_rights_scores) %>% 
   add_row(region = "World",
           `1970` = -0.3,
           `1980` = -0.5,
           `1990` = -0.1,
           `2000` = 0.4,
           `2010` = 0.5,
           `2017` = 0.8) %>% 
   gather(year, mean_human_rights_scores, -region) %>% 
   mutate(year = as.numeric(year)) %>% 
   ggplot(aes(year, mean_human_rights_scores, group = region, color = region)) +
   geom_line(size = 1) + 
   geom_point(size = 3) +
   scale_y_continuous(limits = c(-3.8, 5.4)) +
   scale_x_continuous(limits = c(1970, 2017), breaks = c(1970, 1980, 1990, 2000, 2010, 2017)) +
   ggthemes::theme_gdocs() +
   scale_color_brewer(palette = "Dark2") +
   # viridis::scale_color_viridis() + 
   labs(x = "", y = "Scores de proteção aos direitos humanos") +
   theme(legend.position = "bottom",
         legend.title = element_blank(),
         legend.direction = "horizontal",
         text = element_text(size = 19),
         legend.spacing.x = unit(1, 'cm'))+
   guides(color = guide_legend(label.position = "bottom", ncol = 6))


# Life expectancy

lifexpec %>% 
      janitor::clean_names() %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>% 
      left_join(code) %>% 
      group_by(year, region) %>% 
      summarize(mean_life_expec = mean(life_expectancy, na.rm = T)) %>% 
      na.omit %>% 
      ggplot(aes(year, mean_life_expec, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 2) +
      scale_y_continuous(limits = c(0, 90)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Set1")
      
      

