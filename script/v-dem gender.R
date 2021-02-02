# CARREGA PACOTE ----
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

# CARREGA DADOS ----

v_dem <- readRDS("./tabelas/V-DEM/V-Dem-CY-Full+Others-v10.rds")
code <- read_csv("./tabelas/cnt-code.csv")

# TRANSFORMA DADOS ----

code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

v_dem %>% 
      tibble() %>% 
      select(country_name, code = country_text_id, year, historical_date, codingstart,
             v2pepwrort_osp) %>%
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(country_name == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(year, region) %>% 
      summarize(mean_power_sex = mean(v2pepwrort_osp, na.rm = T)) %>% 
      # gather(variable, value, -year, -region) %>% 
      spread(year, mean_power_sex) %>% 
      mutate(abs_change = `2019`-`1970`,
             rel_change = round(100*((`2019`-`1970`)/abs(`1970`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/tab_power_sex.xlsx", row.names = F, sheetName = "bruto")

v_dem %>% 
      tibble() %>% 
      select(country_name, code = country_text_id, year, historical_date, codingstart,
             v2pepwrort_osp) %>%
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(country_name == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(year, region) %>% 
      summarize(mean_power_sex = mean(v2pepwrort_osp, na.rm = T)) %>%  
      mutate(region = case_when(region == "Africa" ~ "África",
                                region == "Americas" ~ "Américas",
                                region == "Europe" ~ "Europa",
                                region == "Asis" ~ "Ásia",
                                region == "Africa" ~ "África",
                                TRUE ~ as.character(region))) %>% 
      ggplot(aes(year, mean_power_sex, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 4) +
      scale_x_continuous(limits = c(1970, 2019), breaks = c(1970, 1980, 1990, 2000, 2010, 2019)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Índice de distribuição de poder \n por orientação sexual") +
      theme(axis.text = element_text(colour = "black", size = 28),
            axis.title.x = element_text(face = "bold", colour = "black",
                                        margin = margin(t = 0, r = 0, b = -10, l = 0)),
            axis.title.y = element_text(face = "bold", colour = "black"),
            legend.title = element_blank(), 
            legend.text = element_text(size = 26, colour = "black"),
            text = element_text(size = 24, family = "Times New Roman"),
            strip.text = element_text(face = "bold", size = 22),
            legend.spacing.x = unit(1, 'cm'),
            legend.position = "bottom") +
      guides(color = guide_legend(label.position = "bottom", ncol = 6))


















































