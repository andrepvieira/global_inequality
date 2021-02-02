# CARREGA PACOTE ----
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

# CARREGA DADOS ----

gender_equa <- readxl::read_excel("./tabelas/HistoricalGenderEqualityIndex_Compact.xlsx", sheet = 2)
code <- read_csv("./tabelas/cnt-code.csv")

# TRANSFORMA DADOS ----

code <- code %>% 
      select(name, region)

gender_equa %>% 
      filter(year %in% c(1970, 1980, 1990, 2000)) %>% 
      rename(name = country.name) %>% 
      left_join(code) %>% 
      mutate(region = case_when(name == "United States" ~ "Americas",
                                name == "United Kingdom" ~ "Europe",
                                name == "Russia" ~ "Europe",
                                TRUE ~ as.character(region))) %>% 
      group_by(year, region) %>% 
      summarize(mean_equa = mean(value, na.rm = T)) %>% 
      spread(year, mean_equa) %>% 
      mutate(abs_change = `2000`-`1970`,
             rel_change = round(100*((`2000`-`1970`)/abs(`1970`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/tab_gender_equa.xlsx", row.names = F, sheetName = "bruto")
      
gender_equa %>% 
      filter(year %in% c(1970, 1980, 1990, 2000)) %>% 
      rename(name = country.name) %>% 
      left_join(code) %>% 
      mutate(region = case_when(name == "United States" ~ "Americas",
                                name == "United Kingdom" ~ "Europe",
                                name == "Russia" ~ "Europe",
                                TRUE ~ as.character(region))) %>% 
      mutate(region = case_when(region == "Americas" ~ "Américas",
                                region == "Africa" ~ "África",
                                region == "Europe" ~ "Europa",
                                region == "Asia" ~ "Ásia",
                                TRUE ~ as.character(region))) %>% 
      group_by(year, region) %>% 
      summarize(mean_equa = mean(value, na.rm = T)) %>% 
      ggplot(aes(year, mean_equa, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 5) +
      scale_y_continuous(limits = c(40, 90), breaks = seq(40, 90, by = 10)) +
      scale_x_continuous(limits = c(1970, 2000), 
                         breaks = c(1970, 1980, 1990, 2000)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Historical Gender Equality Index") +
      theme(axis.text = element_text(colour = "black", size = 30),
            axis.title.x = element_text(face = "bold", colour = "black",
                                        margin = margin(t = 0, r = 0, b = -10, l = 0)),
            axis.title.y = element_text(face = "bold", colour = "black"),
            legend.title = element_blank(), 
            legend.text = element_text(size = 28, colour = "black"),
            text = element_text(size = 28, family = "Times New Roman"),
            strip.text = element_text(face = "bold", size = 22),
            legend.spacing.x = unit(1, 'cm'),
            legend.position = "bottom") +
      guides(color = guide_legend(label.position = "bottom", ncol = 6))




