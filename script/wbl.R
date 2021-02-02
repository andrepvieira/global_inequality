# CARREGA PACOTE ----
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

# CARREGA DADOS ----

wbl <- readxl::read_excel("./tabelas/WBL50YearPanelDetailsWeb01Jun2020.xlsx", sheet = 2, skip = 1)
code <- read_csv("./tabelas/cnt-code.csv")


code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

wbl %>% 
      tibble() %>% 
      janitor::clean_names() %>% 
      select(code, wbl_report_year, wbl_index) %>%
      filter(wbl_report_year %in% c(1971, 1980, 1990, 2000, 2010, 2020)) %>% 
      left_join(code) %>% 
      # mutate(region = ifelse(country_name == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(wbl_report_year, region) %>% 
      summarize(mean_wbl = mean(wbl_index, na.rm = T)) %>% 
      spread(wbl_report_year, mean_wbl) %>% 
      mutate(abs_change = `2020`-`1971`,
             rel_change = round(100*((`2020`-`1971`)/abs(`1971`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/tab_wbl.xlsx", row.names = F, sheetName = "bruto")


wbl %>% 
      tibble() %>% 
      janitor::clean_names() %>% 
      select(code, wbl_report_year, wbl_index) %>%
      filter(wbl_report_year %in% c(1971, 1980, 1990, 2000, 2010, 2020)) %>% 
      left_join(code) %>% 
      # mutate(region = ifelse(country_name == "World", "World", region)) %>%
      filter(!is.na(region)) %>% 
      group_by(wbl_report_year, region) %>% 
      summarize(mean_wbl = mean(wbl_index, na.rm = T)) %>% 
      mutate(region = case_when(region == "Africa" ~ "África",
                                region == "Americas" ~ "Américas",
                                region == "Europe" ~ "Europa",
                                region == "Asis" ~ "Ásia",
                                region == "Africa" ~ "África",
                                TRUE ~ as.character(region))) %>% 
      ggplot(aes(wbl_report_year, mean_wbl, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 4) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
      scale_x_continuous(limits = c(1970, 2020), 
                         breaks = c(1971, 1980, 1990, 2000, 2010, 2020)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Índice Mulheres, Empresas e a Lei") +
      theme(axis.text = element_text(colour = "black", size = 30),
            axis.title.x = element_text(face = "bold", colour = "black",
                                        margin = margin(t = 0, r = 0, b = -10, l = 0)),
            axis.title.y = element_text(face = "bold", colour = "black"),
            legend.title = element_blank(), 
            legend.text = element_text(size = 28, colour = "black"),
            text = element_text(size = 28, family = "Times New Roman"),
            strip.text = element_text(face = "bold", size = 22),
            legend.spacing.x = unit(1, 'cm'),
            legend.position = "bottom",
      ) +
      guides(color = guide_legend(label.position = "bottom", ncol = 6))
