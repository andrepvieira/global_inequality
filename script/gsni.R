# CARREGA PACOTE ----
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

# CARREGA DADOS ----

gsni <- readxl::read_excel("./tabelas/gsni_tables.xlsx", sheet = 1, skip = 7, n_max = 76)
code <- read_csv("./tabelas/cnt-code.csv")

# TRANSFORMA DADOS ----

code <- code %>% 
      select(name, region)

gsni %>% 
   janitor::clean_names() %>% 
   select(name = country, period, percent_4) %>% 
   left_join(code) %>% 
   mutate(region = case_when(name == "United States" ~ "Americas",
                             name == "United Kingdom" ~ "Europe",
                             name == "Korea (Republic of)" ~ "Asia",
                             name == "Iran, Islamic Republic of" ~ "Asia",
                             name == "Overall averagea,b" ~ "Global",
                             TRUE ~ as.character(region))) %>% 
   filter(region == "Americas")


gsni %>% 
      janitor::clean_names() %>% 
      select(name = country, period, percent_4) %>% 
      left_join(code) %>% 
      mutate(region = case_when(name == "United States" ~ "Americas",
                                name == "United Kingdom" ~ "Europe",
                                name == "Korea (Republic of)" ~ "Asia",
                                name == "Iran, Islamic Republic of" ~ "Asia",
                                name == "Overall averagea,b" ~ "Global",
                                TRUE ~ as.character(region))) %>% 
      # filter(period == "2010–2014") %>%
      group_by(region, period) %>% 
      summarize(mean_pct = mean(percent_4, na.rm = T)) %>% 
      spread(period, mean_pct) %>% 
      dplyr::select(-`last available`) %>% 
      dplyr::filter(!region == "Global") %>% 
      # dplyr::mutate(abs_change = `2010–2014`-`2005–2009`,
      #        rel_change = round(100*((`2010-2014`-`2005–2009`)/abs(`2005–2009`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/tab_gsni.xlsx", row.names = F, sheetName = "bruto")


gsni %>% 
      janitor::clean_names() %>% 
      select(name = country, period, percent_4) %>% 
      left_join(code) %>% 
      mutate(region = case_when(name == "United States" ~ "Americas",
                                name == "United Kingdom" ~ "Europe",
                                name == "Korea (Republic of)" ~ "Asia",
                                name == "Iran, Islamic Republic of" ~ "Asia",
                                name == "Overall averagea,b" ~ "Global",
                                TRUE ~ as.character(region))) %>% 
      filter(!region == "Global") %>%
      mutate(region = case_when(region == "Americas" ~ "Américas",
                                region == "Africa" ~ "África",
                                region == "Europe" ~ "Europa",
                                region == "Asia" ~ "Ásia",
                                TRUE ~ as.character(region))) %>% 
      group_by(region, period) %>% 
      summarize(mean_pct = mean(percent_4, na.rm = T)) %>% 
      ggplot(aes(region, mean_pct, group = period, fill = period)) +
      geom_bar(stat = "identity", position = "dodge", width = .7) +
      # geom_point(size = 4) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
      ggthemes::theme_gdocs() +
      scale_fill_brewer(palette = "Set1") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Percentual de pessoas com pelo menos um viés (%)") +
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
      guides(fill = guide_legend(label.position = "bottom", ncol = 2, nrow = 1))









