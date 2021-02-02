# CARREGA PACOTE ----

library(tidyverse)
library(janitor)
library(extrafont)
loadfonts(device = "win")

# CARREGA DADOS ----

internet <- read_csv(file = "./tabelas/digital/API_IT.NET.USER.ZS_DS2_en_csv_v2_1928189.csv", skip = 4)
banda_larga  <- read_csv(file = "./tabelas/digital/API_IT.NET.BBND.P2_DS2_en_csv_v2_1928186.csv", skip = 4)
server <- read_csv(file = "./tabelas/digital/API_IT.NET.SECR.P6_DS2_en_csv_v2_1928188.csv", skip = 4)
mobile <- read_csv(file = "./tabelas/digital/API_IT.CEL.SETS.P2_DS2_en_csv_v2_1928180.csv", skip = 4)

code <- read_csv("./tabelas/cnt-code.csv")

# TRANSFORMA E SALVA DADOS ----

code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

# Internet ----

internet_clean <- internet %>% 
      clean_names %>% 
      left_join(code, by = c("country_code" = "code")) %>% 
      select(region, x1990:x2020) %>% 
      gather(ano, valor, -region) %>% 
      mutate(ano = as.numeric(str_replace(ano, "x", ""))) %>%
      filter(ano %in% c(1990, 2000, 2010, 2018)) %>% 
      group_by(region, ano) %>% 
      summarize(valor = mean(valor, na.rm = T))
      
internet_clean %>% 
      filter(!is.na(region)) %>% 
      spread(ano, valor) %>% 
      mutate(abs_change = `2018`-`2000`,
             rel_change = round(100*((`2018`-`2000`)/abs(`2000`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/digital/tab_internet.xlsx", row.names = F, sheetName = "bruto")

internet_clean %>% 
      filter(!is.na(region)) %>% 
      mutate(region = case_when(region == "Africa" ~ "África",
                                region == "Americas" ~ "Américas",
                                region == "Europe" ~ "Europa",
                                region == "Asia" ~ "Ásia",
                                TRUE ~ as.character(region)
                                )) %>% 
      ggplot(aes(ano, valor, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 5) +
      scale_y_continuous(limits = c(0, 90)) +
      scale_x_continuous(limits = c(1990, 2018), breaks = c(1990, 2000, 2010, 2018)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      labs(x = "", y = "") +
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


# Banda larga ----

banda_larga_clean <- banda_larga %>% 
      clean_names %>% 
      left_join(code, by = c("country_code" = "code")) %>% 
      select(region, x2001:x2020) %>% 
      gather(ano, valor, -region) %>% 
      mutate(ano = as.numeric(str_replace(ano, "x", ""))) %>%
      filter(ano %in% c(2001, 2010, 2018)) %>% 
      group_by(region, ano) %>% 
      summarize(valor = mean(valor, na.rm = T))

banda_larga_clean %>% 
      filter(!is.na(region)) %>% 
      spread(ano, valor) %>% 
      mutate(abs_change = `2018`-`2010`,
             rel_change = round(100*((`2018`-`2010`)/abs(`2010`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/digital/tab_banda_larga.xlsx", row.names = F, sheetName = "bruto")

banda_larga_clean %>% 
      filter(!is.na(region)) %>% 
      mutate(region = case_when(region == "Africa" ~ "África",
                                region == "Americas" ~ "Américas",
                                region == "Europe" ~ "Europa",
                                region == "Asia" ~ "Ásia",
                                TRUE ~ as.character(region)
      )) %>% 
      ggplot(aes(ano, valor, group = region, color = region)) +
      geom_line(size = 1) + 
      geom_point(size = 5) +
      scale_y_continuous(limits = c(0, 40)) +
      scale_x_continuous(limits = c(2001, 2018), breaks = c(2001, 2010, 2018)) +
      ggthemes::theme_gdocs() +
      scale_color_brewer(palette = "Dark2") +
      labs(x = "", y = "") +
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

# Server ----

server_clean <- server %>% 
      clean_names %>% 
      left_join(code, by = c("country_code" = "code")) %>% 
      select(region, x2010:x2019) %>% 
      gather(ano, valor, -region) %>% 
      mutate(ano = as.numeric(str_replace(ano, "x", ""))) %>%
      filter(ano %in% c(2010, 2019)) %>% 
      group_by(region, ano) %>% 
      summarize(valor = mean(valor, na.rm = T))
      
server_clean %>% 
      filter(!is.na(region)) %>% 
      spread(ano, valor) %>% 
      mutate(abs_change = `2019`-`2010`,
             rel_change = round(100*((`2019`-`2010`)/abs(`2010`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/digital/tab_server.xlsx", row.names = F, sheetName = "bruto")

# Mobile ----

mobile_clean <- mobile %>% 
      clean_names %>% 
      left_join(code, by = c("country_code" = "code")) %>% 
      select(region, x2010:x2019) %>% 
      gather(ano, valor, -region) %>% 
      mutate(ano = as.numeric(str_replace(ano, "x", ""))) %>%
      filter(ano %in% c(2010, 2019)) %>% 
      group_by(region, ano) %>% 
      summarize(valor = mean(valor, na.rm = T))

mobile_clean %>% 
      filter(!is.na(region)) %>% 
      spread(ano, valor) %>% 
      mutate(abs_change = `2019`-`2010`,
             rel_change = round(100*((`2019`-`2010`)/abs(`2010`)), 1)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabelas/digital/tab_mobile.xlsx", row.names = F, sheetName = "bruto")

mobile_clean %>% 
      filter(!is.na(region)) %>% 
      mutate(region = case_when(region == "Africa" ~ "África",
                                region == "Americas" ~ "Américas",
                                region == "Europe" ~ "Europa",
                                region == "Asia" ~ "Ásia",
                                TRUE ~ as.character(region)),
             ano = as.character(ano)) %>% 
      ggplot(aes(ano, valor, group = region, fill = region)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      # geom_line(size = 1) + 
      # geom_point(size = 5) +
      scale_y_continuous(limits = c(0, 130)) +
      # scale_x_continuous(limits = c(2010, 2019), breaks = c(2010, 2019)) +
      ggthemes::theme_gdocs() +
      scale_fill_brewer(palette = "Dark2") +
      labs(x = "", y = "") +
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
      guides(fill = guide_legend(label.position = "bottom", nrow = 1))



