# CARREGA PACOTE ----
library(tidyverse)
library(extrafont)
library(janitor)
loadfonts(device = "win")

# CARREGA DADOS ----

setwd("D:/desigualdade_reis")
setwd("./tabelas/PISA")

pisa_nse <- list.files(pattern = "-nse") %>% 
      map_df(~read_csv(.))

pisa_sexo <- list.files(pattern = "-sexo") %>% 
      map_df(~read_csv(.))

setwd("..")
setwd("..")

code <- read_csv("./tabelas/cnt-code.csv")

# TRANSFORMA DADOS ----

code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

# Tabela com medias geral e por grupos ----

setwd("..")

pisa_nse %>% 
      clean_names %>% 
      select(name = pais, 
             pontuacao_ciencias_geral, 
             pontuacao_ciencias_nivel_socioeconomico_alto, 
             pontuacao_ciencias_nivel_socioeconomico_baixo, 
             pontuacao_leitura_geral, 
             pontuacao_leitura_nivel_socioeconomico_alto,
             pontuacao_leitura_nivel_socioeconomico_baixo,
             pontuacao_matematica_geral,
             pontuacao_matematica_nivel_socioeconomico_alto,
             pontuacao_matematica_nivel_socioeconomico_baixo) %>% 
      filter(!str_detect(name, "Região")) %>% 
      mutate(region = case_when(name %in% c("Austrália", "Nova Zelândia") ~ "Oceania",
                                name %in% c("Arábia Saudita", "Marrocos") ~ "África",
                                name %in% c("Argentina", "Brasil", "Chile", "Colômbia", "Peru", "Uruguai",
                                            "Canadá", "Costa Rica", "Estados Unidos", "México", "Panamá",
                                            "República Dominicana") ~ "América",
                                name %in% c("Baku (Azerbaijão)", "Brunei Darussalam", "Cazaquistão", 
                                            "China (B-S-J-Z)", "Cingapura", "Coreia", "Emirados Árabes Unidos", 
                                            "Filipinas", "Geórgia", "Hong Kong", "Indonésia", "Israel", "Japão",
                                            "Jordânia", "Líbano", "Macao", "Malásia", "Qatar", "Tailândia",
                                            "Taipei Chinesa (Taiwan)", "Taipe Chinesa (Taiwan)") ~ "Ásia",
                                name %in% c("OCDE") ~ "OCDE",
                                TRUE ~ "Europa")) %>% 
      filter(!region == "OCDE") %>%
      group_by(region) %>% 
      summarize(cie_geral = mean(pontuacao_ciencias_geral, na.rm = T),
                cie_alto = mean(pontuacao_ciencias_nivel_socioeconomico_alto, na.rm = T),
                cie_baixo = mean(pontuacao_ciencias_nivel_socioeconomico_baixo, na.rm = T),
                lei_geral = mean(pontuacao_leitura_geral, na.rm = T),
                lei_alto = mean(pontuacao_leitura_nivel_socioeconomico_alto, na.rm = T),
                lei_baixo = mean(pontuacao_leitura_nivel_socioeconomico_baixo, na.rm = T),
                mat_geral = mean(pontuacao_matematica_geral, na.rm = T),
                mat_alto = mean(pontuacao_matematica_nivel_socioeconomico_alto, na.rm = T),
                mat_baixo = mean(pontuacao_matematica_nivel_socioeconomico_baixo, na.rm = T)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "tb-pisa-nse.xlsx", row.names = F)
      
pisa_sexo %>% 
      clean_names %>% 
      select(name = pais, 
             pontuacao_ciencias_geral, 
             pontuacao_ciencias_feminino, 
             pontuacao_ciencias_masculino, 
             pontuacao_leitura_geral, 
             pontuacao_leitura_feminino,
             pontuacao_leitura_masculino,
             pontuacao_matematica_geral,
             pontuacao_matematica_feminino,
             pontuacao_matematica_masculino) %>% 
      filter(!str_detect(name, "Região")) %>% 
      mutate(region = case_when(name %in% c("Austrália", "Nova Zelândia") ~ "Oceania",
                                name %in% c("Arábia Saudita", "Marrocos") ~ "África",
                                name %in% c("Argentina", "Brasil", "Chile", "Colômbia", "Peru", "Uruguai",
                                            "Canadá", "Costa Rica", "Estados Unidos", "México", "Panamá",
                                            "República Dominicana") ~ "América",
                                name %in% c("Baku (Azerbaijão)", "Brunei Darussalam", "Cazaquistão", 
                                            "China (B-S-J-Z)", "Cingapura", "Coreia", "Emirados Árabes Unidos", 
                                            "Filipinas", "Geórgia", "Hong Kong", "Indonésia", "Israel", "Japão",
                                            "Jordânia", "Líbano", "Macao", "Malásia", "Qatar", "Tailândia",
                                            "Taipei Chinesa (Taiwan)", "Taipe Chinesa (Taiwan)") ~ "Ásia",
                                name %in% c("OCDE") ~ "OCDE",
                                TRUE ~ "Europa")) %>% 
      filter(!region == "OCDE") %>%
      group_by(region) %>% 
      summarize(cie_geral = mean(pontuacao_ciencias_geral, na.rm = T),
                cie_fem = mean(pontuacao_ciencias_feminino, na.rm = T),
                cie_masc = mean(pontuacao_ciencias_masculino, na.rm = T),
                lei_geral = mean(pontuacao_leitura_geral, na.rm = T),
                lei_fem = mean(pontuacao_leitura_feminino, na.rm = T),
                lei_masc = mean(pontuacao_leitura_masculino, na.rm = T),
                mat_geral = mean(pontuacao_matematica_geral, na.rm = T),
                mat_fem = mean(pontuacao_matematica_feminino, na.rm = T),
                mat_masc = mean(pontuacao_matematica_masculino, na.rm = T)) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "tb-pisa-sexo.xlsx", row.names = F)



pisa_nse <- pisa_nse %>% 
      clean_names %>% 
      select(name = pais, pontuacao_ciencias_diferenca, pontuacao_leitura_diferenca, pontuacao_matematica_diferenca) %>% 
      filter(!str_detect(name, "Região")) %>% 
      mutate(region = case_when(name %in% c("Austrália", "Nova Zelândia") ~ "Oceania",
                              name %in% c("Arábia Saudita", "Marrocos") ~ "África",
                              name %in% c("Argentina", "Brasil", "Chile", "Colômbia", "Peru", "Uruguai",
                                          "Canadá", "Costa Rica", "Estados Unidos", "México", "Panamá",
                                          "República Dominicana") ~ "América",
                              name %in% c("Baku (Azerbaijão)", "Brunei Darussalam", "Cazaquistão", 
                                          "China (B-S-J-Z)", "Cingapura", "Coreia", "Emirados Árabes Unidos", 
                                          "Filipinas", "Geórgia", "Hong Kong", "Indonésia", "Israel", "Japão",
                                          "Jordânia", "Líbano", "Macao", "Malásia", "Qatar", "Tailândia",
                                          "Taipei Chinesa (Taiwan)", "Taipe Chinesa (Taiwan)") ~ "Ásia",
                              name %in% c("OCDE") ~ "OCDE",
                              TRUE ~ "Europa"))

pisa_nse %>% 
      # filter(!is.na(pontuacao_ciencias_diferenca)) %>% 
      filter(!region == "OCDE") %>%
      group_by(region) %>% 
      summarize(dif_cie = mean(pontuacao_ciencias_diferenca, na.rm = T),
                dif_lei = mean(pontuacao_leitura_diferenca, na.rm = T),
                dif_mat = mean(pontuacao_matematica_diferenca, na.rm = T)) %>% 
      gather(disciplina, valor, -region) %>% 
      mutate(disciplina = case_when(disciplina == "dif_cie" ~ "Ciências",
                                    disciplina == "dif_lei" ~ "Leitura",
                                    disciplina == "dif_mat" ~ "Matemática"),
             valor = round(valor, 0)) %>% 
      ggplot(aes(region, valor, group = disciplina, fill = disciplina)) +
      geom_bar(stat = "identity", width = .7, position = position_dodge()) +
      geom_text(aes(label = valor), size = 8, position = position_dodge(width = 0.7), vjust = -0.25) +
      # scale_y_continuous(limits = c(0, 250)) +
      scale_y_continuous(limits = c(0, 100)) +
      ggthemes::theme_gdocs() +
      scale_fill_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Diferença na pontuação por \n nível socioeconômico") +
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
      guides(fill = guide_legend(label.position = "bottom", ncol = 3, nrow = 1))


pisa_sexo <- pisa_sexo %>% 
      clean_names %>% 
      select(name = pais, pontuacao_ciencias_diferenca, pontuacao_leitura_diferenca, pontuacao_matematica_diferenca) %>% 
      filter(!str_detect(name, "Região")) %>% 
      mutate(region = case_when(name %in% c("Austrália", "Nova Zelândia") ~ "Oceania",
                                name %in% c("Arábia Saudita", "Marrocos") ~ "África",
                                name %in% c("Argentina", "Brasil", "Chile", "Colômbia", "Peru", "Uruguai",
                                            "Canadá", "Costa Rica", "Estados Unidos", "México", "Panamá",
                                            "República Dominicana") ~ "América",
                                name %in% c("Baku (Azerbaijão)", "Brunei Darussalam", "Cazaquistão", 
                                            "China (B-S-J-Z)", "Cingapura", "Coreia", "Emirados Árabes Unidos", 
                                            "Filipinas", "Geórgia", "Hong Kong", "Indonésia", "Israel", "Japão",
                                            "Jordânia", "Líbano", "Macao", "Malásia", "Qatar", "Tailândia",
                                            "Taipei Chinesa (Taiwan)", "Taipe Chinesa (Taiwan)") ~ "Ásia",
                                name %in% c("OCDE") ~ "OCDE",
                                TRUE ~ "Europa"))

pisa_sexo %>% 
      # filter(!is.na(pontuacao_ciencias_diferenca)) %>% 
      filter(!region == "OCDE") %>%
      group_by(region) %>% 
      summarize(dif_cie = mean(pontuacao_ciencias_diferenca, na.rm = T),
                dif_lei = mean(pontuacao_leitura_diferenca, na.rm = T),
                dif_mat = mean(pontuacao_matematica_diferenca, na.rm = T)) %>% 
      gather(disciplina, valor, -region) %>% 
      mutate(disciplina = case_when(disciplina == "dif_cie" ~ "Ciências",
                                    disciplina == "dif_lei" ~ "Leitura",
                                    disciplina == "dif_mat" ~ "Matemática"),
             valor = round(valor, 0)) %>% 
      ggplot(aes(region, valor, group = disciplina, fill = disciplina)) +
      geom_bar(stat = "identity", width = .7, position = position_dodge()) +
      geom_text(aes(label = valor), size = 8, position = position_dodge(width = 0.7), vjust = -0.25) +
      scale_y_continuous(limits = c(-15, 50)) +
      # scale_y_continuous(limits = c(0, 100)) +
      ggthemes::theme_gdocs() +
      scale_fill_brewer(palette = "Dark2") +
      # viridis::scale_color_viridis() + 
      labs(x = "", y = "Diferença na pontuação por sexo") +
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
      guides(fill = guide_legend(label.position = "bottom", ncol = 3, nrow = 1))










