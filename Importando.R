
#%% Censo de 1872 por municipio

library(tidyverse)
library(readxl)

censo <- read_xlsx("C:/Users/izabe/Desktop/censo 1872 por municipio.xlsx")

censo2 <- censo %>% 
  pivot_longer(`Homem livre`:`Total`,
               "Sexo_Condicao")

censo2 <- censo2 %>% 
  pivot_wider(names_from = c("Grupo de variáveis", "Variável"),
              values_from = value)

censo2 <- censo2 %>% 
  group_by(`Código da Província`, 
           Província, 
          `Código do Mun.`,
           Município, 
          Sexo_Condicao) %>% 
  summarize_at(vars(`Total_Almas`:`Outras ocupações_S./ Inf.`),
               sum, na.rm = TRUE)

#%% nomenclatura 

censo2 <- censo2 %>% 
  mutate (Sexo_Condicao = case_when(
    Sexo_Condicao == "Escrava" ~ "Mulher Escravizada",
    Sexo_Condicao == "Escravo" ~ "Homem Escravizado",
    Sexo_Condicao == "Homem livre" ~ "Homem Livre",
    Sexo_Condicao == "Mulher livre" ~ "Mulher Livre",
    Sexo_Condicao == "Total" ~ "Total",
    Sexo_Condicao == "Total - escravo" ~ "Total Escravizados",
    TRUE ~ "Total Livres"
  )) %>% 
  arrange(Província, Município, Sexo_Condicao)

#%% salvando csv

write.csv(censo2, "C:/Users/izabe/Desktop/Github/Censo_1872/Censo_1872_dados_tidy")

