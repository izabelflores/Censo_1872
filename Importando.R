
#%% Censo de 1872 por municipio

library(tidyverse)
library(readxl)

censo <- read_xlsx("C:/Users/izabe/Desktop/censo 1872 por municipio.xlsx")

censo2 <- censo %>% 
  pivot_longer(`Homem livre`:`Total`,
               "Grupo")

censo2 <- censo2 %>% 
  pivot_wider(names_from = c("Grupo de variáveis", "Variável"),
              values_from = value)
