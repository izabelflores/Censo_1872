
#%% Censo de 1872 por municipio

library(tidyverse)
library(RODBC)

# Importacao

con <- odbcConnectAccess('Pop-1872-Brasil_versao1_0.mdb')
censo <- sqlFetch(con,'T8c Consulta_municipios')

# Manipulacao


censo2 <- df %>% 
  pivot_longer(`SomaDeH_LIVRES`:`SomaDeSOMA_G`,
               "Sexo_Condicao")

censo2 <- censo2 %>% pivot_wider(names_from = c('PrimeiroDeGrupo',
                                                'PrimeiroDeDescrição'),
                                 values_from = value)

censo2 <- censo2 %>% group_by(ProvId,
                              PrimeiroDeProvincia,
                              MunicId,
                              PrimeiroDeMunicipio,
                              Sexo_Condicao) %>%
  summarize_at(vars(`Total_Almas`:`Outras ocupações_S./ Inf. `), sum, na.rm=TRUE)

censo2 <- censo2 %>% 
  mutate (Sexo_Condicao = case_when(
    Sexo_Condicao == "SomaDeM_ESCR" ~ "Mulher Escravizada",
    Sexo_Condicao == "SomaDeH_ESCR" ~ "Homem Escravizado",
    Sexo_Condicao == "SomaDeH_LIVRES" ~ "Homem Livre",
    Sexo_Condicao == "SomaDeM_LIVRES" ~ "Mulher Livre",
    Sexo_Condicao == "SomaDeSOMA_G" ~ "Total",
    Sexo_Condicao == "SomaDeSOMA_H" ~ "Total Escravizados",
    TRUE ~ "Total Livres"
  )) %>% 
  arrange(PrimeiroDeProvincia, 
          PrimeiroDeMunicipio, 
          Sexo_Condicao)


censo2 <- censo2 %>% arrange(PrimeiroDeProvincia, 
                             PrimeiroDeMunicipio)

#%% Salvando csv

write.csv2(censo2, "C:/Users/izabe/Desktop/Github/Censo_1872/Censo_1872_dados_tidy_versao2.csv")

