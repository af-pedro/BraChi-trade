##########################################
#### Dados - BraChi Trade (1997-2020) ####
##########################################

#### Carregando pacotes ####

if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)
if(require(devtools) == F) install.packages('devtools'); require(devtools)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
options(width = 10)

#### Carregando dados brutos ####

setwd("C:/Users/DELL/Desktop/BraChi-trade")

# Carregando dados das exportacoes

dados_brutos_exp <- read_delim("Br_EXP_1997_2020.csv", 
                               "\t", escape_double = FALSE, 
                               col_types = cols(`Valor FOB (US$)` = col_number()), 
                               locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE)
View(dados_brutos_exp)


# Carregando dados das importacoes

dados_brutos_imp <- read_delim("Br_IMP_1997_2020.csv", 
                               "\t", escape_double = FALSE,
                               col_types = cols(`Valor FOB (US$)` = col_number()), 
                               locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE)

view(dados_brutos_imp)

#### Processando dos dados brutos ####

# exp

dados_limpos_exp <- dados_brutos_exp %>% 
  janitor::clean_names() %>%
  dplyr::rename(
    "uf" = uf_do_produto, 
    "exp" = valor_fob_us
  ) 

rm(dados_brutos_exp)

# imp

dados_limpos_imp <- dados_brutos_imp %>% 
  janitor::clean_names() %>%
  dplyr::rename(
    "uf" = uf_do_produto, 
    "imp" = valor_fob_us
  ) 

rm(dados_brutos_imp)

#### Juntando as bases de dados de exp-imp (1997-2020) ####

# Juntando as bases

trade <- full_join(x = dados_limpos_exp,
                   y = dados_limpos_imp)

trade[is.na(trade)] <- 0 # transformando NA's em zero


# Calculando a balanca comercial

trade_balance <- trade %>%
  group_by(uf, ano) %>% 
  summarise(saldo = mean(exp - imp, na.rm = TRUE))


# Colocando a balanca comercial na base "trade"

dados <- full_join(x = trade_balance,
                   y = trade)

dados$paises <- NULL # Retirando coluna paises (China) da base de dados

rm(dados_limpos_exp, dados_limpos_imp, trade, trade_balance) # removendo


#### Mapa ####

# Instalando pacotes 

if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(mapproj) == F) install.packages("mapproj"); require(mapproj)
if(require(stringr) == F) install.packages("stringr"); require(stringr)
if(require(abjutils) == F) install.packages("abjutils"); require(abjutils)
if(require(plotly) == F) install.packages('plotly'); require(plotly)
if(require(RColorBrewer) == F) install.packages('RColorBrewer'); require(RColorBrewer)
options(scipen = 999)
memory.limit (9999999999)

# Importando mapa

shp <- readOGR("Mapa\\.", 
               "BR_UF_2020", 
               stringsAsFactors = FALSE, 
               encoding= "")

plot(shp)
shp@data

# Convertendo o shapefile para dataframe

shapefile_df <- fortify(shp)
dim(shapefile_df)

names(shapefile_df)
head(shapefile_df)

shapefile_data <- fortify(shp@data)
shapefile_data$id <- row.names(shapefile_data)

shapefile_df <- full_join(shapefile_df, 
                          shapefile_data, 
                          by="id")

names(shapefile_df)
head(shapefile_df)

# Testando mapa ggplot

map <- ggplot() +
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group, fill = IDHM),
               colour = "black", fill = 'white', size = .2) +
  coord_map()

map

# Limpando base

# primeira rodada

mapa_df <- shapefile_df %>%
  mutate(
    uf = stringr::str_to_lower(NM_UF), # nome do valores em minisculo
    uf = abjutils::rm_accent(uf), # retirar acentos
    uf = dplyr::case_when(uf == "Rondônia" ~ "rondonia",
                          uf == "Pará" ~ "para",
                          uf == "Amapá" ~ "amapa",
                          uf == "Maranhão" ~ "maranhao",
                          uf == "Piauí" ~ "piaui",
                          uf == "Paraíba" ~ "paraiba",
                          uf == "Cear?" ~ "ceara",
                          uf == "Espírito Santo" ~ "espirito santo",
                          uf == "São Paulo" ~ "sao paulo",
                          uf == "Paraná" ~ "parana",
                          uf == "Goiás" ~ "goias",
                          TRUE ~ uf) # renomear nomes
  ) 

# segunda rodada

mapa <- mapa_df %>%
  mutate (
    uf = dplyr::case_when(uf == "ronda?nia" ~ "rondonia",
                          uf == "para?" ~ "para",
                          uf == "amapa?" ~ "amapa",
                          uf == "Maranha?o" ~ "maranhao",
                          uf == "piau-" ~ "piaui",
                          uf == "paraa-ba" ~ "paraiba",
                          uf == "Cear?" ~ "ceara",
                          uf == "espa-rito santo" ~ "espirito santo",
                          uf == "sa?o paulo" ~ "sao paulo",
                          uf == "parana?" ~ "parana",
                          uf == "goia?s" ~ "goias",
                          TRUE ~ uf) # renomear nomes
  )



dadosbr <- dados %>%
  mutate(
    uf = stringr::str_to_lower(uf),
    uf = abjutils::rm_accent(uf),
    uf = dplyr::case_when(uf == "ronda´nia" ~ "rondonia",
                          uf == "para¡" ~ "para",
                          uf == "amapa¡" ~ "amapa",
                          uf == "maranha£o" ~ "maranhao",
                          uf == "piaua-" ~ "piaui",
                          uf == "paraa-ba" ~ "paraiba",
                          uf == "ceara¡" ~ "ceara",
                          uf == "espa-rito santo" ~ "espirito santo",
                          uf == "sa£o paulo" ~ "sao paulo",
                          uf == "parana¡" ~ "parana",
                          uf == "goia¡s" ~ "goias",
                          TRUE ~ uf)
  )

# criando base de dados para 2020

dados_br <- dados_br %>%
  select(uf, saldo, exp, imp, ano)

mapa_br20 <- mapa %>%
  select(uf, NM_REGIAO)

# juntando as bases

dados_br_final <- full_join(dados_br, 
                            mapa_br20,
                            by = "uf")

# Categorizando saldo comercial


dados_br20 <- dados_br20 %>% 
  mutate(saldocat = cut(saldo, breaks = c(-4496388727,-471501669, 
                                          0, 3094528772, 11871243305 
  ),
  labels = c('-4496388727 a -471 501 669',
             '-471501669 a 0',
             '0 a 3094528772',
             '3094528772 a 11871243305'),
  include.lowest = T))

# Plotando mapa 1 - balança comercial (2020)


ggplot() + 
  geom_polygon(data = dados_br20,
               aes(x = long, y = lat, group = group, fill = saldocat),
               colour = "white", size = .2) +
  labs(x = "",
       y = "",
       fill = "Saldo comercial (Bilhões de USD)") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[5:9]) + #escolhendo as tonalidades manualmente
  coord_map()

# Plotando mapa 2 - 2000, 2005, 2010, 2015, 2020

# Manipulando data

dados_2000_a_2020 <- dados %>%
  filter(ano %in% c(2000, 2005, 2010, 2015, 2020))

estados_2000_a_2020 <- dados_2000_a_2020 %>%
  mutate(
    uf = stringr::str_to_lower(uf),
    uf = abjutils::rm_accent(uf),
    uf = dplyr::case_when(TRUE ~ uf)
  )

dados_2000_2020 <- full_join(ne, 
                             estados_2000_a_2020,
                             by = "uf")

# Plotando grafico 3 - Balanca comercial (2000-2020)

ggplot() + 
  geom_polygon(data = dados_2000_2020,
               aes(x = long, y = lat, group = group, fill = saldo),
               colour = "white", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  facet_wrap(~ano, ncol = 3)

# Plotando grafico 4 - exportacao (2000-2020)

ggplot() + 
  geom_polygon(data = dados_2000_2020,
               aes(x = long, y = lat, group = group, fill = exp),
               colour = "white", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  facet_wrap(~ano, ncol = 3)

# Plotando grafico 5 - importacao (2000-2020)

ggplot() + 
  geom_polygon(data = dados_2000_2020,
               aes(x = long, y = lat, group = group, fill = imp),
               colour = "white", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  facet_wrap(~ano, ncol = 3)

# Plotando grafico 5 - time series da balanca comercial dos estado 

balanca_hist <- ggplot() +
  geom_line(data = dados,
            aes(x = ano, y = saldo, group = uf, colour = uf)) +
  theme_minimal()

ggplotly(balanca_hist) # grafico interativo

# Plotando grafico 6 - time series do saldo dos estados do NE

dados %>%
  filter(uf %in% c("Alagoas", "Bahia", "Ceará",
                   "Maranhão", "Paraíba", "Pernambuco",
                   "Piauí","Rio Grande do Norte","Sergipe" )) %>%
  ggplot() +
  geom_line(aes(x = ano, y = saldo)) +
  facet_wrap(~uf, ncol = 3)


dados_ne <- dados %>% 
  filter(uf %in% c("Alagoas", "Bahia", "Ceará",
                   "Maranhão", "Paraíba", "Pernambuco",
                   "Piauí","Rio Grande do Norte","Sergipe" )) %>% 
  select(ano, exp, imp, saldo)


saldo_ne <- ggplot(dados_br) +
  geom_line(aes(x = ano, y = saldo, group = uf, colour = uf)) 

ggplotly(saldo_ne)

# Salvando base de dados_br

write.csv(dados_br, 'dados_br.csv')

# Carregando dados 

dados_br <- read_csv("dados_br.csv", 
                     locale = locale(encoding = "ISO-8859-1"))