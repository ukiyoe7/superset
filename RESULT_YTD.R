##  RESULT YTD SUPERSET
## 11.2023
## SANDRO JAKOSKA

## libraries & connections =============================================

library(DBI)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

con_sgo <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

con_spset <- dbConnect(odbc::odbc(), "dbcomercial", encoding = "UTF-8")

## RESULT ===============================================

new_result_ytd <- dbGetQuery(con_sgo, statement = read_file('RESULT_YTD.sql')) 

View(new_result_ytd)

new_result_ytd_janout23 <- 
get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\SUPERSET\\BASES\\new_result_ytd_janout23.RData"))

## EMISSAO

result_setores_emissao_ytd <-
  new_result_ytd %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="EMISSAO") %>% 
  group_by(SETOR,TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


result_geral_emissao_ytd <-
  new_result_ytd %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="EMISSAO") %>% 
  group_by(SETOR='GERAL',TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


## BAIXA

result_setores_baixa_ytd <-
  new_result_ytd %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="BAIXA") %>% 
  group_by(SETOR,TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


result_geral_baixa_ytd <-
  new_result_ytd %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="BAIXA") %>% 
  group_by(SETOR='GERAL',TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


new_result2_ytd <-
  union_all(result_setores_emissao_ytd,result_geral_emissao_ytd) %>% 
  union_all(.,result_setores_baixa_ytd) %>% 
  union_all(.,result_geral_baixa_ytd) %>% 
  mutate(INDICADOR='RESULTADO YTD') %>% mutate(VALOR=round(VALOR,2))

View(new_result2_ytd)



## METAS

METAS_2023 <-
  get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData"))

View(METAS_2023)

METAS_2023_YTD <- 
METAS_2023 %>% 
 filter(DATA<=as.Date('2023-10-01')) %>% 
  group_by(SETOR) %>% summarize(V=sum(VALOR))

View(METAS_2023_YTD)









