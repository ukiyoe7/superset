## NEW RESULT SUPERSET

library(DBI)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

# RESULT

new_result <- dbGetQuery(con2, statement = read_file('RESULT.sql')) 

View(new_result)

## EMISSAO

result_setores_emissao <-
  new_result %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="EMISSAO") %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR,TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


result_geral_emissao <-
  new_result %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="EMISSAO") %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR='GERAL',TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


## BAIXA

result_setores_baixa <-
new_result %>%  
   mutate(TIPO=str_trim(TIPO)) %>% 
    filter(TIPO=="BAIXA") %>% 
     group_by(MES=floor_date(DATA,"month"),SETOR,TIPO) %>% 
      summarize(VALOR=sum(VRVENDA)) 


result_geral_baixa <-
  new_result %>%  
  mutate(TIPO=str_trim(TIPO)) %>% 
  filter(TIPO=="BAIXA") %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR='GERAL',TIPO) %>% 
  summarize(VALOR=sum(VRVENDA)) 


new_result2 <-
union_all(result_setores_emissao,result_geral_emissao) %>% 
   union_all(.,result_setores_baixa) %>% 
     union_all(.,result_geral_baixa)

View(new_result2)


## METAS

METAS_2023 <-
get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData"))

View(METAS_2023)


METAS_2023 %>% filter(SETOR=="GERAL") %>% summarize(V=sum(VALOR))


save(METAS_2023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData")


metas_setores <-
  METAS_2023 %>%  
   filter(DATA==floor_date(Sys.Date(),"month")) %>% 
    group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
     summarize(METAS=sum(VALOR)) %>% as.data.frame() %>% select(-MES)


View(metas_setores)


alcance_result <-
left_join(new_result2,metas_setores ,by="SETOR") %>% View()




