## RESULT SUPERSET FILIAIS
## 12.2023
## SANDRO JAKOSKA


## libraries & connections =============================================

library(DBI)
library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

con_sgo <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

con_spset <- dbConnect(odbc::odbc(), "dbcomercial", encoding = "UTF-8")


## RESULT ===============================================

result_filiais <- dbGetQuery(con_sgo, statement = read_file("C:\\Users\\REPRO SANDRO\\Documents\\R\\SUPERSET\\SQL\\RESULT_FILIAIS.sql")) 


View(result_filiais)


## METAS ==================================================

metas_filiais_2023 <-
  get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData"))


save(metas_filiais_2023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData")


## RESULT =======================================================

result_filiais_2 <- 
result_filiais %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(VALOR=sum(VRLIQUIDO)) %>% mutate(INDICADOR='VENDA LIQUIDA')

View(result_filiais2)


## METAS =======================================================

metas_filiais_2023_2 <-
metas_filiais_2023 %>% 
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
  mutate(INDICADOR='META MES ATUAL') 

View(metas_filiais_2023_2)


## ALCANCE  ============================================

alcance_result_filiais <-
  left_join(result_filiais_2,
            metas_filiais_2023 %>% 
             rename(METAS=VALOR) %>% 
              filter(DATA==floor_date(Sys.Date(),"month")) %>% 
               select(-EMPRESA,-MES),by="EMPCODIGO") %>% 
  mutate(ALCANCE=round(((VALOR/METAS)),3)) %>% 
  select(-VALOR,-METAS) %>% 
  mutate(INDICADOR='ALCANCE') %>% 
  rename(VALOR=ALCANCE) %>% mutate(VALOR=round(VALOR,3))



## BASE GERAL =========================================

corder <- c("MES","EMPCODIGO","INDICADOR","VALOR")

dt_filiais <-
  union_all(
    result_filiais_2 %>% .[,corder],
    
    metas_filiais_2023_2 %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            alcance_result_filiais %>% .[,corder]) %>% 
  
  as.data.frame()


dt_filiais <- dt_filiais %>% mutate(MES=as.character(MES))

dt_filiais <- dt_filiais %>% mutate(ID=seq(1:nrow(dt_filiais)))

view(dt_filiais)
