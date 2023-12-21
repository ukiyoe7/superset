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

METAS_FILIAIS_2023 <-
  get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData"))


save(METAS_FILIAIS_2023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData")


## RESULT =======================================================

result_filiais_2 <- 
result_filiais %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(VALOR=sum(VRLIQUIDO)) %>% mutate(INDICADOR='VENDA LIQUIDA')

View(result_filiais2)


## METAS =======================================================

metas_filiais_2023_2 <-
  METAS_FILIAIS_2023 %>% 
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
  mutate(INDICADOR='META MES ATUAL') 

View(metas_filiais_2023_2)


## ALCANCE  ============================================

alcance_result_filiais <-
  left_join(result_filiais_2,
            METAS_FILIAIS_2023 %>% 
             rename(METAS=VALOR) %>% 
              filter(DATA==floor_date(Sys.Date(),"month")) %>% 
               select(-EMPRESA,-MES),by="EMPCODIGO") %>% 
  mutate(ALCANCE=round(((VALOR/METAS)),3)) %>% 
  select(-VALOR,-METAS) %>% 
  mutate(INDICADOR='ALCANCE') %>% 
  rename(VALOR=ALCANCE) %>% mutate(VALOR=round(VALOR,3))


## ESPERADO  ============================================

## feriados no ano

holidays <- data.frame(DATES=c(as.Date('2023-10-12'),
                               as.Date('2023-11-02'),
                               as.Date('2023-11-15'),
                               as.Date('2023-12-25')
))

## dias uteis no mes

first_day <- floor_date(Sys.Date(), unit = "month")

last_day <- ceiling_date(Sys.Date(), 'month') - days(1)

num_days <- as.numeric(days_in_month(first_day))

num_weekends <- sum(wday(seq(first_day, last_day, by = "day")) %in% c(1,7))


working_days <- 
  data.frame(
    DATES=seq(first_day, last_day, by = "day")) 


## excluir feriados

working_days_no_holidays <-
  anti_join(working_days,holidays,by="DATES") %>% tally()  

## excluir final de semana

num_days2 <- (working_days_no_holidays - num_weekends) 


## dias uteis ate ontem

dyesterday <- Sys.Date()-1

days_mtd <- 
  data.frame(
    DATES=seq(first_day, dyesterday, by = "day")) 


## exclui feriados ate ontem
days_mtd_no_holidays <-
  anti_join(days_mtd,holidays,by="DATES") %>% tally()

## finais de semana ate ontem
num_weekends_mtd <- sum(wday(seq(first_day, dyesterday, by = "day")) %in% c(1,7))

days_until_yesterday <- days_mtd_no_holidays - num_weekends_mtd 



## alcance esperado

result_esperado_filiais <-
  metas_filiais_2023 %>% 
   filter(DATA==floor_date(Sys.Date(),"month")) %>% 
    mutate(ALCANCE_ESPERADO=(VALOR/as.numeric(num_days2))) %>% 
     mutate(ALCANCE_ESPERADO2=(ALCANCE_ESPERADO*as.numeric(days_until_yesterday))) %>% 
      mutate(ALCANCE_ESPERADO3=(ALCANCE_ESPERADO2/VALOR)) %>% 
       mutate(INDICADOR='ALCANCE ESPERADO') %>% 
        select(EMPCODIGO,DATA,ALCANCE_ESPERADO3,INDICADOR) %>% 
         rename(MES=DATA) %>% 
          rename(VALOR=ALCANCE_ESPERADO3)

View(result_esperado_filiais)


## VAR ESPERADO  ============================================






## BASE GERAL =========================================

corder <- c("MES","EMPCODIGO","INDICADOR","VALOR")

dt_filiais <-
  union_all(
    result_filiais_2 %>% .[,corder],
    
    metas_filiais_2023_2 %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            alcance_result_filiais %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            result_esperado_filiais %>% .[,corder]) %>%
    
  as.data.frame()


dt_filiais <- dt_filiais %>% mutate(MES=as.character(MES))

dt_filiais <- dt_filiais %>% mutate(ID=seq(1:nrow(dt_filiais)))

view(dt_filiais)
