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



## METAS ==================================================

METAS_FILIAIS_2024 <-
  get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\SUPERSET\\BASES\\METAS_FILIAIS_2024.RData"))


## RESULT =======================================================

result_filiais_2 <- 
result_filiais %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(VALOR=sum(VRLIQUIDO)) %>% mutate(INDICADOR='VENDA LIQUIDA')


## METAS =======================================================

metas_filiais_2024_2 <-
  METAS_FILIAIS_2024 %>% 
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),EMPCODIGO) %>% 
  summarize(METAS=sum(METAS)) %>% as.data.frame() %>%
  mutate(INDICADOR='META MES ATUAL') 


## ALCANCE  ============================================

alcance_result_filiais <-
  left_join(result_filiais_2,
            METAS_FILIAIS_2024 %>% 
              filter(DATA==floor_date(Sys.Date(),"month")) %>% 
               select(EMPCODIGO,-DATA,METAS),by="EMPCODIGO") %>% 
  mutate(ALCANCE=round(((VALOR/METAS)),3)) %>% 
  select(-VALOR,-METAS) %>% 
  mutate(INDICADOR='ALCANCE') %>% 
  rename(VALOR=ALCANCE) %>% mutate(VALOR=round(VALOR,3))


## ESPERADO  ============================================

## feriados no ano

holidays <- data.frame(DATES=c(as.Date('2024-01-01'),
                               as.Date('2024-02-13'),
                               as.Date('2024-03-29'),
                               as.Date('2024-04-21'),
                               as.Date('2024-05-01'),
                               as.Date('2024-05-30'),
                               as.Date('2024-11-15'),
                               as.Date('2024-12-25')
                               
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
  metas_filiais_2024_2 %>% 
   filter(MES==floor_date(Sys.Date(),"month")) %>% 
    mutate(ALCANCE_ESPERADO=(METAS/as.numeric(num_days2))) %>% 
     mutate(ALCANCE_ESPERADO2=(ALCANCE_ESPERADO*as.numeric(days_until_yesterday))) %>% 
      mutate(ALCANCE_ESPERADO3=(ALCANCE_ESPERADO2/METAS)) %>% 
       mutate(INDICADOR='ALCANCE ESPERADO') %>% 
        select(EMPCODIGO,MES,ALCANCE_ESPERADO3,INDICADOR) %>% 
          rename(VALOR=ALCANCE_ESPERADO3)



## VAR ESPERADO  ============================================


var_esperado_filiais <- 
 left_join(alcance_result_filiais %>% 
            select(-INDICADOR),result_esperado_filiais %>% 
             select(-INDICADOR,-MES),by="EMPCODIGO") %>% 
              as.data.frame() %>% 
               mutate(VALOR=round(VALOR.x/VALOR.y-1,3)) %>%
                mutate(INDICADOR='VAR ALCANCE ESPERADO') %>% 
                 select(MES,EMPCODIGO,VALOR,INDICADOR) 


## BASE GERAL =========================================

corder <- c("MES","EMPCODIGO","INDICADOR","VALOR")

dt_filiais <-
  union_all(
    result_filiais_2 %>% .[,corder],
    
    metas_filiais_2024_2 %>% rename(VALOR=METAS) %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            alcance_result_filiais %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            result_esperado_filiais %>% .[,corder]) %>%
  
  union_all(. ,  
            
            var_esperado_filiais %>% .[,corder]) %>%
  
  rename(FILIAL=EMPCODIGO) %>% mutate(FILIAL=case_when(
    FILIAL==1 ~ 'MATRIZ',
    FILIAL==3 ~ 'JOINVILLE',
    FILIAL==4 ~ 'CRICIUMA',
    FILIAL==5 ~ 'CHAPECO',
    FILIAL==8 ~ 'BC',
    TRUE ~ as.character(FILIAL)
    )) %>% 
    
  as.data.frame() 


dt_filiais <- dt_filiais %>% mutate(MES=as.character(MES))

dt_filiais <- dt_filiais %>% mutate(ID=seq(1:nrow(dt_filiais))) 


view(dt_filiais)




## INSERT BANCO SUPERSET =========================================

# Se não houver dados, interromper a execução
if (nrow(dt_filiais) == 0) {
  cat("Nenhum dado para inserir.\n")
} else {
  # Verificar se a tabela existe no MariaDB
  nome_tabela <- "result_filiais"
  existe_tabela <- dbExistsTable(con_spset, nome_tabela)
  # Se a tabela não existir, criar automaticamente no MariaDB
  if(!existe_tabela) {
    dbWriteTable(con_spset, nome_tabela, dt_filiais, create = TRUE, row.names = FALSE)
    # Alterar a codificação da tabela para utf8mb4
    query_alteracao <- paste("ALTER TABLE", nome_tabela, 
                             "CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;")
    dbExecute(con_spset, query_alteracao)
  } else { # Se a tabela já existir, inserir os dados
    print(dt_filiais)
    #container
    y <- data.frame(MES=NA,FILIAL=NA,INDICADOR=NA,VALOR=NA,ID=NA)
    for (i in 1:nrow(dt_filiais)) {
      y[i,] <- dt_filiais[i,]
      query <- paste("INSERT IGNORE INTO ",nome_tabela," (MES,FILIAL,INDICADOR,VALOR,ID) VALUES ('",y[i,"MES"],"','",y[i,"FILIAL"],"','",y[i,"INDICADOR"],"',",y[i,"VALOR"],",",y[i,"ID"],") ON DUPLICATE KEY UPDATE ID=",y[i,"ID"],",MES='",y[i,"MES"],"',FILIAL='",y[i,"FILIAL"],"',INDICADOR='",y[i,"INDICADOR"],"',VALOR=",y[i,"VALOR"],";", dbcomercial = "")
      
      dbSendQuery(con_spset,query)
    }
  }
}




# Fechar conexões
dbDisconnect(con_spset)
dbDisconnect(con_sgo)
