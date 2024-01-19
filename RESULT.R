##  RESULT SUPERSET
## 11.2023
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

new_result <- dbGetQuery(con_sgo, statement = read_file("C:\\Users\\REPRO SANDRO\\Documents\\R\\SUPERSET\\RESULT.sql")) 


## EMISSAO

result_setores_emissao <-
  new_result %>%  
   mutate(TIPO=str_trim(TIPO)) %>% 
    filter(TIPO=="EMISSAO") %>% 
     group_by(MES=floor_date(DATA,"month"),SETOR,TIPO) %>% 
      summarize(VALOR=sum(VRVENDA)) 

write.csv2(result_setores_emissao,file = "result_setores_emissao.csv")


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
     union_all(.,result_geral_baixa) %>% 
      mutate(INDICADOR='RESULTADO MES') %>% mutate(VALOR=round(VALOR,2))


## METAS ==================================================

METAS_2024 <-
  get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\SUPERSET\\BASES\\METAS_2024.RData"))


## EMISSAO
metas_setores_emissao <-
  METAS_2024 %>%  
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
  summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
  mutate(TIPO='EMISSAO') %>% 
  mutate(INDICADOR='META MES ATUAL') 


metas_setores_baixa <-
  METAS_2024 %>%  
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
  summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
  mutate(TIPO='BAIXA') %>% 
  mutate(INDICADOR='META MES ATUAL') 


metas_setores <-
  union_all(metas_setores_emissao,metas_setores_baixa) %>% 
  mutate(VALOR=round(VALOR,2))


## ALCANCE  ============================================

alcance_result <-
  left_join(new_result2,
            METAS_2024 %>%  
              filter(DATA==floor_date(Sys.Date(),"month")) %>% 
              group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
              summarize(METAS=sum(VALOR)) %>% 
              as.data.frame() %>% select(-MES),by="SETOR") %>% 
  mutate(ALCANCE=round(((VALOR/METAS)),3)) %>% 
  select(-VALOR,-METAS) %>% 
  mutate(INDICADOR='ALCANCE') %>% 
  rename(VALOR=ALCANCE) %>% mutate(VALOR=round(VALOR,3))


## ESPERADO  ============================================

## feriados no ano

holidays <- data.frame(DATES=c(as.Date('2024-01-01'),
                               as.Date('2024-02-12'),
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

result_esperado_setores <-
  metas_setores %>% 
  mutate(ALCANCE_ESPERADO=(VALOR/as.numeric(num_days2))) %>% 
  mutate(ALCANCE_ESPERADO2=(ALCANCE_ESPERADO*as.numeric(days_until_yesterday))) %>% 
  mutate(ALCANCE_ESPERADO3=(ALCANCE_ESPERADO2/VALOR)) 


result_esperado_setores2 <- 
  left_join(alcance_result,
            result_esperado_setores %>% 
              as.data.frame() %>% 
              select(SETOR,TIPO,ALCANCE_ESPERADO3),by=c("SETOR","TIPO")) %>%
  mutate(ALCANCE_ESPERADO=round(VALOR/ALCANCE_ESPERADO3-1,3)) %>% 
  mutate(INDICADOR='ALCANCE ESPERADO') %>% 
  mutate(VALOR=ALCANCE_ESPERADO3) %>% 
  select(-ALCANCE_ESPERADO3,-ALCANCE_ESPERADO) %>% 
  mutate(VALOR=round(VALOR,3))


## var alcance esperado 


var_esperado_setores <- 
  left_join(alcance_result,
            result_esperado_setores %>% 
              as.data.frame() %>% 
              select(SETOR,TIPO,ALCANCE_ESPERADO3),by=c("SETOR","TIPO")) %>%
  mutate(ALCANCE_ESPERADO=round(VALOR/ALCANCE_ESPERADO3-1,3)) %>% 
  mutate(INDICADOR=' VAR ALCANCE ESPERADO') %>% 
  mutate(VALOR=ALCANCE_ESPERADO) %>% 
  select(-ALCANCE_ESPERADO3,-ALCANCE_ESPERADO) %>% 
  mutate(VALOR=round(VALOR,3))

## BASE GERAL =========================================

corder <- c("MES","SETOR","TIPO","INDICADOR","VALOR")

dt <-
  union_all(
    new_result2 %>% .[,corder],
    
    metas_setores %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            alcance_result  %>% .[,corder])  %>% 
  
  union_all(. ,  
            
            result_esperado_setores2 %>% .[,corder]) %>% 
  
  union_all(. ,  
            
            var_esperado_setores %>% .[,corder])  %>% 
  
  as.data.frame()


dt <- dt %>% mutate(MES=as.character(MES))

dt <- dt %>% mutate(ID=seq(1:nrow(dt)))


## INSERT BANCO SUPERSET =========================================

# Se não houver dados, interromper a execução
if (nrow(dt) == 0) {
  cat("Nenhum dado para inserir.\n")
} else {
  # Verificar se a tabela existe no MariaDB
  nome_tabela <- "result"
  existe_tabela <- dbExistsTable(con_spset, nome_tabela)
  # Se a tabela não existir, criar automaticamente no MariaDB
  if(!existe_tabela) {
    dbWriteTable(con_spset, nome_tabela, dt, create = TRUE, row.names = FALSE)
    # Alterar a codificação da tabela para utf8mb4
    query_alteracao <- paste("ALTER TABLE", nome_tabela, 
                             "CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;")
    dbExecute(con_spset, query_alteracao)
  } else { # Se a tabela já existir, inserir os dados
    print(dt)
    #container
    y <- data.frame(MES=NA,SETOR=NA,TIPO=NA,INDICADOR=NA,VALOR=NA,ID=NA)
    for (i in 1:nrow(dt)) {
      y[i,] <- dt[i,]
      query <- paste("INSERT IGNORE INTO ",nome_tabela," (MES,SETOR,TIPO,INDICADOR,VALOR,ID) VALUES ('",y[i,"MES"],"','",y[i,"SETOR"],"','",y[i,"TIPO"],"','",y[i,"INDICADOR"],"',",y[i,"VALOR"],",",y[i,"ID"],") ON DUPLICATE KEY UPDATE ID=",y[i,"ID"],",MES='",y[i,"MES"],"',SETOR='",y[i,"SETOR"],"',TIPO='",y[i,"TIPO"],"',INDICADOR='",y[i,"INDICADOR"],"',VALOR=",y[i,"VALOR"],";", dbcomercial = "")
      
      dbSendQuery(con_spset,query)
    }
  }
}




# Fechar conexões
dbDisconnect(con_spset)
dbDisconnect(con_sgo)




