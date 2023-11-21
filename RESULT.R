## NEW RESULT SUPERSET
## 11.2023
## SANDRO JAKOSKA


library(DBI)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")



## RESULT ===============================================



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
     union_all(.,result_geral_baixa) %>% 
      mutate(INDICADOR='RESULTADO MES')

View(new_result2)


## METAS ==================================================

METAS_2023 <-
get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData"))

View(METAS_2023)


METAS_2023 %>% filter(SETOR=="GERAL") %>% summarize(V=sum(VALOR))


save(METAS_2023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\RESULT\\BASES\\METAS_2023.RData")

## EMISSAO
metas_setores_emissao <-
  METAS_2023 %>%  
   filter(DATA==floor_date(Sys.Date(),"month")) %>% 
    group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
     summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
      mutate(TIPO='EMISSAO') %>% 
       mutate(INDICADOR='META MES ATUAL') 


View(metas_setores_emissao)


metas_setores_baixa <-
  METAS_2023 %>%  
  filter(DATA==floor_date(Sys.Date(),"month")) %>% 
  group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
  summarize(VALOR=sum(VALOR)) %>% as.data.frame() %>%
  mutate(TIPO='BAIXA') %>% 
  mutate(INDICADOR='META MES ATUAL') 


metas_setores <-
   union_all(metas_setores_emissao,metas_setores_baixa)


View(metas_setores)



## ALCANCE ============================================

alcance_result <-
 left_join(new_result2,
            METAS_2023 %>%  
             filter(DATA==floor_date(Sys.Date(),"month")) %>% 
              group_by(MES=floor_date(DATA,"month"),SETOR) %>% 
               summarize(METAS=sum(VALOR)) %>% 
                as.data.frame() %>% select(-MES),by="SETOR") %>% 
                 mutate(ALCANCE=round(((VALOR/METAS)*100),2)) %>% 
                  select(-VALOR,-METAS) %>% 
                   mutate(INDICADOR='ALCANCE') %>% 
                    rename(VALOR=ALCANCE)


View(alcance_result)




## ESPERADO  ============================================

## CALC DIAS UTEIS

first_day <- floor_date(Sys.Date(), unit = "month")

last_day <- ceiling_date(Sys.Date(), 'month') - days(1)

num_days <- as.numeric(days_in_month(first_day))

num_weekends <- sum(wday(seq(first_day, last_day, by = "day")) %in% c(1,7))

holidays <- data.frame(DATA=c(as.Date('2023-10-12'),
                              as.Date('2023-11-02'),
                              as.Date('2023-11-15'),
                              as.Date('2023-12-25'),
                              as.Date('2023-12-31')
                              )) %>% 
                              mutate(MES=floor_date(DATA, unit = "month")) %>% 
                              filter(MES==floor_date(Sys.Date(), unit = "month")) %>% 
                              tally()
                              
                      

num_days = (num_days - num_weekends) - holidays 

days_until_yesterday <- length(seq(first_day,Sys.Date(),by = "day"))-1

## 


metas_setores %>% 
  mutate(ALCANCE_ESPERADO=(VALOR/as.numeric(num_days))*length(seq(first_day,Sys.Date(),by = "day"))-1) %>% View()




## BASE GERAL =========================================

corder <- c("MES","SETOR","TIPO","INDICADOR","VALOR")

base_result <-
union_all(
new_result2 %>% .[,corder],

metas_setores %>% .[,corder]) %>% 
  
union_all(. ,  

alcance_result  %>% .[,corder])  


View(base_result)


