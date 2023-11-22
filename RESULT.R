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

write.csv2(new_result,file = "new_result.csv")

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



## REACH  ============================================

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




## EXPECTED  ============================================

## holidays in a year

holidays <- data.frame(DATES=c(as.Date('2023-10-12'),
                               as.Date('2023-11-02'),
                               as.Date('2023-11-15'),
                               as.Date('2023-12-25'),
                               as.Date('2023-12-31')))

## calc working days month

first_day <- floor_date(Sys.Date(), unit = "month")

last_day <- ceiling_date(Sys.Date(), 'month') - days(1)

num_days <- as.numeric(days_in_month(first_day))

num_weekends <- sum(wday(seq(first_day, last_day, by = "day")) %in% c(1,7))


working_days <- 
  data.frame(
    DATES=seq(first_day, last_day, by = "day")) 


## exclude holidays
working_days_no_holidays <-
  anti_join(working_days,holidays,by="DATES") %>% tally()  

## exclude weekends
                            
num_days = (working_days_no_holidays - num_weekends) 


## calc days mtd

dyesterday <- Sys.Date()-1

days_mtd <- 
data.frame(
DATES=seq(first_day, dyesterday, by = "day")) 


## exclude holidays
days_mtd_no_holidays <-
anti_join(days_mtd,holidays,by="DATES") %>% tally()

## weekends
num_weekends_mtd <- sum(wday(seq(first_day, dyesterday, by = "day")) %in% c(1,7))

days_until_yesterday <- days_mtd_no_holidays - num_weekends_mtd 

## 

result_esperado_setores <-
metas_setores %>% 
  mutate(ALCANCE_ESPERADO=(VALOR/as.numeric(num_days))) %>% 
   mutate(ALCANCE_ESPERADO2=(ALCANCE_ESPERADO*as.numeric(days_until_yesterday))) %>% 
   mutate(VAR_ALCANCE_ESPERADO=(ALCANCE_ESPERADO2/VALOR)*100) 

View(result_esperado_setores)


## alcance esperado 

result_esperado_setores2 <- 
left_join(alcance_result,
result_esperado_setores %>% 
  as.data.frame() %>% 
  select(SETOR,TIPO,ALCANCE_ESPERADO3),by=c("SETOR","TIPO")) %>%
  mutate(ALCANCE_ESPERADO=round(VALOR/ALCANCE_ESPERADO3-1,3)*100) %>% 
   mutate(INDICADOR='ALCANCE ESPERADO') %>% 
    mutate(VALOR=ALCANCE_ESPERADO3) %>% select(-ALCANCE_ESPERADO3,-ALCANCE_ESPERADO)

View(result_esperado_setores2)


## var alcance esperado 


var_esperado_setores <- 
  left_join(alcance_result,
            result_esperado_setores %>% 
              as.data.frame() %>% 
              select(SETOR,TIPO,ALCANCE_ESPERADO3),by=c("SETOR","TIPO")) %>%
  mutate(ALCANCE_ESPERADO=round(VALOR/ALCANCE_ESPERADO3-1,3)*100) %>% 
  mutate(INDICADOR='ALCANCE ESPERADO') %>% 
  mutate(VALOR=ALCANCE_ESPERADO3) %>% select(-ALCANCE_ESPERADO3,-ALCANCE_ESPERADO)

View(var_esperado_setores)




## BASE GERAL =========================================

corder <- c("MES","SETOR","TIPO","INDICADOR","VALOR")

base_result <-
union_all(
new_result2 %>% .[,corder],

metas_setores %>% .[,corder]) %>% 
  
union_all(. ,  

alcance_result  %>% .[,corder])  


View(base_result)


