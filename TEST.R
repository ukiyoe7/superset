## TEST

library(DBI)
library(tidyverse)
library(readr)

con5 <- dbConnect(odbc::odbc(), "dbcomercial",encoding="Latin1")


dbGetQuery(con5,"SELECT * FROM testepedidos") %>% View()
