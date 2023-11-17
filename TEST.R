## TEST

library(DBI)
library(tidyverse)
library(readr)

con5 <- dbConnect(odbc::odbc(), "dbcomercial",encoding="Latin1")


library(DBI)

# Conectar ao banco de dados MariaDB
con6 <- dbConnect(odbc::odbc(), "Repro_T.I")

