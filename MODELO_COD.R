library(DBI)

# Conectar ao banco de dados Firebird
con <- dbConnect(odbc::odbc(), "dbcomercial", encoding = "UTF-8")

# Conectar ao banco de dados MariaDB
con2 <- dbConnect(odbc::odbc(), "Repro_T.I")

# Realizar consulta no Firebird
dt <- dbGetQuery(con2, "SELECT DISTINCT REPLACE(ad.id_pedido, ',', '') AS id_pedido, 
                        SUBSTR(ad.apobs, 24, 31) AS opticlick,
                        'N' AS statusopt,
                        LPAD(EXTRACT(YEAR FROM ad.apdata), 4, '0') || '.' ||
                        LPAD(EXTRACT(MONTH FROM ad.apdata), 2, '0') || '.' ||
                        EXTRACT(DAY FROM ad.apdata) AS datapd
                  FROM acoped ad
                  INNER JOIN (SELECT * FROM pedroteiro ) po 
                  ON ad.id_pedido = po.id_pedido
                  WHERE ad.lpcodigo = '1810' and ad.apdata BETWEEN CURRENT_DATE - 1 AND CURRENT_DATE")

# Verificar os primeiros registros de dt
print(head(dt))

# Se não houver dados, interromper a execução
if (nrow(dt) == 0) {
  cat("Nenhum dado para inserir.\n")
} else {
  # Verificar se a tabela existe no MariaDB
  nome_tabela <- "result"
  existe_tabela <- dbExistsTable(con, nome_tabela)
  # Se a tabela não existir, criar automaticamente no MariaDB
  if(!existe_tabela) {
    dbWriteTable(con, nome_tabela, dt, create = TRUE, row.names = FALSE)
    # Alterar a codificação da tabela para utf8mb4
    query_alteracao <- paste("ALTER TABLE", nome_tabela, 
                             "CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;")
    dbExecute(con, query_alteracao)
  } else { # Se a tabela já existir, inserir os dados
    print(dt)
    #container
    x <- data.frame(id_pedido=NA,opticlick=NA, statusopt=NA,datapd=NA)
    for (i in 1:nrow(dt)) {
      x[i,] <- dt[i,]
      query <- paste("INSERT IGNORE INTO testepedidos (id_pedido,opticlick,statusopt,datapd) VALUES (",x[i,"id_pedido"],",'",x[i,"opticlick"],"','",x[i,"statusopt"],"','",x[i,"datapd"],"');", dbopt = "")
      dbSendQuery(con,query)
      
    }
  }
}
}

# Fechar conexões
dbDisconnect(con)
dbDisconnect(con2)