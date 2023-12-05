

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
  
  CLI AS (SELECT DISTINCT C.CLICODIGO, SETOR,ENDCODIGO
                             FROM CLIEN C
                               INNER JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                               INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                                E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                                   WHERE CLICLIENTE='S'),
  
  PEDEMIS AS (SELECT ID_PEDIDO,
                      PEDDTEMIS,
                       SETOR
                        FROM PEDID P
                         INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                          WHERE PEDDTEMIS 
                           BETWEEN '01.01.2023' AND '31.10.2023' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PEDBAIXA AS (SELECT ID_PEDIDO,
                       PEDDTBAIXA,
                        SETOR 
                         FROM PEDID P
                          INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                           WHERE PEDDTBAIXA 
                            BETWEEN '01.01.2023' AND '31.10.2023' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PDEMIS AS (SELECT PEDDTEMIS,
                     SETOR, 
                      SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                       FROM PDPRD PD
                        INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
                         INNER JOIN PEDEMIS ON PD.ID_PEDIDO=PEDEMIS.ID_PEDIDO
                          GROUP BY 1,2),
  
  PDBAIXA AS (SELECT PEDDTBAIXA,
                      SETOR, 
                       SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                        FROM PDPRD PD
                         INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
                          INNER JOIN PEDBAIXA ON PD.ID_PEDIDO=PEDBAIXA.ID_PEDIDO
                           GROUP BY 1,2)
  
  SELECT PEDDTEMIS DATA,
          SETOR,
           VRVENDA,
             'EMISSAO' AS TIPO
   FROM PDEMIS 
    UNION
    SELECT PEDDTBAIXA DATA,
            SETOR, 
             VRVENDA,
              'BAIXA' AS TIPO
     FROM PDBAIXA