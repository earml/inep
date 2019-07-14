rm(list=ls(all=T))
setwd("D:\\Pnad") 
dir() 

library(SAScii)
library(descr)
library(RSQLite)
library(downloader)
library(readr)
library(data.table)
library(sqldf)

#Sergipe <- subset(dados, UF==28)
system.time(
  dados_metas <- fread(input='PNADC_2016_5.csv', sep='|', sep2='|', integer64='double')
)

metas <- as.data.frame(dados_metas[,.(UF, V2007, V2009, V2010, V3001, V3002, V3002A, V3003A, V3004, V3005A, V3006, V3007, V3008, V3009A, 
                                      V3010, V3011A, V3012, V3013, V3014, V4010, V4014, VD3001, VD3002)])


selecao1<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE (V2009 > '005' AND V2009 < '015')"

selecao2<-"SELECT V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014
            FROM metas
            WHERE (V2009 > '005' AND V2009 < '015') AND V3002 = 1 
            AND
                  (
                    (V3003A = '04' AND V3004 = 1 AND (V3006 = '02' OR V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08'))
                    OR
                    (V3003A = '04' AND V3004 = 2 AND (V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08' OR V3006 = '09'))
                    OR
                    (V3003A = '05' AND (V3006 = '02' OR V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08'))
                    OR
                    (V3003A = '06' AND V3006 IN ('01','13'))
                    OR
                    (V3003A = '07' AND V3006 IN ('01', '13'))
                  )
              "
  

selecao3<-"SELECT V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014
            FROM metas
            WHERE (V2009 > '005' AND V2009 < '015') 
            AND
              (
                (
                  V3002 = 1 AND
                    (
                      (V3003A = '04' AND V3004 = 1 AND (V3006 = '02' OR V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08'))
                      OR
                      (V3003A = '04' AND V3004 = 2 AND (V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08' OR V3006 = '09'))
                      OR
                      (V3003A = '05' AND (V3006 = '02' OR V3006 = '03' OR V3006 = '04' OR V3006 = '05' OR V3006 = '06' OR V3006 = '07' OR V3006 = '08'))
                      OR
                      (V3003A = '06' AND V3006 IN ('01','13'))
                      OR
                      (V3003A = '07' AND V3006 IN ('01', '13'))
                    )
                )
                OR 
                (
                  V3002 = 2 AND
                  (
  
                    (V3008 = 1 AND V3009A = '05' AND V3012 = 1 AND (V3013 = '01' OR V3013 = '02' OR V3013 = '03' OR V3013 = '04' OR V3013 = '05' OR V3013 = '06'))
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 1 AND ((V3013 = '01' OR V3013 = '02' OR V3013 = '03') OR (V3013 = '04' AND V3014 = 2)))
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 3 AND V3014 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 1 AND V3012 = 1 AND (V3013 = '01' OR V3013 = '02' OR V3013 = '03' OR V3013 = '04' OR V3013 = '05' OR V3013 = '06' OR V3013 = '07'))
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 2 AND V3012 = 1 AND (V3013 = '02' OR V3013 = '03' OR V3013 = '04' OR V3013 = '05' OR V3013 = '06' OR V3013 = '07' OR V3013 = '08'))
                    OR
                    (V3008 = 1 AND V3009A = '08' AND V3012 = 1 AND (V3013 = '01' OR V3013 = '02' OR V3013 = '03' OR V3013 = '04' OR V3013 = '05' OR V3013 = '06' OR V3013 = '07'))
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 1 AND V3013 ='04' AND V3014 = 1)
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 1 AND V3013 ='05')
                    OR
                    (V3008 = 1 AND V3009A = '06' AND V3012 = 3 AND V3014 = 1)
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 1 AND V3012 = 1 AND V3013 = '08')
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 1 AND V3012 = 3 AND V3014 = 1)
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 2 AND V3012 = 1 AND V3013 = '09')
                    OR
                    (V3008 = 1 AND V3009A = '07' AND V3010 = 2 AND V3012 = 3 AND V3014 = 1)
                    OR
                    (V3008 = 1 AND V3009A = '08' AND V3012 = 1 AND V3013 = '08')
                    OR
                    (V3008 = 1 AND V3009A = '08' AND V3012 = 3 AND V3014 = 1)
                    OR
                    (V3008 = 1 AND V3009A = '09' AND V3012 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '09' AND V3012 = 3 AND V3014 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '10' AND V3012 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '10' AND V3012 = 3 AND V3014 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '11' AND V3012 = 2)
                    OR
                    (V3008 = 1 AND V3009A = '11' AND V3012 = 3 AND V3014 = 2)

                  )
                )
              )
"


selecao2<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001
                  FROM metas
                  WHERE (V2009 > '005' AND V2009 < '015') AND
                      (                  
                        V3002 = 1 AND
                          (V3003A IN ('04','05'))
                      OR VD3001=3  
                      )
"



saida1 <- as.data.frame(sqldf(selecao1))
saida2 <- as.data.frame(sqldf(selecao2))


brasil_denominador <- subset(saida1)
brasil_numerador <- subset(saida2,V3003A=='04' | V3003A=="05" | VD3001==3)
percentual_brasil<-dim(brasil_numerador)[1]/dim(brasil_denominador)[1]*100

nordeste_denominador <- subset(saida1,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida2,(V3003A=='04' | V3003A=="05" | VD3001==3) & (UF>=21 & UF<=29))
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}

write.table(saida1, file = "numerador.xlsx", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")
write.table(saida2, file = "denominador.xlsx", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")



#### gráfico 7 e 8 ####

selecao3<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE V2009 = '016'"


selecao4<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001
                  FROM metas
                  WHERE V2009 = '016' AND
                        VD3001 IN(3,4,5,6,7)  
"

saida3 <- as.data.frame(sqldf(selecao3))
saida4 <- as.data.frame(sqldf(selecao4))

percentual_brasil<-dim(saida4)[1]/dim(saida3)[1]*100

nordeste_denominador <- subset(saida3,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida4, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}



####      Meta 3     ####
##  gráfico 9 e 10 ##
selecao5<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE (V2009 > '014' AND V2009 < '018')"


selecao6<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001
                  FROM metas
                  WHERE (V2009 > '014' AND V2009 < '018') 
                  AND (V3002 = 1 OR (V3002 = 2 AND VD3001 = 3)) 
"

saida5 <- as.data.frame(sqldf(selecao5))
saida6 <- as.data.frame(sqldf(selecao6))

percentual_brasil<-dim(saida6)[1]/dim(saida5)[1]*100

nordeste_denominador <- subset(saida5,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida6, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}


####      Meta 3     ####
##  gráficos 11 e 12 ##

selecao7<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE (V2009 > '014' AND V2009 < '018')"


selecao8<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001
                  FROM metas
                  WHERE (V2009 > '014' AND V2009 < '018') 
                  AND ((V3002 = 1 AND V3003A IN ('06','07')) OR VD3001 = 5) 
"

saida7 <- as.data.frame(sqldf(selecao7))
saida8 <- as.data.frame(sqldf(selecao8))

percentual_brasil<-dim(saida8)[1]/dim(saida7)[1]*100

nordeste_denominador <- subset(saida7,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida8, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}


####      Meta 9     ####
##  gráficos 40 e 41 ##

selecao9<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE V2009 > '014'"


selecao10<-"SELECT UF, V2009, V3001, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001
                  FROM metas
                  WHERE V2009 > '014' 
                  AND V3001 = 1 
"

saida9 <- as.data.frame(sqldf(selecao9))
saida10 <- as.data.frame(sqldf(selecao10))

percentual_brasil<-dim(saida10)[1]/dim(saida9)[1]*100

nordeste_denominador <- subset(saida9,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida10, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}



####      Meta 9     ####
##  gráficos 44 e 45 ##

selecao11<-"SELECT UF, V2009, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014,VD3001
                  FROM metas
                  WHERE V2009 > '014'"

selecao12<-"SELECT UF, V2009, V3001, V3002, V3003A, V3004, V3006, V3008, V3009A, V3012, V3013, V3014, VD3001, VD3002
                   FROM metas
                   WHERE V2009 > '014' 
                   AND (V3001=2 AND VD3002 < '05') 
"

saida11 <- as.data.frame(sqldf(selecao11))
saida12 <- as.data.frame(sqldf(selecao12))

percentual_brasil<-dim(saida12)[1]/dim(saida11)[1]*100

nordeste_denominador <- subset(saida11,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida12, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}



####      Meta 12     ####
##  gráficos 51 a 56    ##
selecao12_1<-"SELECT UF, V2007, V2009, V2010, V3002, V3002A, V3003A
                  FROM metas
                  WHERE V2009 > '017' AND V2009 < '025'"

selecao12_2<-"SELECT UF, V2007, V2009, V2010, V3002, V3001, V3002, V3002A, V3003A, VD3001
                  FROM metas
                  WHERE (V2009 > '017' AND V2009 < '025') 
                  AND (V3003A = '08' OR VD3001 = 7)"
                  #AND (V3002=1 AND V3003A = '08')"

saida12_1 <- as.data.frame(sqldf(selecao12_1))
saida12_2 <- as.data.frame(sqldf(selecao12_2))

percentual_brasil<-dim(saida12_2)[1]/dim(saida12_1)[1]*100

nordeste_denominador <- subset(saida12_1,UF>=21 & UF<=29)
nordeste_numerador <- subset(saida12_2, UF>=21 & UF<=29)
percentual_nordeste<-dim(nordeste_numerador)[1]/dim(nordeste_denominador)[1]*100

for (i in 21:29) {
  percentual_estado<-dim(subset(nordeste_numerador,UF==i))[1]/dim(subset(nordeste_denominador,UF==i))[1]*100
  print(paste0(i," Percentual: ", percentual_estado))
  
}

