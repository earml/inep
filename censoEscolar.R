## 1 - limpo a memória do R 
rm(list=ls(all=TRUE))
#docente<-read.csv2("C://Users//earibeiro//Desktop//Educacao//micro_censo_escolar_2016//DADOS//DOCENTES_NORDESTE.CSV",sep="|")

## ----------------------------------------------------- ##
##                     META 6                            ##

library(bit64)
library(data.table)
library(descr)
library(reshape)
library(survey)
library(ineq)
library(rJava)
library(xlsx)

# 2 - Indique o diretório de trabalho 
setwd('D:\\Educacao\\censo_escolar\\')
#D:\\Educacao\\censo_escolar\\MATRICULA_NORDESTE-2014.CSV
#D:\Educacao\micro_censo_escolar_2016\micro_censo_escolar_2016\DADOS\ESCOLAS.CSV
system.time(
dadosMatriculas <- fread(input='MATRICULA_NORDESTE-2017.CSV', sep='|', sep2='|', integer64='double')
)
#queryMatriculas <- "SELECT NU_DURACAO_TURMA,NU_DUR_ATIV_COMP_MESMA_REDE,NU_DUR_ATIV_COMP_OUTRAS_REDES,
#                           NU_DUR_AEE_MESMA_REDE,NU_DUR_AEE_OUTRAS_REDES,TP_DEPENDENCIA, CO_UF
#                           FROM dadosMatriculas"
gc()
#library(sqldf)
#Matriculas <- sqldf(queryMatriculas)

#https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
Matriculas <- dadosMatriculas[,.(NU_DURACAO_TURMA,NU_DUR_ATIV_COMP_MESMA_REDE,NU_DUR_ATIV_COMP_OUTRAS_REDES,
                                  TP_DEPENDENCIA, CO_UF)]

uf<-as.vector(names(table(Matriculas[,5])))
#uf <- c(21,22,23,24,25,26,27,28,29)
somatot<-0
somapar<-0
for (i in uf){
  estados<-subset(Matriculas,CO_UF==i & (TP_DEPENDENCIA==1 | TP_DEPENDENCIA==2 | TP_DEPENDENCIA==3))
  #estados[is.na(estados)] = 0
  estados<-na.omit(estados)
  tot_min <- estados[,1]+estados[,2]+estados[,3] #+estados[,4]+estados[,5]
  integral <- ifelse(tot_min>=420,1,0)
  total<-table(integral);total<-sum(total)
  parcial<-table(integral);parcial<-parcial[2]
  percentual<-prop.table(table(integral)); percentual<-percentual[2]*100
  print(paste0(i,"   ",total,"   ",parcial,"   ",percentual))
  
  somatot<-somatot +total
  somapar<-somapar + parcial
  print(paste0('soma total:  ',"  ",somatot,'soma parcial:  ',somapar))
  
}

totalfin<-c(591872,746988,1147716,460556)
parcfin<-c(63083,68726,135648,24128)


write.table(sergipeDocentes, file = "DOCENTES_SE_2016.csv", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")
write.table(sergipeEscolas, file = "ESCOLAS_SE_2016.csv", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")
write.table(sergipeMatriculas, file = "MATRICULAS_SE_2016.csv", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")
write.table(sergipeTurmas, file = "TURMAS_SE_2016.csv", sep = ";",row.names = F,col.names = TRUE,na = "NA",dec = ",")



selecao <- c('CO_UF','TP_DEPENDENCIA','NU_DURACAO_TURMA','NU_DUR_ATIV_COMP_MESMA_REDE','NU_DUR_ATIV_COMP_OUTRAS_REDES')
system.time(
dadosMatriculas <- fread(input='MATRICULA_SUDESTE-2016.CSV', sep='|', sep2='|', integer64='double',select = selecao)
)


### SPARK  ###
#http://spark.rstudio.com/reference/spark_read_csv/
#https://docs.databricks.com/user-guide/faq/sparklyr.html#sparklyr
#https://docs.databricks.com/spark/latest/sparkr/overview.html
#https://spark.apache.org/docs/latest/sparkr.html
#http://spark.rstudio.com/
#https://blog.rstudio.com/2016/09/27/sparklyr-r-interface-for-apache-spark/

