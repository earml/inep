## ----------------------------------------------------- ##
##                     META 5                            ##
rm(list=ls(all=TRUE))
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

selecao <- c('ID_REGIAO','ID_UF','IN_PRESENCA_LP','IN_PRESENCA_MT','NIVEL_LPO','NIVEL_LPD','NIVEL_MT')

system.time(
  dadosAluno <- fread(input='TS_ALUNO-2014.CSV', sep=',', sep2=',', integer64='double',select = selecao)
)

gc()
uf<-as.vector(names(table(dadosAluno[,ID_UF])))

library(sqldf)
library(plyr)

brasil<- 'SELECT ID_REGIAO, ID_UF, NIVEL_LPO, NIVEL_LPD, NIVEL_MT
                   FROM dadosAluno'

leitura <- 'SELECT ID_REGIAO, ID_UF, NIVEL_LPO, NIVEL_LPD, NIVEL_MT
                   FROM dadosAluno
                   WHERE NIVEL_LPO IN (2,3,4)'

escrita <- 'SELECT ID_REGIAO, ID_UF, NIVEL_LPO, NIVEL_LPD, NIVEL_MT
                   FROM dadosAluno
                   WHERE NIVEL_LPD IN (4,5)'

matematica <- 'SELECT ID_REGIAO, ID_UF, NIVEL_LPO, NIVEL_LPD, NIVEL_MT
                   FROM dadosAluno
                   WHERE NIVEL_MT IN (3,4)'


#leitura <- 'SELECT ID_REGIAO, ID_UF, NIVEL_LPO, NIVEL_LPD, NIVEL_MT
#                   FROM dadosAluno
#                   WHERE NIVEL_LPO = 2 
#                   OR NIVEL_LPO=3
#                   OR NIVEL_LPO=4'

pais <- as.data.frame(sqldf(brasil))

grandes_regioes<-as.vector(names(table(pais[,'ID_UF'])))

for (i in grandes_regioes) {
  for (j in c(3,4,5)) {
    regiao <- na.omit(pais[,c(1,j)])
    regiao <- subset(regiao, ID_UF==i)
    
    if(j==3){
      regiao_proficiencia <- subset(regiao,NIVEL_LPO==2 | NIVEL_LPO==3 | NIVEL_LPO==4)
      leitura <- dim(regiao_proficiencia)[1]/dim(regiao)[1]*100
      print(paste0('Região: ',i,"  Leitura: ",leitura))
    }
    else if(j==4){
      regiao_proficiencia <- subset(regiao,NIVEL_LPD==4 | NIVEL_LPD==5)
      escrita <- dim(regiao_proficiencia)[1]/dim(regiao)[1]*100
      print(paste0('Região: ',i,"  Escrita: ",escrita))
    }
    else{
      regiao_proficiencia <- subset(regiao,NIVEL_MT==3 | NIVEL_MT==4)
      matematica <- dim(regiao_proficiencia)[1]/dim(regiao)[1]*100
      print(paste0('Região: ',i,"  Matemática: ",matematica))
    }
  }
}

grandes_regioes<-as.vector(names(table(pais[,'ID_REGIAO'])))
regiao <- na.omit(pais[,c(1,5)])
regiao_proficiencia <- subset(regiao,NIVEL_MT==3 | NIVEL_MT==4)
matematica <- dim(regiao_proficiencia)[1]/dim(regiao)[1]*100
print(paste0('Região: ',i,"  Matemática: ",matematica))

