## 1 - limpo a memória do R 
rm(list=ls(all=TRUE))
#docente<-read.csv2("C://Users//earibeiro//Desktop//Educacao//micro_censo_escolar_2016//DADOS//DOCENTES_NORDESTE.CSV",sep="|")

## ----------------------------------------------------- ##
##                     META 10                           ##

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
selecao <- c('NU_IDADE','NU_DURACAO_TURMA','NU_DUR_ATIV_COMP_MESMA_REDE','NU_DUR_ATIV_COMP_OUTRAS_REDES',
             'TP_SEXO','IN_EJA','IN_PROFISSIONALIZANTE','TP_ETAPA_ENSINO','CO_UF','TP_DEPENDENCIA')

system.time(
  dadosMatriculas <- fread(input='MATRICULA_NORDESTE-2017.CSV', sep='|', sep2='|', integer64='double')
)


selecao <- c()
gc()
uf<-as.vector(names(table(dadosMatriculas[,CO_UF])))
estados<-dadosMatriculas
nordeste<-subset(estados,(TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 
                          | TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==39))
nordeste<-subset(estados,TP_ETAPA_ENSINO==71)

sergipe<-subset(nordeste,CO_UF==28)
sergipe2<-subset(estados,CO_UF==28 & TP_ETAPA_ENSINO==74)
for (i in uf){
  estados<-subset(dadosMatriculas,CO_UF==i)
  eja_fundamental<-subset(estados,(TP_ETAPA_ENSINO==65 | TP_ETAPA_ENSINO==69 | TP_ETAPA_ENSINO==70))
  eja_medio<-subset(estados,TP_ETAPA_ENSINO==71)
  eja_fundamental_prof<-subset(estados,TP_ETAPA_ENSINO==65 | TP_ETAPA_ENSINO==73)
  eja_medio_prof<-subset(estados,TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==74)
  dim(eja_fundamental_prof)[1]
  dim(eja_fundamental)[1]
  dim(eja_medio_prof)[1]
  dim(eja_medio)[1]
  
  perc_eja_fundamental_prof<-dim(eja_fundamental_prof)[1]/dim(eja_fundamental)[1]*100
  perc_eja_medio_prof<-dim(eja_medio_prof)[1]/dim(eja_medio)[1]*100
  
  print(paste0(i,"   ",'Perc. Fund.: ',perc_eja_fundamental_prof,'  Perc. Méd.: ',perc_eja_medio_prof))
  
}

gc()

fund_par_2016<-c(44598,1966,7275,5543,2530)
fund_par_2017<-c(9168,248,719,1365,236)
fund_tot_2016<-c(942632,213407,275354,536996,119533)
fund_tot_2017<-c(1018351,229003,279881,508929,126271)

funbr2016<-sum(fund_par_2016)/sum(fund_tot_2016)*100;funbr2016
funbr2017<-sum(fund_par_2017)/sum(fund_tot_2017)*100;funbr2017

med_par_2016<-c(23883,1701,3328,2839,2751)
med_par_2017<-c(32928,1769,2466,2702,2901)
med_tot_2016<-c(350727,166741,137723,577961,108985)
med_tot_2017<-c(355768,180060,135249,581077,130892)

medbr2016<-sum(med_par_2016)/sum(med_tot_2016)*100;medbr2016
medbr2017<-sum(med_par_2017)/sum(med_tot_2017)*100;medbr2017




## ----------------------------------------------------- ##
##                     META 15                           ##

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

selecao <- c('CO_PESSOA_FISICA','NU_IDADE','TP_SEXO','IN_NECESSIDADE_ESPECIAL','TP_ESCOLARIDADE','TP_NORMAL_MAGISTERIO',
             'CO_AREA_CURSO_1','CO_CURSO_1','IN_LICENCIATURA_1','CO_AREA_CURSO_2',
             'CO_CURSO_2','IN_LICENCIATURA_2','TP_TIPO_IES_2','CO_AREA_CURSO_3',
             'CO_CURSO_3','IN_LICENCIATURA_3','TP_TIPO_IES_3','IN_ESPECIALIZACAO',
             'IN_MESTRADO','IN_DOUTORADO','IN_POS_NENHUM','TP_TIPO_DOCENTE','TP_TIPO_CONTRATACAO','TP_TIPO_TURMA',
             'TP_MEDIACAO_DIDATICO_PEDAGO','IN_ESPECIAL_EXCLUSIVA','IN_REGULAR','IN_EJA',
             'IN_PROFISSIONALIZANTE','TP_ETAPA_ENSINO','CO_CURSO_EDUC_PROFISSIONAL','CO_UF','TP_DEPENDENCIA',
             'TP_LOCALIZACAO')

system.time(
  dadosDocente <- fread(input='DOCENTES_CO-2017.CSV', sep='|', sep2='|', integer64='double',select = selecao)
)

gc()
uf<-as.vector(names(table(dadosDocente[,CO_UF])))
estados<-dadosDocente

library(sqldf)
nordeste_educa_basica<-"SELECT CO_PESSOA_FISICA,TP_ESCOLARIDADE, IN_ESPECIALIZACAO,
                               IN_MESTRADO, IN_DOUTORADO, TP_TIPO_DOCENTE, TP_TIPO_TURMA, CO_UF
                               FROM estados
                               WHERE TP_TIPO_TURMA NOT IN (4,5) AND TP_TIPO_DOCENTE IN (1,5)"

estados <- as.data.frame(sqldf(nordeste_educa_basica))

for (i in uf) {
  tot_professor_duplicado<-subset(estados,CO_UF==i)
  unico_professor<-tot_professor_duplicado[!duplicated(tot_professor_duplicado[,1]), ]
  professor_nivel_superior<-subset(unico_professor,TP_ESCOLARIDADE==4)
  percentual<-dim(professor_nivel_superior)[1]/dim(unico_professor)[1]*100
  
  print(paste0(i,"   ",percentual))
}


## ----------------------------------------------------- ##
##                     META 16                           ##

for (i in uf) {
  tot_professor_duplicado<-subset(estados,CO_UF==i)
  unico_professor<-tot_professor_duplicado[!duplicated(tot_professor_duplicado[,1]), ]
  professor_nivel_superior<-subset(unico_professor,TP_ESCOLARIDADE==4 & (IN_ESPECIALIZACAO==1 | IN_MESTRADO==1 | IN_DOUTORADO==1))
  percentual<-dim(professor_nivel_superior)[1]/dim(unico_professor)[1]*100
  print(paste0(i,"   ",percentual))
}



##### NORDESTE #####
unico_nordeste<-estados[!duplicated(estados[,1]), ];dim(unico_nordeste)
professor_nivel_superior<-subset(unico_nordeste,TP_ESCOLARIDADE==4);dim(professor_nivel_superior)
percentual<-dim(professor_nivel_superior)[1]/dim(unico_nordeste)[1]*100

print(paste0(i,"   ",percentual))

##### BRASIL #####
