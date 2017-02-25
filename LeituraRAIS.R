
#LEITURA BASE DE DADOS RAIS V�NCULOS
RAIS<-read.csv("C:/Users/Qbex/Desktop/R/Lista de Econometria/BA2015.txt",
               sep=";", head=T, dec=",")

RAIS<-RAIS[,-(c(1:3,11))] #retirando vari�veis de SP...
str(RAIS) #verficiando formatos
names(RAIS) #Nomes das vari�veis

table(RAIS$Munic�pio) #v�nculos por munic�pio
names(table(RAIS$Munic�pio)) #c�digos dos munic�pios dos estabelecimentos

#Criando nova base de dados agregados por munic�pio
RAIS_MUN<-data.frame(MUN = as.factor(names(table(RAIS$Munic�pio))),
                 VINCULOS = as.numeric(table(RAIS$Munic�pio))
                 )

#Local de trabalho dos v�nculos
TEMP<- data.frame(MUN = as.factor(names(table(RAIS$Mun.Trab))),
                  MUN_TRAB = as.numeric(table(RAIS$Mun.Trab))
                  )
RAIS_MUN$MUN_TRAB<- TEMP[TEMP$MUN %in% RAIS_MUN$MUN,2] 
## existem trabalhadores em outros munic�pios
TEMP[!(TEMP$MUN %in% RAIS_MUN$MUN),] 
rm(TEMP)

#Diferen�a entre trabalhadores locais e fora
RAIS_MUN$VINC_DIF<- RAIS_MUN[,2]-RAIS_MUN[,3]

#Trabalhadores na administra��o p�blica (PUBLICO)
TEMP<- with(RAIS,table(RAIS$Munic�pio,RAIS$CNAE.2.0.Classe == 84116))
TEMP<- data.frame(cbind(MUN=names(TEMP[,2]),PUBLICO=TEMP[,2]))
RAIS_MUN$PUBLICO<-TEMP[TEMP$MUN %in% RAIS_MUN$MUN,2]
rm(TEMP)

# Trabalhador por faixa de idade
TEMP<- table(RAIS$Munic�pio, RAIS$Faixa.Et�ria)
colnames(TEMP)<-c( "FE 10 A 14 anos",
                   "FE 15 A 17 anos",
                   "FE 18 A 24 anos",
                   "FE 25 A 29 anos",
                   "FE 30 A 39 anos",
                   "FE 40 A 49 anos",
                   "FE 50 A 64 anos",
                   "FE 65 anos ou mais",
                   "FE N�o class")
TEMP<- as.data.frame(cbind(MUN=rownames(TEMP),TEMP))
RAIS_MUN<- cbind(RAIS_MUN,TEMP[TEMP$MUN %in% RAIS_MUN$MUN,])
rm(TEMP)

########################### AINDA N�O EXECUTADO
head(RAIS_MUN)



table(RAIS$Munic�pio,RAIS$Causa.Afastamento.1)
names()<-c("ACI TRB TIP",
                    "ACI TRB TJT",
                    "DOEN REL TR",
                    "DOEN NREL TR",
                    "LIC MATERNID",
                    "SERV MILITAR",
                    "LIC SEM VENC",
                    "IGNORADO")
############################

#LEITURA BASE DE DADOS RAIS ESTABELECIMENTOS
RAIS_ESTAB<-read.csv(
  "C:/Users/Qbex/Desktop/R/Lista de Econometria/ESTAB2015.txt",sep=";",
  head=T, dec=",")
