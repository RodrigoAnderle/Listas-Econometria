
#LEITURA BASE DE DADOS RAIS V�NCULOS
RAIS <- read.csv2(
  "C:/Users/Qbex/Desktop/R/Lista de Econometria/BA2015.txt")
RAIS <- RAIS[,-(c(1:3,11))] #retirando vari�veis de SP...
RAIS <- RAIS[RAIS$Munic�pio == 292740,] ## s� Salvador
RAIS$GRANDE <- ifelse(RAIS$Tamanho.Estabelecimento > 6, 1, 0)
write.csv2(RAIS,"BASE_QUESTAO_2.csv")

########################################################
#Criando nova base de dados agregados por munic�pio
RAIS_MUN<-data.frame(MUN = as.factor(names(table(RAIS$Munic�pio))),
                 VINCULOS = as.numeric(table(RAIS$Munic�pio))
                 )

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

#Trabalhador por grau de instru��o
TEMP<- table(RAIS$Munic�pio, RAIS$...)
colnames(TEMP)<-c( "ANALFABETO",
                   "ATE 5.A INC",
                   "5.A CO FUND",
                   "6. A 9. FUND",
                   "FUND COMPL",
                   "MEDIO INCOMP",
                   "MEDIO COMPL",
                   "SUP. INCOMP",
                   "SUP. COMP",
                   "MESTRADO",
                   "DOUTORADO")
TEMP<- as.data.frame(cbind(MUN=rownames(TEMP),TEMP))
RAIS_MUN<- cbind(RAIS_MUN,TEMP[TEMP$MUN %in% RAIS_MUN$MUN,])
rm(TEMP)

#################################################################
## Leitura da Base de dados para quest�o 2
##############################################################
BASE2 <- read.csv2(RAIS,"BASE_QUESTAO_2.csv")






####
#LEITURA BASE DE DADOS RAIS ESTABELECIMENTOS
RAIS_ESTAB<-read.csv(
  "C:/Users/Qbex/Desktop/R/Lista de Econometria/ESTB2015.txt",sep=";",
  head=T, dec=",")

### Gr�ficos, ainda n�o deu certo
```{r, echo=TRUE}
BASE2$previsao <- predict.glm(logit1, type = "response")
BASE2$REM_UL <- ((exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade))/(1+exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade)))*0.000001342
BASE2$REM_LL <- -((exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade))/(1+exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade)))*0.000001342
BASE2$Idade_UL <- ((exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade))/(1+exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade)))*0.0001802
BASE2$Idade_LL <- -((exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade))/(1+exp(-0.825+0.009*BASE2$Vl.Remun.M�dia.Nom + 0.0003*BASE2$Idade)))*0.0001802
```
```{r}
library(ggplot2)
ggplot(BASE2, aes(x = Idade, y = previsao)) + 
  geom_line()

geom_ribbon(aes(ymin = Idade_LL,ymax = Idade_UL), alpha = 0.2)

```

