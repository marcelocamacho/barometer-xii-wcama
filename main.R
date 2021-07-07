library(tidyr)
library(dplyr)
library(ggplot2)
require("ggrepel")
library(extrafont)
setwd('~/Desktop/barometro/barometeR/')
source("barometer.R")

data = read.csv(file = "./dataset-2.csv", sep = '|',dec = ',') #20 variáveis
edm=read.csv(file="../v1/edm2.csv", sep='|', dec = '.') #20 variáveis
#data = read.csv(file = "./dataset.csv", sep = '|',dec = ',') #20 variáveis
#edm=read.csv(file="./edm.csv", sep='|', dec = '.') #20 variáveis
ebs=read.csv(file = './ebs.csv',sep = '|',dec = '.')
data[,6:18]<-convert(data[,6:18])
data %>% select(-DIMENSAO,-TEMA,-FONTE_DE_DADOS,-UNIDADE) %>% gather(MUNICIPIO,valor,-INDICADOR) %>% spread(INDICADOR,valor) %>% select(-MUNICIPIO)
##corrigir indicador renda
#data[data$INDICADOR=="Renda per capita",] = data %>% 
#  filter(grepl('Renda',INDICADOR)) %>% 
#  summarise_all(., ~if(is.numeric(.) && .>2) ./1000 else .)

#write.table(data,file="dataset-2.csv",sep="|", row.names = F,dec = ',')

#Operação de cálculo do Barômetro da sustentabilidade
CASTANHAL=run(data$INDICADOR,data$CASTANHAL)
{
  SJOAODOARAGUAIA=run(data$INDICADOR,data$SJOAOARAGUAIA)
  SDOMINGOSARAGUAIA=run(data$INDICADOR,data$SDOMINGOSARAGUAIA)
  PARAUAPEBAS=run(data$INDICADOR,data$PARAUAPEBAS)
  MARABA=run(data$INDICADOR,data$MARABA)
  CURIONOPOLIS=run(data$INDICADOR,data$CURIONOPOLIS)
  CANAADOSCARAJAS=run(data$INDICADOR,data$CANAADOSCARAJAS)
  BREJOGRANDE=run(data$INDICADOR,data$BREJOGRANDE)
  SGERALDODOARAGUAIA=run(data$INDICADOR,data$SGERALDODOARAGUAIA)
  PIÇARRA=run(data$INDICADOR,data$PIÇARRA)
  PALESTINADOPARÁ=run(data$INDICADOR,data$PALESTINADOPARÁ)
  ELDORADODOSCARAJAS=run(data$INDICADOR,data$ELDORADODOSCARAJAS)
  BOMJESUSDOTOCANTINS=run(data$INDICADOR,data$BOMJESUSDOTOCANTINS)
}

BS=data[,c("DIMENSAO","TEMA","INDICADOR")]

#Operação de merge
BS<-merge(BS,CASTANHAL,by ="INDICADOR",sort = F,all.x = T,suffixes = "_CASTANHAL",no.dups = T)
{
  BS<-merge(BS,SJOAODOARAGUAIA,by ="INDICADOR",sort = F,all.x = T,suffixes = "_SJOAODOARAGUAIA",no.dups = T)
  BS<-merge(BS,SDOMINGOSARAGUAIA,by ="INDICADOR",sort = F,all.x = T,suffixes = "_SDOMINGOSARAGUAIA",no.dups = T)
  BS<-merge(BS,PARAUAPEBAS,by ="INDICADOR",sort = F,all.x = T,suffixes = "_PARAUAPEBAS",no.dups = T)
  BS<-merge(BS,MARABA,by ="INDICADOR",sort = F,all.x = T,suffixes = "_MARABA",no.dups = T)
  BS<-merge(BS,CURIONOPOLIS,by ="INDICADOR",sort = F,all.x = T,suffixes = "_CURIONOPOLIS",no.dups = T)
  BS<-merge(BS,CANAADOSCARAJAS,by ="INDICADOR",sort = F,all.x = T,suffixes = "_CANAADOSCARAJAS",no.dups = T)
  BS<-merge(BS,BREJOGRANDE,by ="INDICADOR",sort = F,all.x = T,suffixes = "_BREJOGRANDE",no.dups = T)
  BS<-merge(BS,SGERALDODOARAGUAIA,by ="INDICADOR",sort = F,all.x = T,suffixes = "_SGERALDODOARAGUAIA",no.dups = T)
  BS<-merge(BS,PIÇARRA,by ="INDICADOR",sort = F,all.x = T,suffixes = "_PIÇARRA",no.dups = T)
  BS<-merge(BS,PALESTINADOPARÁ,by ="INDICADOR",sort = F,all.x = T,suffixes = "_PALESTINADOPARÁ",no.dups = T)
  BS<-merge(BS,ELDORADODOSCARAJAS,by ="INDICADOR",sort = F,all.x = T,suffixes = "_ELDORADODOSCARAJAS",no.dups = T)
  BS<-merge(BS,BOMJESUSDOTOCANTINS,by ="INDICADOR",sort = F,all.x = T,suffixes = "_BOMJESUSDOTOCANTINS",no.dups = T)
}

BS<-BS[,c("DIMENSAO","TEMA","INDICADOR",
          "CASTANHAL","SJOAOARAGUAIA","SDOMINGOSARAGUAIA",
          "PARAUAPEBAS","MARABA","CURIONOPOLIS",
          "CANAADOSCARAJAS","BREJOGRANDE","SGERALDODOARAGUAIA",
          "PIÇARRA","PALESTINADOPARÁ","ELDORADODOSCARAJAS","BOMJESUSDOTOCANTINS"
)]

BS$DIMENSAO<-as.factor(BS$DIMENSAO)
BS$TEMA<-as.factor(BS$TEMA)
BS$INDICADOR<-as.factor(BS$INDICADOR)
BS[,4:ncol(BS)]<-convert(BS[,4:ncol(BS)],type = "numeric")


df<-data %>% 
  gather(municipio,valor,-c(DIMENSAO,TEMA,FONTE_DE_DADOS,INDICADOR,UNIDADE)) %>% 
  select(!c(FONTE_DE_DADOS,UNIDADE)) %>% 
  inner_join(BS %>% 
               gather(municipio,bs,-c(DIMENSAO,TEMA,INDICADOR))
             )

df$DIMENSAO[df$DIMENSAO=='Bem-estar humano']='Humano'
df$DIMENSAO[df$DIMENSAO=='Ecosistêmico']='Ecosistemico'

df %>% 
  group_by(DIMENSAO,TEMA,municipio) %>% 
  summarise(v=mean(as.numeric(valor)),bs=mean(bs)) 

df.1<-df %>% 
#  filter(DIMENSAO=="Humano") %>% 
  group_by(DIMENSAO,TEMA,municipio) %>% 
  summarise(bs=mean(bs))
df.1
xtabs(round(bs,2) ~ municipio+TEMA,data=df.1)

df.2<-df %>% 
  #  filter(DIMENSAO=="Humano") %>% 
  group_by(DIMENSAO,municipio) %>% 
  summarise(bs=mean(bs))
xtabs(bs ~ municipio+DIMENSAO,df.2)

bs<-df.2 %>% 
  select(municipio,DIMENSAO,bs) %>% 
  spread(DIMENSAO,bs)

escala<-data.frame(
  div=c(0,20,40,60,80),
  posi=c(10,30,50,70,90),
  labels=c("Insustentável","P. insustentável","Intermediário", "P. sustentável","Sustentável"))

ggplot(bs,aes(x=Humano,y=Ecosistemico)) +
  geom_vline(xintercept=c(20,40,60,80),alpha=0.2,col="red")+
  geom_hline(yintercept=c(20,40,60,80),alpha=0.2,col="red")+
  geom_label_repel(aes(label=(
    paste0("(",round(Humano,2),",",round(Ecosistemico,2),")"))),
    size=2,family = "Times New Roman",force = 1,box.padding = 0.5, max.overlaps = 10) +
  geom_point(size=3,aes(shape=municipio)) +
  scale_x_continuous(expand = c(0, 0),breaks = c(10,20,30,40,50,60,70,80,90),limits = c(0,100))+
  scale_y_continuous(expand = c(0, 0),breaks = c(10,20,30,40,50,60,70,80,90),limits = c(0,100))+
  geom_text(data=escala,aes(x=div,y=1,
    label = labels,angle=90, vjust="right",hjust="bottom", family = "Times New Roman"))+
  geom_text(data=escala,aes(x=1,y=div,
    label = labels, vjust="bottom",hjust="left",family = "Times New Roman"))+
  scale_shape_manual(values = c(1:14))+
  labs(title="Barômetro da sustentabilidade", x="Dimensão humana",y="Dimensão ecosistêmica",
    caption = "Fonte: Elaborado pelos autores")+ 
  theme(plot.title = element_text(vjust=0.5,hjust=0.5),
    plot.caption = element_text(vjust=0.5,hjust=0),
    text=element_text(family="Times New Roman", face="bold", size=12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.background = element_rect(fill="white",color = "black") )


