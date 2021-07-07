setwd('~/Desktop/barometro/barometeR/')
source("barometer.R")
library(tidyr)
library(dplyr)
library(ggplot2)
require("ggrepel")
library(extrafont)
library(tidyverse) # Para manipular os dados
library(stats) # Para PCA
library(factoextra) # Para criar alguns gráficos
library(FactoMineR)
library(gt)


data = read.csv(file = "./dataset-2.csv", sep = '|',dec = ',') #20 variáveis
edm=read.csv(file="../v1/edm2.csv", sep='|', dec = '.') #20 variáveis
ebs=read.csv(file = './ebs.csv',sep = '|',dec = '.')
dataLong=read.csv(file="dataset_completo.csv",sep=",",dec='.')

df.humano.vlr<-dataLong %>% 
  filter(DIMENSAO=='Humano') %>%
  select(-bs,-DIMENSAO,-TEMA,-X) %>% 
  group_by(municipio) %>% 
  spread(.,INDICADOR,valor) %>%
  ungroup(.) %>%
  mutate(vlrMean=rowMeans(select(.,-municipio)))

df.ambiental.vlr<-dataLong %>% 
  filter(DIMENSAO=='Ecosistemico') %>%
  select(-bs,-DIMENSAO,-TEMA,-X) %>% 
  group_by(municipio) %>% 
  spread(.,INDICADOR,valor) %>%
  ungroup(.) %>%
  mutate(vlrMean=rowMeans(select(.,-municipio)))

factoresHumanos<- names(df.humano.vlr) %>% 
  janitor::make_clean_names( parsing_option = 3,
                             replace = c('mortalidade'='mort','escolar'='escol','Número'='num',
                                         'Trabalho'='trab','Produto'='prod','infantil'='inf',
                                         'per capita'='percap','atividade'='atvdade','Interno'='inter',
                                         'Mortalidade'='mort','Infancia'='inf','infância e adolescência'='inf',
                                         'mulheres' = 'mul','Fundamental'='fund')) 
factoresHumanos

factoresAmbientais<-names(df.ambiental.vlr) %>%
  janitor::make_clean_names(parsing_option = 3,replace=c('População'='pop','domicílios'='dom',
                                                         'banheiro'='ban'))
factoresAmbientais


#Dataset para correlação
#df.ambiental.vlr[,c(fs_cor(df.ambiental.vlr[,-1],6))] %>% rename_with(janitor::make_clean_names(parsing_option = "3")) %>% gather(.,key = INDICADOR,value = vlr,-municipio)
fs<-function(dataset,method,cutoff){
  colunas=do.call(method,list(dataset[,-1],cutoff=cutoff))
  colunas=colunas[which(colunas!=1)]
  df= dataset[,c(1,colunas)] %>%
  gather(.,key = INDICADOR,value = vlr,-municipio) %>%
  spread(.,municipio,vlr)
  bs=apply(df[,-1],2,run,x.indicator=df$INDICADOR)
  df<-do.call(cbind,bs) %>% 
    select(-contains("INDICADOR"),1)
  names(df)<-c("BOMJESUSDOTOCANTINS","BREJOGRANDE",
                 "CANAADOSCARAJAS","CASTANHAL","CURIONOPOLIS",
                "ELDORADODOSCARAJAS","MARABA","PALESTINADOPARÁ",
                "PARAUAPEBAS","PIÇARRA","SDOMINGOSARAGUAIA","SGERALDODOARAGUAIA",
                "SJOAOARAGUAIA","INDICADOR")
  return(df)
}
sim<-function(hum,amb,threshold=3:5){
  resultados=list()
  method=c("fs_sd","fs_cor","fs_pca")
  for(cutoff in seq_along(threshold)){
print(cutoff) 
    for (meth in seq_along(method)){
      print(meth)
        h=fs(hum,method[meth],threshold[cutoff]) %>% 
                  mutate(DIMENSAO="Humano",method=method[meth],cutoff=threshold[cutoff])
        a=fs(amb,method[meth],threshold[cutoff]) %>% 
                  mutate(DIMENSAO="Ecosistemico",method=method[meth],cutoff=threshold[cutoff])
      
    }
  }
  h
}

todos<-rbind(
  fs(df.humano.vlr,"fs_sd",3)%>% mutate(DIMENSAO="Humano",method="fs_sd",cutoff=3),  
  fs(df.ambiental.vlr,"fs_sd",3)%>% mutate(DIMENSAO="Ambiental",method="fs_sd",cutoff=3),  
      fs(df.humano.vlr,"fs_cor",3)%>% mutate(DIMENSAO="Humano",method="fs_cor",cutoff=3),
      fs(df.ambiental.vlr,"fs_cor",3)%>% mutate(DIMENSAO="Ambiental",method="fs_cor",cutoff=3),
      fs(df.humano.vlr,"fs_pca",3)%>% mutate(DIMENSAO="Humano",method="fs_pca",cutoff=3),
      fs(df.ambiental.vlr,"fs_pca",3)%>% mutate(DIMENSAO="Ambiental",method="fs_pca",cutoff=3),
  fs(df.humano.vlr,"fs_sd",4)%>% mutate(DIMENSAO="Humano",method="fs_sd",cutoff=4),  
  fs(df.ambiental.vlr,"fs_sd",4)%>% mutate(DIMENSAO="Ambiental",method="fs_sd",cutoff=4),    
  fs(df.humano.vlr,"fs_cor",4)%>% mutate(DIMENSAO="Humano",method="fs_cor",cutoff=4),
      fs(df.ambiental.vlr,"fs_cor",4)%>% mutate(DIMENSAO="Ambiental",method="fs_cor",cutoff=4),
      fs(df.humano.vlr,"fs_pca",4)%>% mutate(DIMENSAO="Humano",method="fs_pca",cutoff=4),
      fs(df.ambiental.vlr,"fs_pca",4)%>% mutate(DIMENSAO="Ambiental",method="fs_pca",cutoff=4),
  fs(df.humano.vlr,"fs_sd",5)%>% mutate(DIMENSAO="Humano",method="fs_sd",cutoff=5),  
  fs(df.ambiental.vlr,"fs_sd",5)%>% mutate(DIMENSAO="Ambiental",method="fs_sd",cutoff=5),    
  fs(df.humano.vlr,"fs_cor",5)%>% mutate(DIMENSAO="Humano",method="fs_cor",cutoff=5),
      fs(df.ambiental.vlr,"fs_cor",5)%>% mutate(DIMENSAO="Ambiental",method="fs_cor",cutoff=5),
      fs(df.humano.vlr,"fs_pca",5)%>% mutate(DIMENSAO="Humano",method="fs_pca",cutoff=5),
      fs(df.ambiental.vlr,"fs_pca",5)%>% mutate(DIMENSAO="Ambiental",method="fs_pca",cutoff=5),
  fs(df.humano.vlr,"fs_sd",6)%>% mutate(DIMENSAO="Humano",method="fs_sd",cutoff=6),  
  fs(df.ambiental.vlr,"fs_sd",6)%>% mutate(DIMENSAO="Ambiental",method="fs_sd",cutoff=6),
      fs(df.humano.vlr,"fs_cor",6)%>% mutate(DIMENSAO="Humano",method="fs_cor",cutoff=6),
      fs(df.ambiental.vlr,"fs_cor",6)%>% mutate(DIMENSAO="Ambiental",method="fs_cor",cutoff=6),
      fs(df.humano.vlr,"fs_pca",6)%>% mutate(DIMENSAO="Humano",method="fs_pca",cutoff=6),
      fs(df.ambiental.vlr,"fs_pca",6) %>% mutate(DIMENSAO="Ambiental",method="fs_pca",cutoff=ncol(df.humano.vlr)-1),
  fs(df.humano.vlr,"fs_sd",ncol(df.humano.vlr)-1)%>% mutate(DIMENSAO="Humano",method="fs_sd",cutoff=3),  
  fs(df.ambiental.vlr,"fs_sd",ncol(df.ambiental.vlr)-1)%>% mutate(DIMENSAO="Ambiental",method="fs_sd",cutoff=3),
      fs(df.humano.vlr,"fs_cor",ncol(df.humano.vlr)-1)%>% mutate(DIMENSAO="Humano",method="fs_cor",cutoff=ncol(df.humano.vlr)-1),
      fs(df.ambiental.vlr,"fs_cor",ncol(df.ambiental.vlr)-1)%>% mutate(DIMENSAO="Ambiental",method="fs_cor",cutoff=ncol(df.ambiental.vlr)-1),
      fs(df.humano.vlr,"fs_pca",ncol(df.humano.vlr)-1)%>% mutate(DIMENSAO="Humano",method="fs_pca",cutoff=ncol(df.humano.vlr)-1),
      fs(df.ambiental.vlr,"fs_pca",ncol(df.ambiental.vlr)-1)%>% mutate(DIMENSAO="Ambiental",method="fs_pca",cutoff=ncol(df.ambiental.vlr)-1)
)



# bs.corr<-rbind(
# (bs.ambiental.cor) %>% mutate(DIMENSAO="Ecosistemico"),
# (bs.hum.cor) %>% mutate(DIMENSAO="Humano"))
# bs.corr[,-c(14,15)]<-convert(bs.corr[,-c(14,15)])
# 
# bs.corr<-bs.corr %>% 
#   gather(.,municipio,valor,-INDICADOR,-DIMENSAO) %>%
#   select(municipio,DIMENSAO,valor) %>%
#   group_by(municipio,DIMENSAO) %>%
#   summarise(bs=mean(valor)) %>%
#   ungroup() %>%
#   spread(DIMENSAO,bs)
# 
# 

