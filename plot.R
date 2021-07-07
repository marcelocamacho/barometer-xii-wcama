bs<-read.csv('plot.csv', dec=',')
escala<-data.frame(
  div=c(0,20,40,60,80),
  divx=c(0,NA,40,60,80),
  posi=c(10,30,50,70,90),
  labels=c("Insustentável","P. insustentável","Intermediário", "P. sustentável","Sustentável"))

image<-bs %>% filter(Cenario %in% c("Original","Corr -5 fatores","PCA - 5 fatores")) %>%
ggplot(aes(x=Humano,y=Ecosistemico)) +
  geom_vline(xintercept=c(20,40,60,80),alpha=0.2,col="red")+
  geom_hline(yintercept=c(20,40,60,80),alpha=0.2,col="red")+
  geom_label_repel(aes(label=(
    paste0("(",round(Humano,2),",",round(Ecosistemico,2),")"))),
    size=4,family = "Times New Roman",force = 1,box.padding = 0.5, max.overlaps = 10) +
  geom_point(size=5,aes(shape=municipio,color=Cenario)) +
  scale_x_continuous(expand = c(0, 0),breaks = c(10,20,30,40,50,60,70,80,90),limits = c(26,62))+
  scale_y_continuous(expand = c(0, 0),breaks = c(10,20,30,40,50,60,70,80,90),limits = c(23,83))+
  geom_text(data=escala,aes(x=divx,y=24,
                            label = labels,angle=90, vjust="right",hjust="bottom", family = "Times New Roman",
                            ),size=5)+
  geom_text(data=escala,aes(x=26,y=div,
                            label = labels, vjust="bottom",hjust="left",family = "Times New Roman", 
                            ),size=5)+
  scale_shape_manual(values = c(1:14))+
  labs(title="Barômetro da sustentabilidade", x="Dimensão Humana",y="Dimensão Ambiental",
       caption = "Fonte: Elaborado pelos autores")+ 
  theme(plot.title = element_text(vjust=0.5,hjust=0.5),
        plot.caption = element_text(vjust=0.5,hjust=0),
        text=element_text(family="Times New Roman", face="bold", size=14),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.ticks.length = unit(0, "cm"),
        axis.ticks.margin = unit(0, "cm"),
        panel.background = element_rect(fill="white",color = "black") ) + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2")

ggsave(file="grafico.svg", plot=image, width=15, height=7)

