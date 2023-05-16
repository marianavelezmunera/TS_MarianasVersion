# General analysis

Sys.setlocale("LC_ALL", "German")
options(encoding = "UTF-8") #For the tildes (I don't know why ggplot wasn't having it with the tildes)
datos_TS$Album <- factor(datos_TS$Album , levels=c("Taylor Swift", "Fearless (Taylor's Version)", "Speak Now", "Red (Taylor's Version)","1989","reputation","Lover","folklore","evermore","Midnights")) #Reordering so it is in launch order

font_add_google("Lato","Lato") #Font
font.families()
showtext_auto()

# Boxplot with score info per album

general_bp<-ggplot(data=datos_TS,aes(x=Album,y=Puntaje,fill=Album))+
  geom_boxplot(color="black")+
  xlab("ALBUM")+ylab("PUNTAJE")+
  theme_pubclean()+
  theme(legend.position = "none")+
  scale_fill_manual(values =  c("#769839","#D9B97E","#BF5690","#92150F","#93B3BF","#486049","#F2C2D4","grey","#F2845C","#162759"))+
  coord_flip()+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(family = "Lato",size=16))+
  theme(axis.title.x = element_text(family = "Lato",size=20,face="bold"))
  
general_bp
pimage <- axis_canvas(general_bp, axis = 'y')+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/4/40/Taylor_swift_logo.jpg", y = 2.2 , scale = 0.75)+
  cowplot::draw_image("https://cdn.shopify.com/s/files/1/0011/4651/9637/t/235/assets/fr.png?v=95464405164507874381683655912", y = 3, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/5/52/Taylor_Swift_-_Speak_Now.svg", y = 3.75, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Red_%28Taylor%27s_Version%29_logo.svg/800px-Red_%28Taylor%27s_Version%29_logo.svg.png", y = 4.5, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/5/5d/1989_album_logo.png", y =5.5, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Reputation_Logo.svg/800px-Reputation_Logo.svg.png", y = 6.2, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/6/61/Taylor_Swift_-_Lover_%28Logo%29.png", y = 7, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/TS_folklore_Wordmark.svg/1920px-TS_folklore_Wordmark.svg.png", y = 7.8, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Evermore-wordmark.svg/270px-Evermore-wordmark.svg.png", y = 8.6, scale = 0.75)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/3/37/Taylor_Swift_-_Midnights_%28Logo%29.png", y = 9.4, scale = 0.75) #Canvas with album logos 
  
plot_listo<-ggdraw(insert_yaxis_grob(general_bp, pimage, position = "left")) #Plot with the canvas and the boxplots
plot_listo
ggsave("boxplot_total.png",plot=plot_listo) #save plot

# Barplot Lyrics

bars<-ggplot(data=datos_TS,aes(x=Letra,fill=Letra))+
  geom_bar(width = 0.25,color="black")+
  theme_pubclean()+
  theme(aspect.ratio = 1)+
  ylab("NÚMERO DE CANCIONES")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(family="Lato",face="bold",size = 20))+
  theme(axis.text.x = element_text(family = "Lato",size=14))+
  theme(axis.text.y = element_text(family = "Lato",size=18))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Cringe","Masterpiece","Normal"))+
  scale_fill_manual(values =c("#92150F","#93B3BF","#486049"))+
  ylim(c(0,100))+
  ggtitle("¿CÓMO ME PARECE LA LETRA DE LAS CANCIONES?")+
  theme(plot.title = element_text(family = "Lato",face="bold",size = 20,hjust = 0))

ggsave("letra.png",bars)

#Skips

skips<-as.data.frame(table(datos_TS$Album,datos_TS$Skip))
colnames(skips)<-c("album","skips","frecuencia")

skips
skips_graph<-ggplot(data = skips,aes(x=album,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black")+
  theme_pubclean()+
  coord_flip()+
  ylab("NÚMERO DE CANCIONES")+
  scale_fill_manual(name = "SKIPS",values=c("#162759","grey"))+
  theme(axis.title.x = element_text(family="Lato",face="bold",size=20))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(family = "Lato",size = 16))+
  theme(legend.text = element_text(family = "Lato",size=16))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(legend.position = "right")
skips_graph

skips_listo<-ggdraw(insert_yaxis_grob(skips_graph, pimage, position = "left"))
skips_listo

ggsave("skips.png",skips_listo)

# Densities

ridge<-ggplot(data=datos_TS,aes(x=Puntaje,y=Album,fill=Album))+
  geom_density_ridges(alpha=0.7)+
  theme_pubclean()+
  theme(legend.position = "none")+
  scale_fill_manual(values =  c("#769839","#D9B97E","#BF5690","#92150F","#93B3BF","#486049","#F2C2D4","grey","#F2845C","#162759"))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  xlab("PUNTAJE")+
  theme(axis.title.x = element_text(family = "Lato",face="bold",size=20))+
  theme(axis.text.x = element_text(family = "Lato",size=16))+
  scale_x_continuous(breaks=seq(2, 10, 2))
  
  
ridges_listo<-ggdraw(insert_yaxis_grob(ridge, pimage, position = "left"))
ridges_listo
ggsave("ridges.png",ridges_listo)

#Identification

conteo_iden<-count(datos_TS,Album,Meidentifico) #Freq table

no<-subset(conteo_iden,Meidentifico=="NO")
si<-subset(conteo_iden,Meidentifico=="SÍ")
tabla_ident<-data.frame(album=datos_TS$Album,si=cou)

identi<-no[,-2]
colnames(identi)[2]<-"NO"
identi$SI<-si$n

identi$Album <- factor(identi$Album , levels=c("Taylor Swift", "Fearless (Taylor's Version)", "Speak Now", "Red (Taylor's Version)","1989","reputation","Lover","folklore","evermore","Midnights"))


loli<-ggplot(identi) +
  geom_segment( aes(x=Album, xend=Album, y=SI, yend=NO), color="black") +
  geom_point( aes(x=Album, y=SI,color = "SI"), size=3 ) +
  geom_point( aes(x=Album, y=NO,color="NO"), size=3 ) +
  scale_color_manual(values = c("#BF5690", "#162759"),
                     guide  = guide_legend(), 
                     name   = "¿ME IDENTIFICO CON LA CANCIÓN?")+
  theme_pubclean()+
  coord_flip()+
  xlab("") +
  ylab("NÚMERO DE CANCIONES")+
  theme(legend.position = "bottom",
        panel.border    = element_blank())+
  theme(legend.text = element_text(family = "Lato",size = 16))+
  theme(legend.title = element_text(family = "Lato",face="bold",size=16))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_text(family = "Lato",face="bold",size = 20) )+
  theme(axis.text.x = element_text(family = "Lato",size=16))

loli_listo<-ggdraw(insert_yaxis_grob(loli, pimage, position = "left"))
loli_listo
ggsave("identificada_total.png",plot=loli_listo)
  

