#Midnights

# Table with scores per song

tabla_Mid<-datos_TS[c(170:189),-2] %>%
  gt()%>%
  cols_label(Canción="Canción",Puntaje="Puntaje",Skip="Skip",Meidentifico="Me identifico")%>%
  opt_all_caps()%>%
  opt_table_font(font = list(google_font("Lato"),default_fonts())) %>%
  tab_style(
    style = cell_borders(
      sides = c("left","right"),
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )%>%
  tab_style(
    style = cell_borders(
      sides=c("bottom","top"),color="transparent"),
    locations = cells_body()
  ) %>%
  tab_options(table_body.border.bottom.color = "black") %>%
  tab_options(column_labels.font.size = px(15)) %>%
  cols_align("center")%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "all", weight = px(2)),
      #Make text bold
      cell_text(weight = "bold")
    ))%>%
  gt::tab_header(title = add_text_img("",url=info_completa$url[5],height = 40))%>%
  tab_style(
    locations = cells_title("title"),
    style = list(
      cell_borders(sides="all"))) %>%
  tab_style(
    locations = cells_title("title"),
    style = list(
      cell_borders(sides=c("top","right","left"),color="transparent")
    )
  )%>%
  tab_options(table.border.top.style = "hidden")%>%
  tab_style(
    style = cell_fill(color ="#6597AA"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="The Great War"))
gtsave_extra(tabla_Mid,"tabla_Midnights.png")

#Lyrics

#Percentage per category

cringe_mid<-nrow(subset(datos_TS,Album=="Midnights"&Letra=="CRINGE"))
master_mid<-nrow(subset(datos_TS,Album=="Midnights"&Letra=="MASTERPIECE"))
normal_mid<-nrow(subset(datos_TS,Album=="Midnights"&Letra=="NORMAL"))
total_mid<-nrow(subset(datos_TS,Album=="Midnights"))
percentage_cringe_mid<-cringe_mid/total_mid
percentage_master_mid<-master_mid/total_mid
percentage_normal_mid<-normal_mid/total_mid
ymax_mid<-cumsum(c(percentage_cringe_mid,percentage_master_mid,percentage_normal_mid))
ymin_mid<-c(0,head(ymax_mid,n=-1))

data_mid<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_mid,percentage_master_mid,percentage_normal_mid),ymax_mid,ymin_mid)

labelposition_mid<-(ymax_mid+ymin_mid)/2

#Pie chart

pie_mid<-ggplot(data_mid,aes(ymax=ymax_mid,ymin=ymin_mid,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=16,family="Lato"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_mid,label=c("20%","55%","25%")))+
  scale_fill_manual(values=c("#D98F4E","#6597AA","#552115"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 20))

# Skips 

bar_mid<-ggplot(data = subset(skips,album=="Midnights"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#6597AA","#552115"))+
  theme(axis.title = element_text(family = "Lato",face = "bold",size = 20))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(aspect.ratio = 1)
mid_stats<-bar_mid+pie_mid
ggsave("mid_stats.png",plot=mid_stats)
