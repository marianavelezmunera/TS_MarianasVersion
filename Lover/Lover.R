#Lover
tabla_lover<-datos_TS[c(118:135),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[10],height = 100))%>%
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
    style = cell_fill(color ="#F2C2D4"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Daylight"))
tabla_lover

gtsave_extra(tabla_lover,"tabla_lover.png")
# Porcentaje de letras

cringe_lov<-nrow(subset(datos_TS,Album=="Lover"&Letra=="CRINGE"))
master_lov<-nrow(subset(datos_TS,Album=="Lover"&Letra=="MASTERPIECE"))
normal_lov<-nrow(subset(datos_TS,Album=="Lover"&Letra=="NORMAL"))
total_lov<-nrow(subset(datos_TS,Album=="Lover"))
percentage_cringe_lov<-cringe_lov/total_lov
percentage_master_lov<-master_lov/total_lov
percentage_normal_lov<-normal_lov/total_lov
ymax_lov<-cumsum(c(percentage_cringe_lov,percentage_master_lov,percentage_normal_lov))
ymin_lov<-c(0,head(ymax_lov,n=-1))

data_lov<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_lov,percentage_master_lov,percentage_normal_lov),ymax_lov,ymin_lov)

labelposition_lov<-(ymax_lov+ymin_lov)/2

#Plot

pie_lov<-ggplot(data_lov,aes(ymax=ymax_lov,ymin=ymin_lov,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=16,family="Lato"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_lov,label=c("28%","44%","28%")))+
  scale_fill_manual(values=c("#F2C9D4","#F2DBAE","#6CBAD9"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 20))

percentage_normal_lov

# Skips 

bar_lov<-ggplot(data = subset(skips,album=="Lover"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#F2C9D4","#6CBAD9"))+
  theme(axis.title = element_text(family = "Lato",face = "bold",size = 20))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(aspect.ratio = 1)
lov_stats<-bar_lov+pie_lov
ggsave("lov_stats.png",plot=lov_stats)
