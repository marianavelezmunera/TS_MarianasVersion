#1989#

tabla_1989<-datos_TS[c(87:102),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[3],height = 100))%>%
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
    style = cell_fill(color ="#93B3BF"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="New Romantics"))
tabla_1989
gtsave_extra(tabla_1989,"tabla_1989.png")

# Porcentaje de letras

cringe_1989<-nrow(subset(datos_TS,Album=="1989"&Letra=="CRINGE"))
master_1989<-nrow(subset(datos_TS,Album=="1989"&Letra=="MASTERPIECE"))
normal_1989<-nrow(subset(datos_TS,Album=="1989"&Letra=="NORMAL"))
total_1989<-nrow(subset(datos_TS,Album=="1989"))
percentage_cringe_1989<-cringe_1989/total_1989
percentage_master_1989<-master_1989/total_1989
percentage_normal_1989<-normal_1989/total_1989
ymax_1989<-cumsum(c(percentage_cringe_1989,percentage_master_1989,percentage_normal_1989))
ymin_1989<-c(0,head(ymax_1989,n=-1))

data_1989<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_1989,percentage_master_1989,percentage_normal_1989),ymax_1989,ymin_1989)

labelposition_1989<-(ymax_1989+ymin_1989)/2

#Plot

pie_1989<-ggplot(data_1989,aes(ymax=ymax_1989,ymin=ymin_1989,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=16,family="Lato"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_1989,label=c("25%","44%","31%")))+
  scale_fill_manual(values=c("#5E4973","#93B3BF","#D9CEAD"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 20))
percentage_normal_1989

# Skips 

bar_1989<-ggplot(data = subset(skips,album=="1989"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#5E4973","#93B3BF"))+
  theme(axis.title = element_text(family = "Lato",face = "bold",size = 20))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(aspect.ratio = 1)
bar_FTV
stats_1989<-bar_1989+pie_1989
ggsave("1989_deb.png",plot=stats_1989)  
