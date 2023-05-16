#evermore

tabla_evermore<-datos_TS[c(153:169),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[4],height = 40))%>%
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
    style = cell_fill(color ="#F2845C"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="no body no crime (con Haim)"))
gtsave_extra(tabla_evermore,"tabla_evermore.png")

#Lyrics

#Percentage per category

cringe_ever<-nrow(subset(datos_TS,Album=="evermore"&Letra=="CRINGE"))
master_ever<-nrow(subset(datos_TS,Album=="evermore"&Letra=="MASTERPIECE"))
normal_ever<-nrow(subset(datos_TS,Album=="evermore"&Letra=="NORMAL"))
total_ever<-nrow(subset(datos_TS,Album=="evermore"))
percentage_cringe_ever<-cringe_ever/total_ever
percentage_master_ever<-master_ever/total_ever
percentage_normal_ever<-normal_ever/total_ever
ymax_ever<-cumsum(c(percentage_cringe_ever,percentage_master_ever,percentage_normal_ever))
ymin_ever<-c(0,head(ymax_ever,n=-1))

data_ever<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_ever,percentage_master_ever,percentage_normal_ever),ymax_ever,ymin_ever)

data_ever<-subset(data_ever,letra!="cringe") #There were no cringes in evermore, so I subseted the data to avoid adding a cringe box in the legend when there weren't any cringes
labelposition_ever<-(ymax_ever+ymin_ever)/2

#Pie chart

pie_ever<-ggplot(data_ever,aes(ymax=ymax_ever,ymin=ymin_ever,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=16,family="Lato"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_ever[2:3],label=c("35%","65%")))+
  scale_fill_manual(values=c("#D9704A","#594636"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 20))

# Skips 

bar_ever<-ggplot(data = subset(skips,album=="evermore"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#D9704A","#594636"))+
  theme(axis.title = element_text(family = "Lato",face = "bold",size = 20))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(aspect.ratio = 1)
ever_stats<-bar_ever+pie_ever
ggsave("ever_stats.png",plot=ever_stats)
