# Speak Now
tabla_sn<-datos_TS[c(41:57),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[2],height = 60))%>%
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
    style = cell_fill(color ="#BF5690"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Mean"))
tabla_sn


# Porcentaje de letras

cringe_sn<-nrow(subset(datos_TS,Album=="Speak Now"&Letra=="CRINGE"))
master_sn<-nrow(subset(datos_TS,Album=="Speak Now"&Letra=="MASTERPIECE"))
normal_sn<-nrow(subset(datos_TS,Album=="Speak Now"&Letra=="NORMAL"))
total_sn<-nrow(subset(datos_TS,Album=="Speak Now"))
percentage_cringe_sn<-cringe_sn/total_sn
percentage_master_sn<-master_sn/total_sn
percentage_normal_sn<-normal_sn/total_sn
ymax_sn<-cumsum(c(percentage_cringe_sn,percentage_master_sn,percentage_normal_sn))
ymin_sn<-c(0,head(ymax_sn,n=-1))

data_sn<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_sn,percentage_master_sn,percentage_normal_sn),ymax_sn,ymin_sn)

labelposition_sn<-(ymax_sn+ymin_sn)/2

#Plot

ggplot(data_sn,aes(ymax=ymax_sn,ymin=ymin_sn,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5,family = "Lato",face = "bold",vjust=-3))+
  theme(legend.text = element_text(size=12,family="Lato"))+
  geom_text(x=3.5,aes(y=labelposition_sn,label=c("6%","35%","59%")))+
  scale_fill_manual(values=c("#E1B283","#BF5690","#BF463B"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 16))



