# reputation

tabla_rep<-datos_TS[c(103:117),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[6],height = 60))%>%
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
    style = cell_fill(color ="olivedrab4"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Getaway Car"))
tabla_rep


# Porcentaje de letras

cringe_rep<-nrow(subset(datos_TS,Album=="reputation"&Letra=="CRINGE"))
master_rep<-nrow(subset(datos_TS,Album=="reputation"&Letra=="MASTERPIECE"))
normal_rep<-nrow(subset(datos_TS,Album=="reputation"&Letra=="NORMAL"))
total_rep<-nrow(subset(datos_TS,Album=="reputation"))
percentage_cringe_rep<-cringe_rep/total_rep
percentage_master_rep<-master_rep/total_rep
percentage_normal_rep<-normal_rep/total_rep
ymax_rep<-cumsum(c(percentage_cringe_rep,percentage_master_rep,percentage_normal_rep))
ymin_rep<-c(0,head(ymax_rep,n=-1))

data_rep<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_rep,percentage_master_rep,percentage_normal_rep),ymax_rep,ymin_rep)

labelposition_rep<-(ymax_rep+ymin_rep)/2

#Plot

ggplot(data_rep,aes(ymax=ymax_rep,ymin=ymin_rep,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5,family = "Lato",face = "bold",vjust=-3))+
  theme(legend.text = element_text(size=12,family="Lato"))+
  geom_text(x=3.5,aes(y=labelposition_rep,label=c("40%","27%","33%")))+
  scale_fill_manual(values=c("olivedrab4","#0C231F","#A49749"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 16))

percentage_normal_rep
