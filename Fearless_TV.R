# Fearless (TV)

tabla_fearless<-datos_TS[c(15:40),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[8],height = 60))%>%
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
    style = cell_fill(color = "#D9B97E"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Mr. Perfectly Fine"))
tabla_fearless

## Porcentaje de las letras

cringe_FTV<-nrow(subset(datos_TS,Album=="Fearless (Taylor's Version)"&Letra=="CRINGE"))
master_FTV<-nrow(subset(datos_TS,Album=="Fearless (Taylor's Version)"&Letra=="MASTERPIECE"))
normal_FTV<-nrow(subset(datos_TS,Album=="Fearless (Taylor's Version)"&Letra=="NORMAL"))
total_FTV<-nrow(subset(datos_TS,Album=="Fearless (Taylor's Version)"))
percentage_cringe_FTV<-cringe_FTV/total_FTV
percentage_master_FTV<-master_FTV/total_FTV
percentage_normal_FTV<-normal_FTV/total_FTV
ymax_FTV<-cumsum(c(percentage_cringe_FTV,percentage_master_FTV,percentage_normal_FTV))
ymin_FTV<-c(0,head(ymax_FTV,n=-1))

data_FTV<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_FTV,percentage_master_FTV,percentage_normal_FTV),ymax_FTV,ymin_FTV)

labelposition_FTV<-(ymax_FTV+ymin_FTV)/2

#Plot

ggplot(data_FTV,aes(ymax=ymax_FTV,ymin=ymin_FTV,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5,family = "Lato",face = "bold",vjust=-3))+
  theme(legend.text = element_text(size=12,family="Lato"))+
  geom_text(x=3.5,aes(y=labelposition_FTV,label=c("4%","38%","58%")))+
  scale_fill_manual(values=c("#BF8B4B","#F2D8A7","#733E1F"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 16))

percentage_normal_FTV
