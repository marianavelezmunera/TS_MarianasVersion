# Taylor Swift (Debut album)

# Table with scores 

tabla_debut<-datos_TS[c(1:14),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[9],height = 60))%>%
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
    style = cell_fill(color = "#769839"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Picture to Burn"))
tabla_debut
gtsave_extra(tabla_debut,"tabla_debut.png")
# Porcentaje de letras

cringe_deb<-nrow(subset(datos_TS,Album=="Taylor Swift"&Letra=="CRINGE"))
master_deb<-nrow(subset(datos_TS,Album=="Taylor Swift"&Letra=="MASTERPIECE"))
normal_deb<-nrow(subset(datos_TS,Album=="Taylor Swift"&Letra=="NORMAL"))
total_deb<-nrow(subset(datos_TS,Album=="Taylor Swift"))
percentage_cringe_deb<-cringe_deb/total_deb
percentage_master_deb<-master_deb/total_deb
percentage_normal_deb<-normal_deb/total_deb
ymax<-cumsum(c(percentage_cringe_deb,percentage_master_deb,percentage_normal_deb))
ymin<-c(0,head(ymax,n=-1))

data_deb<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_deb,percentage_master_deb,percentage_normal_deb),ymax,ymin)

labelposition<-(ymax+ymin)/2
data_deb<-subset(data_deb,letra!="master")

#Plot

ggplot(data_deb,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5,family = "Lato",face = "bold",vjust=-3))+
  theme(legend.text = element_text(size=12,family="Lato"))+
  geom_text(x=3.5,aes(y=labelposition[-2],label=c("21%","78%")))+
  scale_fill_manual(values=c("#769839","#18B5D9"),labels=c("Cringe","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 16))



