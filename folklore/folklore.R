#folklore

tabla_folklore<-datos_TS[c(136:152),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[1],height = 60))%>%
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
    style = cell_fill(color ="grey"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="mirrorball"))
tabla_folklore

gtsave_extra(tabla_folklore,"tabla_folklore.png")
#Porcentaje de letras

cringe_fol<-nrow(subset(datos_TS,Album=="folklore"&Letra=="CRINGE"))
master_fol<-nrow(subset(datos_TS,Album=="folklore"&Letra=="MASTERPIECE"))
normal_fol<-nrow(subset(datos_TS,Album=="folklore"&Letra=="NORMAL"))
total_fol<-nrow(subset(datos_TS,Album=="folklore"))
percentage_cringe_fol<-cringe_fol/total_fol
percentage_master_fol<-master_fol/total_fol
percentage_normal_fol<-normal_fol/total_fol
ymax_fol<-cumsum(c(percentage_cringe_fol,percentage_master_fol,percentage_normal_fol))
ymin_fol<-c(0,head(ymax_fol,n=-1))

data_fol<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_fol,percentage_master_fol,percentage_normal_fol),ymax_fol,ymin_fol)
data_fol<-subset(data_fol,letra!="cringe")

labelposition_fol<-(ymax_fol+ymin_fol)/2

#Plot

ggplot(data_fol,aes(ymax=ymax_fol,ymin=ymin_fol,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5,family = "Lato",face = "bold",vjust=-3))+
  theme(legend.text = element_text(size=12,family="Lato"))+
  geom_text(x=3.5,aes(y=labelposition_fol[2:3],label=c("82%","18%")))+
  scale_fill_manual(values=c("#BEB9B5","#BFBB7A"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 16))

percentage_master_fol
