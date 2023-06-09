# Red (Taylor's Version)

# Table with scores per song

tabla_red<-datos_TS[c(58:86),-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[7],height = 60))%>%
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
    style = cell_fill(color ="#BF1304"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="State of Grace"))
gtsave_extra(tabla_red,"tabla_red.png")

#Lyrics

#Percentage per category

cringe_red<-nrow(subset(datos_TS,Album=="Red (Taylor's Version)"&Letra=="CRINGE"))
master_red<-nrow(subset(datos_TS,Album=="Red (Taylor's Version)"&Letra=="MASTERPIECE"))
normal_red<-nrow(subset(datos_TS,Album=="Red (Taylor's Version)"&Letra=="NORMAL"))
total_red<-nrow(subset(datos_TS,Album=="Red (Taylor's Version)"))
percentage_cringe_red<-cringe_red/total_red
percentage_master_red<-master_red/total_red
percentage_normal_red<-normal_red/total_red
ymax_red<-cumsum(c(percentage_cringe_red,percentage_master_red,percentage_normal_red))
ymin_red<-c(0,head(ymax_red,n=-1))

data_red<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_red,percentage_master_red,percentage_normal_red),ymax_red,ymin_red)

labelposition_red<-(ymax_red+ymin_red)/2

#Pie chart

pie_red<-ggplot(data_red,aes(ymax=ymax_red,ymin=ymin_red,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=16,family="Lato"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_red,label=c("4%","24%","72%")))+
  scale_fill_manual(values=c("#BFA08E","#8C6A56","#BF1304"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Lato",face="bold",size = 20))

# Skips 

bar_red<-ggplot(data = subset(skips,album=="Red (Taylor's Version)"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#8C6A56","#BF1304"))+
  theme(axis.title = element_text(family = "Lato",face = "bold",size = 20))+
  theme(axis.text = element_text(family = "Lato",size = 16))+
  theme(aspect.ratio = 1)

red_stats<-bar_red+pie_red #combined plot using patchwork
ggsave("red_stats.png",plot=red_stats)

