## Tabla

# Means
medias<-aggregate(datos_TS$Puntaje,list(datos_TS$Album),FUN=mean)
colnames(medias)<-c("Álbum","Puntaje promedio")

# Medians

medianas<-aggregate(datos_TS$Puntaje,list(datos_TS$Album),FUN=median)

# Sort both in a single table from max to min median

info_completa<- cbind(medias,medianas$x)
colnames(info_completa)[3]<-"Puntaje medio"

info_completa<-info_completa[order(info_completa$`Puntaje medio`,info_completa$`Puntaje promedio`,decreasing = TRUE),] #Scoring sorted by median, the ties were solved by mean

colnames(info_completa)<-c("album","prom","mediana","url")
info_completa<-info_completa %>% 
  relocate(url,.before = album)

url_albums<-c("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/TS_folklore_Wordmark.svg/1920px-TS_folklore_Wordmark.svg.png","https://upload.wikimedia.org/wikipedia/commons/5/52/Taylor_Swift_-_Speak_Now.svg","https://upload.wikimedia.org/wikipedia/commons/5/5d/1989_album_logo.png","https://upload.wikimedia.org/wikipedia/commons/2/21/Evermore-wordmark.svg","https://upload.wikimedia.org/wikipedia/commons/3/37/Taylor_Swift_-_Midnights_%28Logo%29.png","https://upload.wikimedia.org/wikipedia/commons/3/31/Reputation_Logo.svg","https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Red_%28Taylor%27s_Version%29_logo.svg/800px-Red_%28Taylor%27s_Version%29_logo.svg.png","https://upload.wikimedia.org/wikipedia/commons/8/83/Fearless_%28Taylor%27s_Version%29_logo.svg","https://upload.wikimedia.org/wikipedia/commons/4/40/Taylor_swift_logo.jpg","https://upload.wikimedia.org/wikipedia/commons/6/61/Taylor_Swift_-_Lover_%28Logo%29.png") # Album logos

info_completa$url<-url_albums

tabla<-info_completa[,c(1,3,4)] %>%
  gt() %>%
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(url)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = c(30,40,40,20,25,30,40,20,35,50) #image size
      )
    }) %>%
  cols_label(url="Álbum",prom="Puntaje promedio",mediana="Puntaje medio") 
  
tabla<-tabla %>%
  opt_all_caps()%>%
  opt_table_font(font = list(google_font("Lato"),default_fonts()))

tabla<-tabla %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "all", weight = px(2)),
        #Make text bold
        cell_text(weight = "bold")
  ))
tabla<-tabla %>%
  tab_style(
    style = cell_borders(
      sides = c("left","right"),
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )
tabla <-tabla%>%
  tab_style(
    style = cell_borders(
      sides=c("bottom","top"),color="transparent"),
    locations = cells_body()
    )
tabla <- tabla %>%
  tab_options(table_body.border.bottom.color = "black") %>%
  tab_options(column_labels.font.size = px(15)) %>%
  cols_align("center")
tabla

gtsave(tabla,"tabla_puntajes1.png")
