# Packages needed #
library(tidyverse)
library(showtext)
library(ggpubr)
library(patchwork)
library(readxl)
library(gt)
library(ggtext)
library(cowplot)
library(magick)
library(ggridges)
library(gtExtras)
library(treemap)

# Data import #
datos_TS <- read_excel("datos_TS.xlsx")

datos_TS<-datos_TS[,1:6] # Deleting the mean and median table included in the Excel
# Checking spelling and blankspaces 
names(datos_TS) #Checked
unique(datos_TS$Album) #Checked
unique(datos_TS$Skip) #Checked
unique(datos_TS$Letra) #Checked
unique(datos_TS$Meidentifico) #Checked

