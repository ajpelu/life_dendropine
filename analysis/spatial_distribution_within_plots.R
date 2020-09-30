library("here")
library("tidyverse")
library("magrittr")
library("stringr")
library("readODS")
#devtools::install_github("ajpelu/dendroEco")
library("dendroEco")
library("ggrepel")
library(ggplus)
library(ggforce)


path <- "/Users/ajpelu/Google Drive/0_proyectos/ADAPTAMED_iE/ACCIONES/C1/data_forestal_c1_previo/"

# Leer todos los archivos ods 
ods_file_names <- path %>% 
  list.files() %>%
  .[str_detect(., ".ods")] 

# Read tipo data 
df_tipo <- ods_file_names %>% 
  purrr::map_dfr(function(file_name){ 
    daso <- read_ods(paste0(path, file_name),
                     sheet = "2_EJEMPLARES_TIPO", col_types = cols()) %>%
      as_tibble(.name_repair = "unique") %>% # Name variable col 7
      mutate(code = str_remove(file_name, "_c1_forestal.ods"))
  }) %>% 
  dplyr::rename(cuadrante = Cuadrante, sp = Especie, 
                rumbo = "Rumbo (º)", chapa = "N.º Chapa",
                distance = "Distancia (m)",
                tree_height = "Altura (m)", 
                dbh1 = "D.nor (cm) 1", dbh2 = "D.nor (cm) 2", 
                dbase1 ="D.bas (cm) 1", dbase2 = "D.bas (cm) 2", 
                fcv = Fcv,
                proyeccion1 = "Proyección (m) 1", 
                proyeccion2 = "Proyección (m) 2", 
                damage_type = "Daños (tipo)", 
                damage_percentage = "Daños (% afectado/pie)")









# 
# get_coords_trees <- function(angle, distance, x0, y0) {
#   angle <- ifelse(angle <= 90, 90 - angle, 450 - angle)
#   data.frame(x = x0 + distance * cos(angle / 180 * pi),
#              y = y0+ distance * sin(angle / 180 * pi))
# }

# see stackoverflow questions 6862742
gg_circle <- function(radius, xcenter, ycenter, color='black', fill=NA, ...){
  x <- xcenter + radius*cos(seq(0,pi, length.out = 100))
  ymax <- ycenter + radius*sin(seq(0, pi, length.out = 100))
  ymin <- ycenter + radius*sin(seq(0, -pi, length.out = 100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}



df_tipo <- df_tipo %>% 
  mutate(xcord = coordtrees(angle= rumbo, distance = distance)$x,
         ycord = coordtrees(angle= rumbo, distance = distance)$y)


# ejemplo 
ll <- df_tipo %>% filter(code %in% c("PH_CONTROL_CENTRAL", "PH_CONTROL_ESTE")) 

pdf("dist_tipos.pdf", 8, 8) 
dist_tipos <- df_tipo %>% 
  ggplot(aes(x=xcord, y=ycord, label=chapa)) + 
  geom_point(aes(size=dbh1), shape=21) +
  xlim(-16,16) + ylim(-16,16) + 
  geom_point(aes(x=0, y=0), color='gray') +
  theme_bw() +
  gg_circle(radius = 16, xcenter = 0, ycenter = 0) + 
  geom_label_repel(nudge_x = 2, fill="transparent") +
  geom_text_repel(aes(label=cuadrante))

facet_multiple(plot = dist_tipos , facets = 'code', ncol = 2, nrow = 2)
dev.off()





+
  facet_wrap_paginate(~code, ncol = 2, nrow = 2)


ggsave(here::here("/analysis/dist_tipos.pdf"), dist_tipos, width = 29.7, height = 21, units = "cm")




df2 <- df2[!is.na(df2$Sales),]
pdf("C:\\1\\test.pdf", 7, 5)
p <- ggplot(df2, aes(x =Date, y = Sales)) +
  geom_line(aes(colour=id),size = 0.01)+
  scale_x_date(breaks = seq(as.Date("2001-01-01"),
                            as.Date("2007-01-01"), by="1 year"),
               labels = date_format("%Y"))
facet_multiple(plot = p, facets = 'id', ncol = 2, nrow = 2)
dev.off()


  