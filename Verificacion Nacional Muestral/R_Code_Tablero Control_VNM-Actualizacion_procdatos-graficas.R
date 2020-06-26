############### Tableros de control_VNM-Actualizacion_VF ###########
############### Procesamiento de datos y graficas ###############
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
### Autor: Miguel David Alvarez Hernández
### Ultima versión : 20/06/2020


#----------------------------------------------------------------------------------#
################################### Paquetes y setup ###############################
#----------------------------------------------------------------------------------#

library(pacman)
p_load(tidyverse,
       lubridate,
       plotly,
       ggthemes,
       RColorBrewer)

#Prevenir notación científica
options(scipen=999) 
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())



#----------------------------------------------------------------------------------#
####################################### Datos ######################################
#----------------------------------------------------------------------------------#

Datos_VNM_A <- read_csv("Datos_VNM_Actualizacion.csv", 
                                col_types = cols(Indicador = col_double(), 
                                                 Limite_Inferior = col_double(), 
                                                 Limite_Superior = col_double(),
                                                 `año` = col_date(format = "%Y")))


#convertir a factor la columna estados
Datos_VNM_A$Edo <- as.factor(Datos_VNM_A$Edo)

#convertir a factor la columna tramite
Datos_VNM_A$Nombre_Indicador <- as.factor(Datos_VNM_A$Nombre_Indicador)


View(Datos_VNM_A)



#----------------------------------------------------------------------------------#
################################ Procesamiento Datos  ##############################
#----------------------------------------------------------------------------------#

#se eliminan las filas sobrantes
Datos_VNM_A <- Datos_VNM_A %>% drop_na(Edo)

#se renombran las columnas
Datos_VNM_A <- Datos_VNM_A %>%
  rename(
    Fecha = año)

#se obtienen las claves de los estados presentes 
clv_edo <- unique(unlist(Datos_VNM_A$Edo))
#View(clv_edo)

#numero de valores vacíos en indicador
sum(is.na(Datos_VNM_A$Indicador))

#cambiar nan en columna indicador y limites por ceros
#Datos_VNM_A2 <- Datos_VNM_A %>% replace_na(list(Indicador = 0))
#Datos_VNM_A2 <- Datos_VNM_A2 %>% replace_na(list(Limite_Inferior = 0))
#Datos_VNM_A2 <- Datos_VNM_A2 %>% replace_na(list(Limite_Superior = 0))

#eliminar renglones con nan en indicador y limites
Datos_VNM_A2 <- Datos_VNM_A %>% drop_na(Indicador, Limite_Inferior, Limite_Superior)

sum(is.na(Datos_VNM_A2$Indicador))

#añadimos colunma adicional con la diferencia entre los límites inferior y superior
Datos_VNM_A2 <- Datos_VNM_A2 %>% mutate(sd = (Limite_Superior - Limite_Inferior)/2)
#View(Datos_VNM_A2)

#añadimos colunma adicional con año en formato numerico
Datos_VNM_A2 <- Datos_VNM_A2 %>% mutate(año = year(Fecha))
View(Datos_VNM_A2)

#se guardan los resultados
#write.csv(Datos_VNM_A2,"Datos_VNM_Actualizacion_Proc.csv", row.names = FALSE)

Datos_VNM_A3 <- Datos_VNM_A2 %>% filter(Edo != 'NACIONAL')
Datos_VNM_A3$Edo <- as.character(Datos_VNM_A3$Edo)
Datos_VNM_A3$Edo <- as.factor(Datos_VNM_A3$Edo)

Datos <- Datos_VNM_A2

Datos2 <- Datos_VNM_A3

#----------------------------------------------------------------------------------#
##################################### Graficas #####################################
#----------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#
############################### opciones estéticas #################################
#----------------------------------------------------------------------------------#

f <- list(
  size = 14,
  color = "#7f7f7f"
)

xx <- list(
  title = "Fecha",
  titlefont = f
)

xx2 <- list(
  title = "Entidad",
  titlefont = f
)

yy <- list(
  title = "Porcentaje (%)",
  titlefont = f
)

l <- list(
  font = list(
    size = 8,
    color = "#000"),
  orientation = 'v',
  x = -400, 
  y = 0.5)

l1 <- list(
  font = list(
    size = 8,
    color = "#000"),
  orientation = 'h')


# funcion para generar la lista de opciones del menu 
#(ver: https://stackoverflow.com/questions/55075168/loop-plotly-menu)

get_menu_list <- function(names){
  n_names = length(names)
  buttons = vector("list",n_names)
  
  for(i in seq_along(buttons)){
    buttons[i] = list(list(method = "restyle",
                           args = list("transforms[0].value", names[i]),
                           label = names[i]))
  }
  
  return_list = list(
    list(
      type = 'dropdown',
      active = 0,
      buttons = buttons
    )
  )
  
  return(return_list)
}


#----------------------------------------------------------------------------------#
################## line chart (menu por entidad, color por indicador)  #############
#----------------------------------------------------------------------------------#

fig <- Datos %>% 
  plot_ly(
    x = ~Fecha, 
    y = ~Indicador,
    customdata = ~Edo,
    color = ~Nombre_Indicador,
    text = ~paste(Edo,
                  '<br>Año:', year(Fecha),
                  '<br>Indicador:', Nombre_Indicador,
                  '<br>Porcentaje:', round(Indicador,2),'%',
                  '<br>Limite inferior:', round(Limite_Inferior,2),'%',
                  '<br>Limite superior:', round(Limite_Superior,2),'%'),
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = 'customdata',
        operation = '=',
        value = unique(Datos$Edo)[1]
      )
    )) %>% 
  add_trace(type = 'scatter',
            mode='lines+markers',
            visible = 'legendonly'
  )%>% 
  layout(xaxis = xx, 
         yaxis = yy,
         legend = list(
           font = list(
             size = 10,
             color = "#000"),
           orientation = 'h',
           x = 0.0, 
           y = -0.2),
         updatemenus = list(
           list(direction = "right",
                xanchor = 'right',
                yanchor = "top",
                x = 0.1,
                y= 1.1,
                font = list(
                  size = 10),
                type = 'dropdown',
                active = 0,
                buttons = apply(as.data.frame(as.factor(unique(Datos$Edo))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         )
  )

#fig
htmlwidgets::saveWidget(as_widget(fig), "Rplot_VNM-Actualizacion_line-chart_menu-edo.html")


#----------------------------------------------------------------------------------#
##################### line chart (menu por tramite, color por edo)  ################
#----------------------------------------------------------------------------------#

fig1 <- Datos %>% 
  plot_ly(x = ~Fecha,
          y = ~Indicador,
          color = ~factor(Edo, levels=as.character(unique(Datos$Edo))),
          customdata = ~Nombre_Indicador,
          hoverinfo = 'text',
          text = ~paste(Edo,
                        '<br>Año:', year(Fecha),
                        '<br>Indicador:', Nombre_Indicador,
                        '<br>Porcentaje:', round(Indicador,2),'%',
                        '<br>Limite inferior:', round(Limite_Inferior,2),'%',
                        '<br>Limite superior:', round(Limite_Superior,2),'%'),
          transforms = list(
            list(
              type = 'filter',
              target = ~'customdata',
              operation = '=',
              value = as.factor(unique(Datos$Nombre_Indicador)[1])))
  ) %>% 
  add_trace(type = 'scatter',
            mode='lines+markers',
            visible = 'legendonly'
  ) %>% 
  layout(xaxis = xx, 
         yaxis = yy,
         legend = list(
           font = list(
             size = 10,
             color = "#000"),
           orientation = 'v',
           x = -0.2, 
           y = 0.9),
         updatemenus = list(
           list(direction = "right",
                xanchor = 'left',
                yanchor = "top",
                x = 0.0,
                y= 1.1,
                font = list(
                  size = 10),
                type = 'dropdown',
                active = 0,
                buttons = apply(as.data.frame(as.factor(unique(Datos$Nombre_Indicador))), 1, function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         )
  )

#fig1
htmlwidgets::saveWidget(as_widget(fig1), "Rplot_VNM-Actualizacion_line-chart_menu-indicador.html")



#----------------------------------------------------------------------------------#
####################### grouped bar por estado (menu por año) ######################
#----------------------------------------------------------------------------------#

fig2 <- plot_ly(Datos2, 
                x = ~Edo, 
                y = ~Indicador,
                customdata = ~año,
                color = ~Nombre_Indicador,
                type = 'bar',
                visible = 'legendonly',
                hoverinfo = 'text',
                text = ~paste(Edo,
                              '<br>Indicador:', Nombre_Indicador,
                              '<br>Porcentaje:', round(Indicador,2),'%',
                              '<br>Limite inferior:', round(Limite_Inferior,2),'%',
                              '<br>Limite superior:', round(Limite_Superior,2),'%'),
                transforms = list(
                  list(
                    type = 'filter',
                    target = 'customdata',
                    operation = '=',
                    value = as.numeric(unique(Datos2$año)[1])
                  ))) %>% 
  layout(barmode = 'group',
         xaxis = xx2, 
         yaxis = yy,
         legend = list(
           font = list(
             size = 10,
             color = "#000"),
           orientation = 'h',
           x = 0.0, 
           y = -0.2),
         updatemenus = list(
           list(direction = "right",
                xanchor = 'left',
                yanchor = "top",
                x = 0.0,
                y= 1.1,
                font = list(
                  size = 10),
                type = 'dropdown',
                active = 0,
                buttons = apply(as.data.frame(as.numeric(unique(Datos2$año))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         ))

#fig2
htmlwidgets::saveWidget(as_widget(fig2), "Rplot_VNM-Actualizacion_bar-chart_menu-año.html")


