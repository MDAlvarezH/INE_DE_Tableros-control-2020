############### Graficas Tableros de control_Movimientos PE_VF ###########
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
### Autor: Miguel David Alvarez Hernández
### Ultima versión : 20/06/2020


#----------------------------------------------------------------------------------#
################################# Paquetes y setup #################################
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
############################# Datos originales #####################################
#----------------------------------------------------------------------------------#

#Datos por semana de todos los tramites
Datos <- read_csv("Datos_Movimientos_PE_proc.csv",
                  locale = locale(encoding = 'latin1'),
                  col_types = cols(Fecha = col_date(format = "%Y-%m-%d")))

#NIVELES factor para entidades
estados <- unique(Datos$Edo)[1:32]
niveles <- c("NACIONAL",estados)

#convertir a factor la columna estados
Datos$Edo <- factor(Datos$Edo, levels = niveles)
#levels(Datos$Edo)

#convertir a factor la columna tramite
Datos$Tramite <- as.factor(Datos$Tramite)

#View(Datos)

#----------------------------------------------------------------------------------#
############################# transformacion de datos ##############################
#----------------------------------------------------------------------------------#

#Datos por semana sin tramite de tipo: entrega de credenciales
Datos_sin_credenciales <- Datos %>% filter(Tramite != 'Entrega_credencial')
Datos_sin_credenciales$Tramite <- as.character(Datos_sin_credenciales$Tramite)
Datos_sin_credenciales$Tramite <- as.factor(Datos_sin_credenciales$Tramite)
#levels(Datos_sin_credenciales$Edo)
#View(Datos_sin_credenciales)

#Datos por semana de entregas de credencial y total de solicitudes (suma de los tramites)
Datos_sol_cred <- Datos_sin_credenciales %>% 
  group_by(año,mes,dia,Fecha,Semana,Edo) %>% 
  summarize(Numero = sum(Numero)) %>% 
  mutate(Tramite = as.factor("Total_solicitudes")) %>% 
  ungroup()
#se pegan con los registros de entrega de credenciales
Datos_sol_cred <- bind_rows(Datos_sol_cred, 
                            Datos %>% filter(Tramite == 'Entrega_credencial'))
Datos_sol_cred$Tramite <- as.factor(Datos_sol_cred$Tramite)


#Datos por año de todos los tramites
Datos_todos_año <- Datos %>% 
  group_by(año,Edo,Tramite) %>% 
  summarize(conteo = sum(Numero))

#Datos por año (porcentaje respecto al total de tramites por año)
Datos_todos_año_porcentaje <- Datos_todos_año %>% 
  ungroup() %>% 
  group_by(año,Edo) %>% 
  summarise(total = sum(conteo))
Datos_todos_año_porcentaje <- left_join(Datos_todos_año, Datos_todos_año_porcentaje, by = c("año","Edo"))
Datos_todos_año_porcentaje <- Datos_todos_año_porcentaje %>% mutate(Porcentaje = (conteo/total)*100)



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
  title = "Número de trámites",
  titlefont = f
)

yy2 <- list(
  title = "Porcentaje (%)",
  titlefont = f
) 

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
##### line chart (menu por entidad, total solicitudes y entrega credenciales)  #####
#----------------------------------------------------------------------------------#

fig0 <- Datos_sol_cred %>% 
  plot_ly(
    x = ~Fecha, 
    y = ~Numero,
    customdata = ~Edo,
    color = ~factor(Tramite),
    text = ~paste(Edo,
                  '<br>Tipo de trámite:', Tramite,
                  '<br>Fecha de corte:', Fecha,
                  '<br>Semana:', Semana,
                  '<br>Número de trámites semanal:', format(Numero,big.mark=",", trim=TRUE)
    ),
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = "customdata",
        operation = '=',
        value = unique(Datos_sol_cred$Edo)[1]
      )
    )) %>% 
  add_trace(type = 'scatter',
            mode='lines+markers'
  )%>% 
  layout(xaxis = xx, 
         yaxis = yy,
         legend = list(
           font = list(
             size = 12,
             color = "#000"),
           orientation = 'h',
           x = 0.4, 
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
                buttons = apply(as.data.frame(as.factor(unique(Datos_sol_cred$Edo))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         )
  )


#fig0
htmlwidgets::saveWidget(as_widget(fig0), "Rplot_Mov-PE_line-chart_menu-edo_totalsol-cred.html")



#----------------------------------------------------------------------------------#
######## Line chart (menu por entidad, color tipo movimiento, sin credenciales)  ###
#----------------------------------------------------------------------------------#

fig <- Datos_sin_credenciales %>% 
  plot_ly(
    x = ~Fecha, 
    y = ~Numero,
    color = ~Tramite,
    text = ~paste(Edo,
                  '<br>Tipo de trámite:', Tramite,
                  '<br>Fecha de corte:', Fecha,
                  '<br>Semana:', Semana,
                  '<br>Número de trámites semanal:', format(Numero,big.mark=",", trim=TRUE)
                  ),
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~Edo,
        operation = '=',
        value = unique(Datos_sin_credenciales$Edo)[33]
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
          size = 12,
          color = "#000"),
          orientation = 'h',
          x = 0.2, 
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
               buttons = apply(as.data.frame(as.factor(niveles)), 1, 
                               function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
        )
  )

#fig
htmlwidgets::saveWidget(as_widget(fig), "Rplot_Mov-PE_line-chart_menu-edo_solicitudes.html")


#----------------------------------------------------------------------------------#
################### Line chart (menu por tramite, color por edo)  ##################
#----------------------------------------------------------------------------------#

fig1 <- Datos %>% 
  plot_ly(x = ~Fecha,
          y = ~Numero,
          color = ~Edo,
          customdata = ~Tramite,
          hoverinfo = 'text',
          text = ~paste(Edo,
                        '<br>Tipo de trámite:', Tramite,
                        '<br>Fecha de corte:', Fecha,
                        '<br>Semana:', Semana,
                        '<br>Número de trámites semanal:', format(Numero,big.mark=",", trim=TRUE)),
          transforms = list(
            list(
              type = 'filter',
              target = ~'customdata',
              operation = '=',
              value = as.factor(unique(Datos$Tramite)[1])))
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
           y = 1),
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
                buttons = apply(as.data.frame(as.factor(unique(Datos$Tramite))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         )
  )

#fig1
htmlwidgets::saveWidget(as_widget(fig1), "Rplot_Mov-PE_line-chart_menu-tramite_todostramites.html")


#----------------------------------------------------------------------------------#
############## Grouped bar por año (menu por estado, color por tramite) ############
#----------------------------------------------------------------------------------#

fig2 <- plot_ly(Datos_todos_año, 
                x = ~as.factor(año), 
                y = ~conteo,
                customdata = ~Edo,
                color = ~Tramite,
                type = 'bar',
                visible = 'legendonly',
                hoverinfo = 'text',
                text = ~paste(Edo,
                              '<br>Tipo de trámite:', Tramite,
                              '<br>Año:', año,
                              '<br>Número de trámites anual:', format(conteo,big.mark=",", trim=TRUE)),
                transforms = list(
                  list(
                    type = 'filter',
                    target = 'customdata',
                    operation = '=',
                    value = as.factor(unique(Datos_todos_año$Edo)[1])
                  )
                )
) %>% 
  layout(barmode = 'group',
         xaxis = xx, 
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
                buttons = apply(as.data.frame(as.factor(unique(Datos_todos_año$Edo))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         ))



#fig2
htmlwidgets::saveWidget(as_widget(fig2), "Rplot_Mov-PE_bar-chart_menu-edo_todos-anual.html")



#----------------------------------------------------------------------------------#
############## Stacked bar por año (menu por estado, color por tramite) ############
#----------------------------------------------------------------------------------#

fig3 <- plot_ly(Datos_todos_año_porcentaje, 
                x = ~Edo, 
                y = ~Porcentaje,
                customdata = ~año,
                color = ~Tramite,
                type = 'bar',
                hoverinfo = 'text',
                text = ~paste(Edo,
                              '<br>Tipo de trámite:', Tramite,
                              '<br>Año:', año,
                              '<br>Número de trámites anual:', format(conteo,big.mark=",", trim=TRUE),
                              '<br>Porcentaje (respecto al total de trámites en la entidad):', round(Porcentaje, 2),'%'),
                transforms = list(
                  list(
                    type = 'filter',
                    target = 'customdata',
                    operation = '=',
                    value = as.numeric(unique(Datos_todos_año_porcentaje$año)[1])
                  )
                )
) %>% 
  layout(barmode = 'stack',
         xaxis = xx2, 
         yaxis = yy2,
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
                buttons = apply(as.data.frame(as.numeric(unique(Datos_todos_año_porcentaje$año))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         ))



#fig3
htmlwidgets::saveWidget(as_widget(fig3), "Rplot_Mov-PE_barchart-stack_menu-año_todos-porcentaje.html")


#----------------------------------------------------------------------------------#
######################## Pie chart (menu por entidad y año)  #######################
#----------------------------------------------------------------------------------#


fig4 <- Datos_todos_año_porcentaje %>% plot_ly(labels = ~Tramite, 
                           values = ~conteo,
                           customdata = ~año,
                           textinfo = 'percent',
                           transforms = list(
                             list(
                               type = 'filter',
                               target = ~Edo,
                               operation = '=',
                               value = unique(Datos_todos_año_porcentaje$Edo)[1]
                             )
                           ,list(
                             type = 'filter',
                             target = ~año,
                             operation = '=',
                             value = as.numeric(unique(Datos_todos_año_porcentaje$año)[1])
                           ))) %>% 
                      add_pie(hole = 0.6) %>% 
                      layout(updatemenus = list(
                                list(direction = "down",
                                     xanchor = 'right',
                                     yanchor = "top",
                                     x = 0.2,
                                     y= 1.1,
                                     font = list(
                                       size = 10),
                                    type = 'dropdown',
                                    active = 0,
                                    buttons = apply(as.data.frame(unique(Datos_todos_año_porcentaje$Edo)), 1, function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x))),
                                list(direction = "down",
                                     xanchor = 'center',
                                     yanchor = "top",
                                     x = 0.3,
                                     y= 1.1,
                                     font = list(
                                       size = 10),
                                    type = 'dropdown',
                                    active = 0,
                                    buttons = apply(as.data.frame(as.numeric(unique(Datos_todos_año_porcentaje$año))), 1, function(x) list(method = 'restyle',args = list('transforms[1].value',x),label = x)))),
                        title = "",  
                        showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F))



#fig4
htmlwidgets::saveWidget(as_widget(fig4), "Rplot_Mov-PE_pie-chart_menu-edo-año.html")







