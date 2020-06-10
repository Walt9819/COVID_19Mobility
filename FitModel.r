library(dplyr)
library(magrittr)
library(ggplot2)
library(EpiEstim)
library(incidence)
library(zoo)
library(plotly)
library(gplots)

path = "Data/"

set.seed(1)
mob <- read.csv(paste0(path, "GlobalMobilityReport.csv"))
mob <- mob %>% filter(country_region_code == "MX")
states <- unique(mob$sub_region_1)
estados <- c("Nacional", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                                              "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Ciudad de Mexico",
                                              "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                                              "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
mob$sub_region_1 <- as.character(mob$sub_region_1)
mob$sub_region_1 <- enc2utf8(mob$sub_region_1)
mob$date <- as.Date(mob$date, format="%Y-%m-%d")

for (state in seq(length(states))){
  mob$sub_region_1[mob$sub_region_1 == states[state]] <- estados[state]
}




names(mob) <- c("Country Code", "Country", "State", "SubState", "Date", "RetailRecreation", "GroceryPharmacy", "Parks", "TransitStations", "Workplaces", "Residential")
mob <- mob %>% select(Date, State, RetailRecreation, GroceryPharmacy, Parks, TransitStations, Workplaces, Residential)
Dates <- c(min(mob$Date), max(mob$Date))
file.entidades <- data.frame(id = c(seq(1, 32), 36, 97, 98, 99), entidad = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche","Coahuila", "Colima",
                                              "Chiapas", "Chihuahua", "Ciudad de Mexico", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                                              "Mexico", "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                                              "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan",
                                              "Zacatecas", "Nacional", "No aplica",
                                              "Se ignora", "No Especificado"))

#file.entidades <- read.csv(paste0(path, "entidades.csv"))
file.entidades$entidad <- as.character(file.entidades$entidad)
#file.entidades$entidad <- enc2utf8(file.entidades$entidad)
totdata <- read.csv(paste0(path, "TotalMX.csv"))
totdata$FECHA_ACTUALIZACION = as.Date(totdata$FECHA_ACTUALIZACION, format="%Y-%m-%d")
totdata$FECHA_INGRESO = as.Date(totdata$FECHA_INGRESO, format="%Y-%m-%d")
totdata$FECHA_SINTOMAS = as.Date(totdata$FECHA_SINTOMAS, format="%Y-%m-%d")
ndat <- totdata %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES) %>% filter(RESULTADO == 1 & FECHA_SINTOMAS >= Dates[1] & FECHA_SINTOMAS <= Dates[2]) %>% summarise("I" = n()) %>% select(FECHA_SINTOMAS, ENTIDAD_RES, I)
ndat <- ndat %>% filter(ENTIDAD_RES <= 32)
days <- seq(as.Date(Dates[1]), as.Date(Dates[2]), 1)

file.entidades$entidad <- as.character(file.entidades$entidad)

for (enti in seq(length(unique(ndat$ENTIDAD_RES)))){
  days0 <- days[!(days %in% (ndat$FECHA_SINTOMAS[ndat$ENTIDAD_RES == file.entidades[enti, 1]]))]
  tmp.ndat <- data.frame(FECHA_SINTOMAS = as.Date(days0), ENTIDAD_RES = file.entidades[enti, 1], I = as.integer(0))
  tmp.ndat <- as.data.frame(tmp.ndat)
  ndat <- rbind(as.data.frame(ndat), tmp.ndat)
  rm(tmp.ndat, days0)
}

for (enti in seq(length(unique(ndat$ENTIDAD_RES)))){
  ndat$ENTIDAD_RES[ndat$ENTIDAD_RES == file.entidades[enti, 1]] <- file.entidades[enti, 2]
}
ndat <- as.data.frame(rbind(ndat, data.frame(ndat %>% group_by(FECHA_SINTOMAS) %>% summarise("I" = sum(I)), ENTIDAD_RES = "Nacional")))
names(ndat) <- c("Date", "State", "I")

###############################################################
mobData <- merge(ndat, mob, by = c("Date", "State"))
###############################################################

mobA <- read.csv(paste0(path, "GlobalMobilityApple.csv"))
mobA <- mobA %>% filter(region == "Mexico" | (country == "Mexico")) %>% filter(transportation_type == "driving")
mobAMX <- mobA %>% filter(geo_type %in% c("sub-region", "country/region") | region == "Mexico City")
mobAMX <- mobAMX %>% select(-c(geo_type, transportation_type, alternative_name, sub.region, country))
mobAMX$region <- c("Nacional", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")

names(mobAMX)[2:length(names(mobAMX))] <- sub("X", "", names(mobAMX)[2:length(names(mobAMX))])
mobDriv <- data.frame()
for (state in seq(nrow(mobAMX))){
  tmp.mobA <- data.frame(State = mobAMX$region[state], Date = names(mobAMX)[2:length(names(mobAMX))], Driving = as.numeric(mobAMX[state, 2:length(mobAMX)]))
  tmp.mobA$Driving[tmp.mobA$Date %in% c("2020.05.11", "2020.05.12")] <- mean(tmp.mobA$Driving[tmp.mobA$Date %in% c("2020.05.09", "2020.05.10", "2020.05.13", "2020.05.14")])
  mobDriv <- rbind(mobDriv, tmp.mobA)
  rm(tmp.mobA)
}
mobDriv$Date <- as.Date(mobDriv$Date, format="%Y.%m.%d")
mobDriv$Driving <- mobDriv$Driving - 100

###############################################################
mobData <- merge(mobData, mobDriv, by = c("Date", "State"))
###############################################################
#mobData <- na.approx(mobData, fromLast = TRUE) ##solución posible (aunque ya corregí y no debe de haber NA's)
mobData[is.na(mobData)] <- 0 #no muy profesional (ni útil)

################################################################################
########### NOW All DATA IS IN ONE DATAFRAME (mobData) #########################
################################################################################

##########################################
############ PCA Analysis ################
##########################################

# libreria #NO BORRAR PESA MUCHO
library(devtools)
#install_github("vqv/ggbiplot")
#install_github("kassambara/factoextra")
library(factoextra)

# datos
#d <- mobData %>% filter(State == state) %>% select(-c(Date,I,State))
#d[is.na(d)] <- 0

##################### Creación del data frame para el ggplot de los PoV de cada estado ##############
PoV.states <- data.frame(PoV = c(0), entidad = c("Nacional", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas"))


####################### Solución #########################################################
PoV.states$entidad <- as.character(PoV.states$entidad)                                #
for(i in seq(length(PoV.states$entidad))){                                            #
  var1 <- mobData %>% filter(State == PoV.states[i, 2]) %>% select(-c(Date,I,State))  #
  ## var1[is.na(d)] <- 0 ¿¿¿ Para qué es esto ??? Chequen línea 92                    #
  pca.var1 = prcomp(var1, center = TRUE, scale. = TRUE)                               #
  PoV <- pca.var1$sdev^2/sum(pca.var1$sdev^2) ##tiene dimension N = 7 (todos los PCA) #
  PoV.states[i,1] = PoV[1] ##agarramos únicamente el primero                          #
}                                                                                     #                                                                                    #
##################### gg plot ######## ################################################
ggplot(PoV.states, aes(x = entidad, y = PoV)) +                                       #
    geom_point(position = position_dodge(width = 0.4)) +                              #
    ylim(0, 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))             #
                                                                                      #

png(filename = "Results/pov.png", width = 1080, height = 1080, units = "px", pointsize = 12, bg = "transparent")
ggplot(PoV.states, aes(x = reorder(entidad,-PoV), y = PoV)) +                                       #
  geom_point(position = position_dodge(width = 0.4)) + ylim(0, 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
###################### Representación del PCA en Queretaro ###############################
pca.dataQro <- mobData %>% filter(State == "Queretaro") %>% select(-c(Date,I,State))
pca.Qro = prcomp(pca.dataQro, center = TRUE, scale. = TRUE)
summary(pca.Qro)
library(ggbiplot)

ggbiplot(pca.Qro)

################# DATA FRAME DE VECTORES #######################################################
pca.vectors <- data.frame("Ciudad"=0,"RetailRecreation"=0,"GroceryPharmacy"=0,"Parks"=0,"TransitStations"=0,"Workplaces"=0,"Residential"=0,"Driving"=0)
for(i in seq(length(PoV.states$entidad))){                                            #
  varState <- mobData %>% filter(State == PoV.states[i, 2]) %>% select(-c(Date,I,State))  #
  pca.state = prcomp(varState, center = TRUE, scale. = TRUE)                               #
  vector_values <- pca.state$rotation
  named_vector_values <- c(PoV.states[i,2],vector_values[1:7])
  if(i==1){
    temp.vector <- rbind(pca.vectors,named_vector_values)
  }
  else{temp.vector <- rbind(temp.vector,named_vector_values)}
  if(i==length(PoV.states$entidad)){
    pca.vectors <- temp.vector
    pca.vectors <- pca.vectors[-1,]

  }
}


###################### Una vez creado el dataframe procedemos a obtener los angulos
#Primero debemos crear la matriz vacía
angle.matrix <- matrix(0,nrow=33,ncol=33)
state.names <- c("Nacional", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
colnames(angle.matrix) <- state.names
rownames(angle.matrix) <- state.names
random.matrix <- angle.matrix
z.val <- angle.matrix

########### Create randomized angles for performing Z-value test ##########
pca.nvectors <- pca.vectors[, 2:length(pca.vectors)]

norm_vec <- function(x) sqrt(sum(x^2))

random.angles = c()
pca.nvectors2 <- sample(pca.nvectors)
for(i in seq(nrow(pca.nvectors2)-1)){
  for(j in seq((i+1), nrow(pca.nvectors2))){
    temp.P <- as.numeric(pca.nvectors2[i, ])
    temp.Q <- as.numeric(pca.nvectors2[j, ])
    dot.product <- sum(temp.P * temp.Q)
    angle.inrad <- acos(dot.product / (norm_vec(temp.P) * norm_vec(temp.Q)))
    angle.indeg <- (angle.inrad * 180) / pi ##maybe it is not necessary to do this
    random.angles <- append(random.angles, angle.indeg) ##all random angles list
  }
}

#ACTUALIZACION ------> Están normalizados los vectores por lo que la magnitud SIEMPRE será 1
#Esto significa que el cálculo del ángulo se reduce al cos^-1 del producto punto
######### Fill just the upper triangle ###########
for(i in seq(32)){
#temp.magnitude <-  sqrt(sum(as.numeric(pca.vectors[i,2:8])^2))
#magnitude.vectors[i,1] <- temp.magnitude

  for(j in seq((i+1), 33)){ #pasando por la matriz...
    temp.P <- as.numeric(pca.vectors[i,2:8]) #Definir vectores
    temp.Q <- as.numeric(pca.vectors[j,2:8])
    dot.product <- 0
    dot.product <- sum(temp.P*temp.Q)
    #dot.product <- temp.P*temp.Q
    angle.inrad <- acos(dot.product)
    angle.indeg <- (angle.inrad * 180) / pi ##maybe it is not necessary to do this
    angle.matrix[i,j]= angle.inrad #Aun no definimos temp.angle, pero al final esa va a ser la accion del for
    z.val[i, j] = (angle.indeg - mean(random.angles) / sd(random.angles))
  }
}

########### Fill all matrix #############
for(i in seq(32)){
  for(j in seq((i+1), 33)){
    angle.matrix[j,i]= angle.matrix[i, j]
    z.val[j, i] = z.val[i, j]
  }
}

###### HEATMAP plot (so cool) ###########
heatmap(angle.matrix, Colv = NA, Rowv = NA, scale="column") ##without R clustering algorithm
heatmap(angle.matrix) ##with R clustering algorithm

png(filename = "Results/angle_heatmap.png", width = 1080, height = 1080, units = "px", pointsize = 12, bg = "transparent")
heatmap.2(angle.matrix, trace = "none", margins = c(7, 7), offsetRow = 0.1, offsetCol = 0.1)
dev.off()

png(filename = "Results/zvalue_heatmap.png", width = 1080, height = 1080, units = "px", pointsize = 12, bg = "transparent")
heatmap.2(z.val, trace = "none", margins = c(7, 7), offsetRow = 0.1, offsetCol = 0.1)
dev.off()


##### Inicia parte René
PoVR.states <- data.frame(PoV = c(0), entidad = estados)
PoVR.states$Vector <- list(c(1,2,3,4,5,6,7))
PoVR.states$entidad <- as.character(PoVR.states$entidad)
####################### Loop para obtener los valores #########################################################

for(i in seq(length(PoVR.states$entidad))){                                            #
  var1 <- mobData %>% filter(State == PoVR.states[i, 2]) %>% select(-c(Date,I,State))  #
  ## var1[is.na(d)] <- 0 ¿¿¿ Para qué es esto ??? Chequen línea 92                    #
  pca.var1 = prcomp(var1, center = TRUE, scale. = TRUE)
  PoVR.states$Vector[[i]] = as.numeric(pca.var1$rotation[,1] )                   #
  PoVR  <- pca.var1$sdev^2/sum(pca.var1$sdev^2) ##tiene dimension N = 7 (todos los PCA) #
  PoVR.states[i,1] = PoVR[1] ##agarramos únicamente el primero                          #
}
#Cálculo de la proyección de los valores de movilidad de cada día sobre la componente principal
#Se guarda sobre mobData (data frame que jusnta todos los datos)
mobData$PPCA <- rep(0,dim(mobData)[1])

for (i in 1:dim(mobData)[1]) {
  PC <- PoVR.states[which(PoVR.states$entidad==mobData[i,	"State"]),"Vector"]
  PC <- PC[[1]]
  mobData$PPCA[i] = as.numeric(mobData[i,4:(dim(mobData)[2]-1)])%*%PC/norm_vec(PC)
}
#Agrego una columna para indicar el periodo temporal en el que se encuantra dado día para el calculo de R
mobData$Period = rep(0,dim(mobData)[1])






##########################################
#### R(t) inference from data ############
##########################################
#-------------------------Funciones-------------------------------------------
#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_control<-function(d,t_st,t_e,mean_gamma,sd_gamma){
  rts_control <- estimate_R(d,
                         method="parametric_si",
                         config = make_config(list(
                           t_start = t_st,
                           t_end = t_e,
                           mean_si = mean_gamma,
                           std_si = sd_gamma))
  )
  return(rts_control)
}

#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_uncertain<-function(data,config_gamma){
  rts_uncertain <- estimate_R(data,
                              method = "uncertain_si",
                              config = config_gamma)
  return(rts_uncertain)
}

#-------------------------Parámetros-------------------------------------------
#Considerando la ultima ventana de 7 dias
#Además se calcula por estado
RS <- data.frame(estado = estados)
RS$RMedia <- list(c(1,2,3))
RS$estado <- as.character(RS$estado)
RS$RStd<- list(c(1,2,3))
sinceQuarintine <- as.numeric(as.Date("2020-03-23") - as.Date(Dates[1]))
for (i in estados) {
  d <- data.frame("dates" = mobData %>% filter(State == i) %>% select(Date), "I" = mobData %>% filter(State == i) %>% select(I))
  t_start <- c(2, sinceQuarintine, nrow(d)-7)
  t_end <- c(sinceQuarintine - 1, nrow(d)-8, nrow(d))
  for (j in 1:length(t_start)) {
    mobData$Period[mobData$State==i & mobData$Date %in% d$Date[t_start[j]:t_end[j]]] = j
  }

  #Parametros de distribucion Gamma a priori
  #------Parametros de Nishiura
  mean_nish<-4.7
  sd_nish<-2.9
  #------Parametros de Du
  mean_du<-3.96
  sd_du<-4.75

  #--------------------------Uncertain---------------------------------------
  config_nish <- make_config(list(t_start = t_start, t_end = t_end,
                                  mean_si = 4.7, std_mean_si = 1,
                                  min_mean_si = 3.1, max_mean_si = 6.3,
                                  std_si = 2.9, std_std_si = 0.5,
                                  min_std_si = 1.4, max_std_si = 4.4))

  config_du <- make_config(list(t_start = t_start, t_end = t_end,
                                mean_si = 3.96, std_mean_si = 1,
                                min_mean_si = 2.36, max_mean_si = 5.56,
                                std_si = 4.75, std_std_si = 0.5,
                                min_std_si = 3.25, max_std_si = 6.25))
  #calcular r

  rts_control_du= tryCatch(
    fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du),warning=function(w) {
      #print(i)
      tmp_r <- fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
      #print(plot(tmp_r, "R")+ geom_hline(aes(yintercept = 1), color = "red", lty = 2))
      return(tmp_r)
    }
  )
  RS$RMedia[which(RS$estado==i)][[1]] = as.numeric(rts_control_du$R[,"Mean(R)"])
  RS$RStd[which(RS$estado==i)][[1]] = as.numeric(rts_control_du$R[,'Std(R)'])

}
#Finalmente gráficar

f <- list(
  family = "Roboto Slab",
  size = 18,
  color = "#000"
)
x <- list(
  title = "R",
  titlefont = f
)
y <- list(
  title = "Proy 1°CP",
  titlefont = f
)
fig <- plot_ly()
lay.UpdateMenus <- list()
for (i in estados) {
  datosEstado <- mobData[which(mobData$State==i&mobData$Period!=0),]
  datosMeanEstado <- datosEstado %>% group_by(Period) %>% summarise(MPPCA = mean(PPCA),sd = sd(PPCA))

  x_datos <- RS[which(RS$estado==i),"RMedia"][[1]][datosEstado$Period]
  x_mean <-RS[which(RS$estado==i),"RMedia"][[1]][datosMeanEstado$Period]

  y_datos <- datosEstado$PPCA
  y_mean <- datosMeanEstado$MPPCA

  y_error <- datosMeanEstado$sd

  y_sdl <- datosMeanEstado$MPPCA-datosMeanEstado$sd
  y_sdh <- datosMeanEstado$MPPCA+datosMeanEstado$sd

  x_error <- RS[which(RS$estado==i),"RStd"][[1]][datosMeanEstado$Period]

  fig <- fig %>% add_trace(
    mode = 'lines',
    type="scatter",
    y = y_sdl[c(1,2)],
    x = x_mean[c(1,2)],
    showlegend = FALSE,
    name="Error1L",
    visible = FALSE,
    line = list(
      color = 'rgba(0,0,0,0)'
    )
  )
  fig <- fig %>% add_trace(
    mode = 'lines',
    type="scatter",
    y = y_sdh[c(1,2)],
    x = x_mean[c(1,2)],
    fill = 'tonexty',
    fillcolor='rgba(0,0,0,.1)',
    showlegend = FALSE,
    name="Error1S",
    visible = FALSE,
    line = list(
      color = 'rgba(0,0,0,0)'
    )
  )
  fig <- fig %>% add_trace(
    mode = 'lines',
    type="scatter",
    y = y_sdl[c(2,3)],
    x = x_mean[c(2,3)],
    showlegend = FALSE,
    name="Error2L",
    visible = FALSE,
    line = list(
      color = 'rgba(0,0,0,0)'
    )
  )
  fig <- fig %>% add_trace(
    mode = 'lines',
    type="scatter",
    y = y_sdh[c(2,3)],
    x = x_mean[c(2,3)],
    showlegend = FALSE,
    fill = 'tonexty',
    fillcolor='rgba(0,120,12,.2)',
    name="Error2S",
    visible = FALSE,
    line = list(
      color = 'rgba(0,0,0,0)'
    )
  )


  fig <- fig %>%  add_trace(
    x = x_datos,
    y = y_datos,
    type = 'scatter',
    mode = 'markers',
    name="Datos",
    visible = FALSE,
    marker = list(
      color = '#1f77b4'
    )
  )
  fig <- fig %>% add_trace(
    x =x_mean,
    y = y_mean,
    type = 'scatter',
    mode = "lines+markers",
    name="Medias",
    visible = FALSE,
    line = list(
      color = '#ff7f0e'
    ),
    marker = list(
      color = '#ff7f0e'
    ),
    error_y = list(
      type = "data",
      array = y_error,
      color = 'black'
    ),
    error_x =  list(
      type = "data",
      array = x_error,
      color = 'red'
    )
  )


  lay.UpdateMenus[[length(lay.UpdateMenus)+1]] <- list(
    method = "restyle",
    args = list("visible",rep(estados,each=6) == i),
    label = i

  )

}
fig <- fig %>% layout(
  xaxis = x,
  yaxis = y,
  updatemenus = list(
      list(
        y = 0.7,
        buttons = lay.UpdateMenus
      )
  )
)
fig


#--------------------------Evaluar R(t)---------------------------------------

# rts_control_nish=fun.estim_rts_control(d, mean_nish, sd_nish)
# plot(rts_control_nish, "R")+
#   geom_hline(aes(yintercept = 1), color = "red", lty = 2)
#
# rts_control_du=fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
# plot(rts_control_du, "R")+
#   geom_hline(aes(yintercept = 1), color = "red", lty = 2)
#
#
# rts_uncertain_nish=fun.estim_rts_uncertain(d,config_nish)
# plot(rts_uncertain_nish, legend = TRUE)
#
# rts_uncertain_du=fun.estim_rts_uncertain(d,config_du)
# plot(rts_uncertain_du, legend = TRUE)
