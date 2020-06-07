library(dplyr)
library(magrittr)
library(ggplot2)
library(EpiEstim)
library(incidence)
library(zoo) ##importar la librería, por favor ;)

#path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"
path = "Data/"
set.seed(1)
mob <- read.csv(paste0(path, "GlobalMobilityReport.csv"))
mob <- mob %>% filter(country_region_code == "MX")
states <- unique(mob$sub_region_1)
estados <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                                              "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Ciudad de Mexico",
                                              "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                                              "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
mob$sub_region_1 <- as.character(mob$sub_region_1)
#mob$sub_region_1 <- enc2utf8(mob$sub_region_1)
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
                                              "Zacatecas", "Estados Unidos Mexicanos", "No aplica",
                                              "Se ignora", "No Especificado"))

#file.entidades <- read.csv(paste0(path, "entidades.csv"))
file.entidades$entidad <- as.character(file.entidades$entidad)
#file.entidades$entidad <- enc2utf8(file.entidades$entidad)
totdata <- read.csv(paste0(path, "TotalMX.csv"))

totdata$FECHA_ACTUALIZACION = as.Date(totdata$FECHA_ACTUALIZACION)
totdata$FECHA_INGRESO = as.Date(totdata$FECHA_INGRESO)
totdata$FECHA_SINTOMAS = as.Date(totdata$FECHA_SINTOMAS)
ndat <- totdata %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES) %>% filter(RESULTADO == 1 & FECHA_SINTOMAS >= Dates[1] & FECHA_SINTOMAS <= Dates[2]) %>% summarise("I" = n()) %>% select(FECHA_SINTOMAS, ENTIDAD_RES, I)
ndat <- ndat %>% filter(ENTIDAD_RES <= 32)

days <- seq(as.Date(Dates[1]), as.Date(Dates[2]), 1)
for (enti in seq(length(unique(ndat$ENTIDAD_RES)))){
  days0 <- days[!(days %in% (ndat$FECHA_SINTOMAS[ndat$ENTIDAD_RES == file.entidades[enti, 1]]))]
  tmp.ndat <- data.frame(FECHA_SINTOMAS = days0, ENTIDAD_RES = file.entidades[enti, 1], I = as.integer(0))
  tmp.ndat <- as.data.frame(tmp.ndat)
  ndat <- rbind(as.data.frame(ndat), tmp.ndat)
}

for (enti in seq(length(unique(ndat$ENTIDAD_RES)))){
  ndat$ENTIDAD_RES[ndat$ENTIDAD_RES == file.entidades[enti, 1]] <- file.entidades[enti, 2]
}
ndat <- as.data.frame(rbind(ndat, data.frame(ndat %>% group_by(FECHA_SINTOMAS) %>% summarise("I" = sum(I)), ENTIDAD_RES = "Estados Unidos Mexicanos")))
names(ndat) <- c("Date", "State", "I")

mob %>% select(State) %>% unique()
###############################################################
mobData <- merge(ndat, mob, by = c("Date", "State"))
###############################################################

mobA <- read.csv(paste0(path, "GlobalMobilityApple.csv"))
mobA <- mobA %>% filter(region == "Mexico" | (country == "Mexico")) %>% filter(transportation_type == "driving")
mobAMX <- mobA %>% filter(geo_type %in% c("sub-region", "country/region") | region == "Mexico City")
mobAMX <- mobAMX %>% select(-c(geo_type, transportation_type, alternative_name, sub.region, country))
mobAMX$region <- c("Estados Unidos Mexicanos", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")

names(mobAMX)[2:length(names(mobAMX))] <- sub("X", "", names(mobAMX)[2:length(names(mobAMX))])
mobDriv <- data.frame()
for (state in seq(nrow(mobAMX)))
{
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

## Select State ##
mobData %>% select("State") %>% unique() %>% as.character() ##all states list
state <- "Queretaro"


##########################################
############ PCA Analysis ################
##########################################

# libreria #NO BORRAR PESA MUCHO
library(devtools)
#install_github("kassambara/factoextra")
library(factoextra)

# datos
#d <- mobData %>% filter(State == state) %>% select(-c(Date,I,State))
#d[is.na(d)] <- 0

##################### Creación del data frame para el ggplot de los PoV de cada estado ##############
PoV.states <- data.frame(PoV = c(0), entidad = c("Estados Unidos Mexicanos", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
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
state.names <- c("Estados Unidos Mexicanos", "Ciudad de Mexico", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Mexico", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
colnames(angle.matrix) <- state.names
rownames(angle.matrix) <- state.names

#Luego debemos hacer el for para ir agregando los valores
for(i in seq(33)){
  for(j in seq(33)){ #pasando por la matriz...

    angle.matrix[i,j]=temp.angle #Aun no definimos temp.angle, pero al final esa va a ser la accion del for
  }
}

                    ########### Intentos anteriores, (uso del while)################
#while(i<=33){
#  var1 <- mobData %>% filter(State == (as.character(PoV.states[i,2]))) %>% select(-c(Date,I,State))
#  var1[is.na(var1)] <- 0
#  pca.var1 = prcomp(var1, center = TRUE, scale. = TRUE)
#  PoV <- pca.var1$sdev^2/sum(pca.var1$sdev^2)
#  PoV.states[i,1] = PoV
#  i=i+1
#}
#PoV.states[3,1]
 ############################ PCA CON factoextra ##########################################

#pca.kikis <- prcomp(var1, center = TRUE, scale. = TRUE)
#PC1s = predict(pca.kikis, newdata= mobData)
#print(PC1s)
#head(PC1s,4)
#var <- get_pca_var(pca.kikis)
#var
#####################################################################################
## Plot mobility data before and after PCA ##
png(paste0(path, "Data.jpg"))
plot(d)
dev.off()
png(paste0(path, "DataPCA.jpg"))
plot(d * Eigen$vectors[1,])
dev.off()


##########################################
#### R(t) inference from data ############
##########################################
#-------------------------Funciones-------------------------------------------
#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_control<-function(d,t_st,t_e,mean_gamma,sd_gamma)
{
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
fun.estim_rts_uncertain<-function(data,config_gamma)
{ rts_uncertain <- estimate_R(data,
                              method = "uncertain_si",
                              config = config_gamma)
  return(rts_uncertain)
}

#-------------------------Parámetros-------------------------------------------
#Considerando la ultima ventana de 7 dias
sinceQuarintine <- as.numeric(as.Date("2020-03-23") - as.Date(Dates[1]))
t_start <- c(2, sinceQuarintine, nrow(d)-7)
t_end <- c(sinceQuarintine - 1, nrow(d)-8, nrow(d))

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

#--------------------------Evaluar R(t)---------------------------------------
d <- data.frame("dates" = mobData %>% filter(State == state) %>% select(Date), "I" = mobData %>% filter(State == state) %>% select(I))

rts_control_nish=fun.estim_rts_control(d, mean_nish, sd_nish)
plot(rts_control_nish, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

rts_control_du=fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
plot(rts_control_du, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)


rts_uncertain_nish=fun.estim_rts_uncertain(d,config_nish)
plot(rts_uncertain_nish, legend = TRUE)

rts_uncertain_du=fun.estim_rts_uncertain(d,config_du)
plot(rts_uncertain_du, legend = TRUE)
