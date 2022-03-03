library(tidyverse)
library(lubridate)

victimas <- read_csv('./data/victimas-en-carpetas-de-investigacion-pgj.csv')

str(victimas)

head(victimas)
tail(victimas, 20)

colnames(victimas)

 ano <- victimas %>% group_by(Año_hecho) %>% count()
tail(ano, 15)
 
# clean data 

victimas_transporte <- victimas %>% select(Delito, Categoria, Sexo, Edad, 
                                FechaHecho, HoraHecho,geopoint,
                                AlcaldiaHechos, ColoniaHechos,
                                latitud, longitud) %>% 
                        #fix date into unique datetime column, remove ld date, time columns
                        mutate(datetime = dmy_hms(paste0(FechaHecho, " ", HoraHecho ))) %>% 
                        select(-FechaHecho, -HoraHecho) %>% 
                        # filter crimes that have a "pasajero" in them
  # MAYBE ADD TRANSEUNTE
                        filter(str_detect( Delito, "PASAJERO") | str_detect(Delito, "TAXI")) %>% 
                        mutate( modo = factor(case_when(
                            is.na(Delito) ~ NA_character_,
                            str_detect(Delito, "METROBUS") ~ "METROBUS",
                            str_detect(Delito, "METRO") ~ "METRO",
                            str_detect(Delito, "PESERO") ~ "PESERO",
                            str_detect(Delito, "TRANSPORTE PÚBLICO") ~ "TRANSPORTE PUBLICO",
                            str_detect(Delito, "AUTOBÚS FORÁNEO") ~ "AUTOBUS FORANEO",
                            str_detect(Delito, "AUTOBUS FORANEO") ~ "AUTOBUS FORANEO",
                            str_detect(Delito, "ECOBUS") ~ "ECOBUS",
                            str_detect(Delito, "RTP") ~ "RTP",
                            str_detect(Delito, "TREN LIGERO") ~ "TREN LIGERO",
                            str_detect(Delito, "TROLEBUS") ~ "TROLEBUS",
                            str_detect(Delito, "VEHICULO") ~ "VEHICULO",
                            str_detect(Delito, "TREN SUBURBANO") ~ "TREN SUBURBANO",
                            str_detect(Delito, "TAXI") ~ "TAXI"
                          ))) %>% 
  
                      mutate( violencia = factor(case_when(
                        is.na(Delito) ~ NA_character_,
                        str_detect(Delito, "CON VIOLENCIA") ~ "Si",
                        str_detect(Delito, "SIN VIOLENCIA") ~ "No"
                      )))

#victimas_atr <-victimas %>% filter(str_detect(Delito, "ATROPELLADO"))

# horas_delito_transporte 

modos_principales <- c("METRO", "METROBUS", "PESERO", 
                      "VEHICULO", "TRANSPORTE PUBLICO", "TAXI")
sexo <- c("Masculino", "Fememnino")

victimas_transporte %>% select(Delito, datetime, modo, violencia, Sexo) %>% 
                        mutate(hour = hour(datetime)) %>% 
                        filter (modo %in% modos_principales) %>% 

  
  ggplot(aes(x = hour, fill = violencia)) + geom_histogram(binwidth = 2) +
  facet_grid(modo ~  Sexo) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = rel(0.6))) +
  ylab("Asaltos en el transporte") + xlab("Hora del día")


prueba <- victimas %>% filter(Delito == "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA")

# "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA"
# "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO SIN VIOLENCIA" 

# "ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA"
# "ROBO A PASAJERO A BORDO DE METROBUS SIN VIOLENCIA"

# "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA"
# "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA"

# "ROBO A PASAJERO EN ECOBUS SIN VIOLENCIA"
# "ROBO A PASAJERO EN ECOBUS CON VIOLENCIA" 

# "ROBO A PASAJERO EN TROLEBUS SIN VIOLENCIA"
# "ROBO A PASAJERO EN TROLEBUS CON VIOLENCIA"

# "ROBO A PASAJERO EN TREN LIGERO SIN VIOLENCIA"
# "ROBO A PASAJERO EN TREN LIGERO CON VIOLENCIA"

# "ROBO A PASAJERO EN TREN SUBURBANO SIN VIOLENCIA"
# "ROBO A PASAJERO EN TREN SUBURBANO CON VIOLENCIA" 

# "ROBO A PASAJERO EN RTP CON VIOLENCIA"
# "ROBO A PASAJERO EN RTP SIN VIOLENCIA"  

# "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA"
# "ROBO A TRANSEUNTE A BORDO DE TAXI PUBLICO Y PRIVADO SIN VIOLENCIA"

# "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA" 
# "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA" 
