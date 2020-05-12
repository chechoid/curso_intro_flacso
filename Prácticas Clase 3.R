#### Práctica guiada ####
library(tidyverse)
library(openxlsx)
library(scales)

individual_t117 <- read.table("Fuentes/usu_individual_t117.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE)

individual_t119 <- read.table("Fuentes/usu_individual_t119.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE)

aglomerados <- read.xlsx("Fuentes/Aglomerados EPH.xlsx")


variables_interes <- c("ANO4", "TRIMESTRE", "ESTADO", "PONDERA", "REGION", "AGLOMERADO", "PP03J","INTENSI")

base_ind_chica <- individual_t117 %>%
  select(variables_interes)

base_ind_chica_19 <- individual_t119 %>%
  select(variables_interes)

base_dos_trim <- bind_rows(base_ind_chica,base_ind_chica_19)


tasas <- base_ind_chica %>%
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J == 1]),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2, 9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa ocupados demandantes'       = Ocupados_demand/PEA,
            'Tasa Subocupación'               = Subocupados/PEA,
            'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupación no demandante' = Suboc_no_demand/PEA) %>% 
  select(-(1:8)) %>% 
  pivot_longer(cols = 1:7, names_to = "Tasas", values_to = "Valor_t117")
tasas


tasas_aglom <- base_ind_chica %>%
  group_by(AGLOMERADO) %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            activa = ocupados + desocupados,
            tasa_actividad = percent(activa / poblacion),
            tasa_ocupados = percent(ocupados / activa),
            tasa_desocupados= percent(desocupados / activa))
tasas_aglom


tasas_aglom_nom <- tasas_aglom %>%
  left_join(aglomerados, by = "AGLOMERADO") %>%
  select(Nom_Aglo, tasa_actividad, tasa_ocupados, tasa_desocupados)
tasas_aglom_nom

tasas_aglom_comparada <- base_dos_trim %>%
  group_by(ANO4, TRIMESTRE, AGLOMERADO) %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            activa = ocupados + desocupados,
            tasa_actividad = percent(activa / poblacion),
            tasa_ocupados = percent(ocupados / activa),
            tasa_desocupados= percent(desocupados / activa))
tasas_aglom_comparada


#### Práctica independiente ####
library(tidyverse)
library(openxlsx)
library(scales)

# Levantar todas las tablas individuales

variables_interes <- c("ANO4", "TRIMESTRE", "ESTADO", "AGLOMERADO", "PONDERA" ,"PONDIH", "CH04", "CH06", "CAT_OCUP", "PP08D1")
variables_interes2 <- c("ANO4", "TRIMESTRE", "ESTADO", "AGLOMERADO", "PONDERA" ,"PONDIH", "CH04", "CH06", "CAT_OCUP", "P21")

individual_t216 <- read.table("Fuentes/usu_individual_t216.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE) 


individual_t316 <- read.table("Fuentes/usu_individual_t316.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE) 

individual_t416 <- read.table("Fuentes/usu_individual_t416.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE) 

individual_t117 <- read.table("Fuentes/usu_individual_t117.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE)

individual_t119 <- read.table("Fuentes/usu_individual_t119.txt",
                              sep = ";",
                              dec = ",",
                              header = TRUE,
                              fill = TRUE)

# Variables filtradas
ind_chica_t216 <- individual_t216 %>% select(variables_interes)
ind_chica_t316 <- individual_t316 %>% select(variables_interes)
ind_chica_t416 <- individual_t416 %>% select(variables_interes)
ind_chica_t117 <- individual_t117 %>% select(variables_interes)
ind_chica_t119 <- individual_t119 %>% select(variables_interes)

#Variables filtradas para ejercicios ponderados
ind_chica_t216_2 <- individual_t216 %>% select(variables_interes2)
ind_chica_t316_2 <- individual_t316 %>% select(variables_interes2)
ind_chica_t416_2 <- individual_t416 %>% select(variables_interes2)
ind_chica_t117_2 <- individual_t117 %>% select(variables_interes2)
ind_chica_t119_2 <- individual_t119 %>% select(variables_interes2)

trimestres <- bind_rows(ind_chica_t216, ind_chica_t316, ind_chica_t416, ind_chica_t117, ind_chica_t119)

trim_pond <- bind_rows(ind_chica_t216_2,ind_chica_t316_2, ind_chica_t416_2, ind_chica_t117_2, ind_chica_t119_2) %>%
  rename(Sexo = CH04,
         Edad = CH06,
         Ingreso = P21) %>%
  mutate(Sexo = factor(Sexo, levels = c("1", "2"), labels = c("Hombre", "Mujer")))

summary(trim_pond)
  
glimpse(trimestres)

##### Cálculos

trimestres_limpio <- trimestres %>%
  rename(Sexo = CH04,
         Edad = CH06,
         Ingreso = PP08D1) %>%
  mutate(Sexo = factor(Sexo, levels = c("1","2"), labels = c("Hombre", "Mujer")))

summary(trimestres_limpio)
  

ejercicio_1 <- trimestres_limpio %>%
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]))
ejercicio_1



ejercicio_2 <- trimestres_limpio %>%
  filter(Edad >= 18, Edad <= 35) %>%
  group_by(ANO4, TRIMESTRE, Sexo) %>%
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados+Desocupados,
            'Tasa Actividad' = percent(PEA/Poblacion, accuracy = 0.1),
            'Tasa Empleo' = percent(Ocupados / PEA, accuracy = 0.1),
            'Tasa Desempleo' = percent(Desocupados/PEA, accuracy = 0.1)) %>%
  select(Año = ANO4, TRIMESTRE, Sexo, 'Tasa Actividad':'Tasa Desempleo')
ejercicio_2

ejercicio_3 <- trimestres_limpio %>%
  filter(Edad %in% c(18,70),CAT_OCUP == 3, Ingreso > 0) %>%
  mutate(Rango_Etario = case_when(
    Edad %in% 18:35 ~ "18 a 35",
    Edad %in% 36:70 ~ "36 a 70")) %>%
  group_by(Sexo, Rango_Etario) %>%
  summarise(Ingreso_Promedio = mean(Ingreso)) %>%
  select(Sexo, Rango_Etario, Ingreso_Promedio)
ejercicio_3

ejercicio_3_ponderado <- trim_pond %>%
  filter(Edad %in% c(18,70),CAT_OCUP == 3, Ingreso > 0) %>%
  mutate(Rango_Etario = case_when(
    Edad %in% 18:35 ~ "18 a 35",
    Edad %in% 36:70 ~ "36 a 70")) %>%
  group_by(Sexo, Rango_Etario) %>%
  summarise(Ingreso_Promedio = weighted.mean(Ingreso, w = PONDIH)) %>%
  select(Sexo, Rango_Etario, Ingreso_Promedio)
ejercicio_3_ponderado
  


#### Guardar archivo
# write.xlsx( x = tabla_de_R, file = "archivo_tabla.xlsx",
          #  row.names = FALSE)

practica_ind_3 <- list(ejercicio_1, ejercicio_2, ejercicio_3, ejercicio_3_ponderado)
  
write.xlsx(practica_ind_3, file = "practica_independiente_3.xlsx", row.names = FALSE)  

  