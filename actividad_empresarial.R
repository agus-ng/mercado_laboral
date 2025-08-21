# 1. Preámbulo ------------------------------------------------------------

# Paquetes
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)

# Rutas
dir_principal <- "C:/Users/rdzgu/OneDrive/Escritorio/Proyectos R/mercado_laboral/"
ruta_datos    <- paste0(dir_principal, "rama.xlsx") 
ruta_ciiu     <- paste0(dir_principal, "ciiu4-cl-2012.xls")

# 2. Importación datos ----------------------------------------------------

# Mantenemos hojas relevantes
hojas_datos <- excel_sheets(ruta_datos)
hojas_datos <- hojas_datos[hojas_datos != "índice"]

# Importamos 
datos_completos <- map(.x = hojas_datos,
                       .f = \(x) read_excel(ruta_datos, sheet = x, 
                                            range = "A6:AV156",
                                            guess_max = 1000))

names(datos_completos) <- hojas_datos

# Importación CIIU

ciiu <- read_excel(ruta_ciiu, skip = 1, guess_max = 10000) %>% 
  filter(is.na(División)) %>% 
  clean_names() %>% 
  select(seccion, glosa) %>% 
  mutate(glosa = str_replace(glosa, "; ", ";\n"),
         glosa = str_replace(glosa, "bienes y servicios para ",
                             "bienes y servicios para\n"))

# Limpieza datos ----------------------------------------------------------

#Eliminar filas sin información
datos_completos <- map(.x = datos_completos,
                       .f = \(x) filter(.data = x, !is.na(Año)))

#Renombraremos columnas en función de la actividad económica - sección CIIU
nombres_columnas_dataset <- c("agno", "trimestre", "nota_total", "total")

for (i in LETTERS) {
  vector_auxiliar <- c(paste0("nota_", i), paste0("actividad_", i))
  
  nombres_columnas_dataset <- c(nombres_columnas_dataset, vector_auxiliar)
  rm(vector_auxiliar)
}

nombres_columnas_dataset <- nombres_columnas_dataset[grepl("[^V|W|X|Y|Z]$", nombres_columnas_dataset)]
nombres_columnas_dataset <- c(nombres_columnas_dataset, "nota_NS_NR", "NS_NR")

datos_completos <- map(.x = datos_completos,
                       .f = \(x) setNames(x, nombres_columnas_dataset))

datos_completos <- datos_completos %>% 
  map(.f = \(x) group_by(.data = x, agno) %>% 
        mutate(id_trimestre = row_number()) %>%
        ungroup() %>% 
        mutate(id = row_number(),
               id_trimestre = if_else(agno == 2013, id_trimestre + 1, id_trimestre),
               id_grafico = str_c(agno, " - ", id_trimestre),
               across(starts_with("actividad"), .fns = as.numeric)) %>% 
        relocate(id_trimestre, id_grafico, .after = trimestre))

nivel_nacional <- datos_completos[["AS"]]

nivel_nacional <- nivel_nacional %>% 
  mutate(auxiliar_año1 = if_else(id == 1 | id %% 12 == 0, id, NA),
         auxiliar_año2 = if_else(id == 1 | id %% 12 == 0, agno, NA))


# Graficar ----------------------------------------------------------------

for (act in grepv("actividad", colnames(nivel_nacional))){
  
  auxiliar_actividad <- ciiu[grepl(str_sub(act, start = -1), ciiu[["seccion"]]), "glosa"]
  
  grafico <- nivel_nacional %>% 
    ggplot(mapping = aes(x = id, y = get(act), group = 1)) +
    geom_line(color = "darkblue") + 
    geom_point(color = "darkblue", size = 0.8) +
    ggtitle(auxiliar_actividad) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_vline(aes(xintercept = auxiliar_año1),
               linetype = "dashed",
               color = "gray") +
    ylab("Población ocupada (miles)") +
    xlab("Año") +
    labs(caption ="Datos corresponden a trimestres móviles.") +
    scale_x_continuous(breaks = nivel_nacional$auxiliar_año1,
                       labels = nivel_nacional$auxiliar_año2) +
    ggthemes::theme_few() +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dotted", size = 0.7),
          plot.title = element_text(size = 13))
  
  print(grafico)
  ggsave(filename = paste0(act, ".png"),
         path = paste0(dir_principal, "/graficos/"),
         dpi = "retina")
  rm(grafico)
}

# Generar tabla con promedios --------------------------------------------

# Pasar datos relevantes a formato long
tabla_promedios <- nivel_nacional %>% 
  mutate(periodo = case_when(
    agno <= 2019 ~ 1,
    agno == 2020 ~ 2,
    agno >= 2021 ~ 3,
    .default = NA)
  ) %>% 
  filter(periodo != 2) %>% 
  select(-starts_with("nota"), -c(total, id_grafico, NS_NR:auxiliar_año2)) %>% 
  pivot_longer(cols = starts_with("actividad"), names_to = "actividad", values_to = "ocupados") %>% 
  mutate(actividad = str_sub(actividad, start = -1, end = -1))

# Calcular promedios
tabla_promedios <- tabla_promedios %>% 
  group_by(actividad, periodo) %>% 
  summarise(promedio = mean(ocupados)) %>% 
  mutate(crecimiento = (promedio - lag(promedio))/(lag(promedio))) %>% 
  ungroup() %>% 
  mutate(crecimiento = round(crecimiento * 100, 2),
         periodo = case_when(
           periodo == 1 ~ "2013-2019",
           periodo == 3 ~ "2021-2025"
         ))

# Exportar como markdown
options(knitr.kable.NA = "")
knitr::kable(tabla_promedios,
             digits = 2, 
             col.names = c("Actividad económica",
                           "Período",
                           "Población ocupada promedio (miles)",
                           "Tasa de crecimiento (%)"))
