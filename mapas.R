library(tidyverse)
library(sf)
library(httr2)
library(ows4R)
library(rmapshaper)
library(janitor)
library(scales)
library(santoku)

accentless <- function( s ) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
    s );
}




## consultas los mapas que el ING tiene disponibles
url_wfs <- "https://geos.snitcr.go.cr/be/IGN_5_CO/wfs?"

bwk_client <- WFSClient$new(
  url = url_wfs,
  serviceVersion = "2.0.0"
)

as_tibble(bwk_client$getFeatureTypes(pretty = TRUE))




# descargar mapa de cantones
url <- url_parse(url_wfs)
url$query <- list(
  service = "wfs",
  request = "GetFeature",
  typename = "IGN_5_CO:limitecantonal_5k",
  srsName = "EPSG:4326"
)
request <- url_build(url) 



regiones <- read_sf(request) |> 
  select(
    "CÓDIGO_DE_PROVINCIA",
    "PROVINCIA","CÓDIGO_CANTÓN","CANTÓN"
  )
st_crs(regiones) <- st_crs(4326)


#
ggplot(
  data = regiones
) +
  geom_sf()




#simplificar el mapa
regiones_simplificadas <- ms_simplify(
  input = regiones,
  keep = 0.0001,
  keep_shapes = FALSE
)

ggplot(
  data = regiones_simplificadas
) +
  geom_sf()

# eliminar la isla del coco
regiones_simplificadas <- ms_filter_islands(
  input = regiones_simplificadas,
  min_area = 24000000
)

write_sf(
  obj = regiones_simplificadas, dsn = "cantones_cr.geojson")





## cargar y visualizar el mapa
cantones <- st_read("cantones_cr.geojson", quiet = TRUE)


ggplot(
  data = cantones
) +
  geom_sf()


datos <- read_sf("data/cantones_ips_cr.geojson")


ggplot(
  data = datos,
  mapping = aes(
    fill = tasa_de_homicidios
  )
) +
  geom_sf(
    color = "white"
  ) +
  scale_fill_viridis_c() +
  labs(
    title = "Tasa de homicidios por cantón",
    fill = "tasa de homicidios"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.justification = "left",
    legend.key.width = unit(1.2,"cm")
  )



datos <- datos |> 
  mutate(
    grupos_homicidios = chop_quantiles(tasa_de_homicidios, c(Q1 = 0, Q2 = 0.25, Q3 = 0.5, Q4 = 0.75))
  )


ggplot(
  data = datos,
  mapping = aes(
    fill = grupos_homicidios
  )
) +
  geom_sf(
    color = "#E4E0E1"
  ) + 
  scale_fill_viridis_d(
    na.value = "#E4E0E1"
  ) + 
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.justification = "left",
    legend.key.width = unit(1.2,"cm")
  )



GAM <- c("San José", "Escazú", "Desamparados", "Aserrí",
         "Mora", "Goicoechea", "Santa Ana", "Alajuelita",
         "Vázquez de Coronado", "Tibás", "Moravia",
         "Montes de Oca", "Curridabat", "Alajuela",
         "Atenas", "Poás", "Cartago", "Paraíso",
         "La Unión", "Oreamuno", "Alvarado", "El Guarco",
         "Heredia", "Barva", "Santo Domingo",
         "Santa Bárbara", "San Rafael", "San Isidro",
         "Belén", "Flores", "San Pablo")

GAM <- str_to_upper(GAM)
GAM <- accentless(GAM)


datos2 <- filter(
  datos, 
  canton %in% GAM
)

ggplot(
  data = datos2,
  mapping = aes(
    fill = grupos_homicidios
  )
) +
  geom_sf(
    color = "#E4E0E1"
  ) + 
  scale_fill_viridis_d(
    na.value = "#E4E0E1"
  ) + 
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.justification = "left",
    legend.key.width = unit(1.2,"cm")
  )




