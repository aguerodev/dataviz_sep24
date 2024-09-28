library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggokabeito)

# fechas en español 
# windows
#Sys.setlocale("LC_TIME", "Spanish_Spain.1252")
# mac/linux
Sys.setlocale("LC_TIME", "es_ES.UTF-8")


datos <- data.frame(
  tienda = sample(c("A","B","C","D"), 1000, replace = TRUE),
  ventas = sample(c(1000,5000,10000,12000,20000), 1000, replace = TRUE),
  fecha = sample(ymd("2020-01-01") + 0:(365*5), 1000, replace = TRUE)
)

## ventas por mes por tienda

df <- datos |> 
  mutate(
    fecha = floor_date(fecha, "month")
  ) |> 
  summarise(
    ventas = sum(ventas),
    .by = c(tienda, fecha)
  ) |> 
  arrange(tienda, fecha)



## base
ggplot(
  data = df,
  mapping = aes(
    x = fecha,
    y = ventas,
    color = tienda
  )
) +
  geom_line()


# v2
ggplot(
data = df,
mapping = aes(
  x = fecha,
  y = ventas,
  color = tienda
)
) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = comma_format(prefix = "₡")
  )

# v3

colones <- function(texto){
  function(texto){
    texto <- comma(texto)
    paste0("₡", texto)
  }
}

# formato fechas
# https://www.stat.berkeley.edu/~s133/dates.html
ggplot(
  data = df,
  mapping = aes(
    x = fecha,
    y = ventas,
    color = tienda
  )
) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = colones()
  ) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%B \n %Y"
  )

#v4
ggplot(
  data = df,
  mapping = aes(
    x = fecha,
    y = ventas,
    color = tienda
  )
) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = colones()
  ) +
  scale_x_date(
    date_breaks = "4 months",
    labels = label_date_short()
  ) +
  scale_color_manual(
    values = c(
      "A" = "#73EC8B",
      "B" = "#FF6500",
      "C" = "#3A6D8C",
      "D" = "#FCDE70"
    )
  )

#v5

#v4
# RColorBrewer::display.brewer.all()
ggplot(
  data = df,
  mapping = aes(
    x = fecha,
    y = ventas,
    color = tienda
  )
) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = colones()
  ) +
  scale_x_date(
    date_breaks = "4 months",
    labels = label_date_short()
  ) +
  scale_color_okabe_ito()


#v5
ggplot(
  data = df,
  mapping = aes(
    x = fecha,
    y = ventas,
    color = tienda
  )
) +
  annotate(
    geom = "rect",
    xmin = ymd("2020-03-01"),
    xmax = ymd("2020-06-01"),
    ymin = 0,
    ymax = 120000,
    fill = "#D2FF72",
    alpha = 0.3
  ) +
  annotate(
    geom = "text",
    x = ymd("2020-04-01"),
    y = 125000,
    color = "white",
    alpha = 0.3,
    label = "Pandemia Covid",
    hjust = 0 
  ) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = colones()
  ) +
  labs(
    title = "Ventas mensuales por tienda",
    x = "",
    subtitle = "Para las tiendas de ropa de Cartago, Heredia y San José",
    caption = "Fuente: Sistema de ventas central"
  ) +
  scale_x_date(
    date_breaks = "4 months",
    labels = label_date_short()
  ) +
  scale_color_okabe_ito() +
  theme(
    plot.background = element_rect(
      fill = "#1E201E"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#31363F"
    ),
    axis.text = element_text(
      color = "white"
    ),
    legend.text = element_text(
      color = "white"
    ),
    legend.title = element_text(
      color = "white"
    ),
    axis.title = element_text(
      color = "white"
    ),
    plot.title = element_text(
      color = "white",
      family = "Arial Black",
      size = 18,
      margin = margin(
        t = 8,
        b = -5
      )
    ),
    plot.subtitle = element_text(
      color = "white",
      family = "Arial",
      size = 14,
      margin = margin(
        t = 10,
        b = 12
      )
    ),
    plot.margin = margin(
      10,10,10,10
    ),
    plot.caption = element_text(
      color = "white",
      hjust = 0,
      family = "Arial"
    )
  )

























