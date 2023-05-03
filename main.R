rm(list=ls())

library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(tmap)
library(terra)

# Dane pobrane z EO Browser, Sentinel 2 o danych (...), Utworzono raster stack
#
main_data <- stack("data/raster_stack.grd")


#usunac
rgbplot_main_data <- spplot(main_data[[1]],  col.regions="transparent",
                     sp.layout =rgb2spLayout(sen_ms[[3:1]], quantiles = c(0.02, 0.98), alpha = 1))
??rgb2spLayout

# Sprawdzmy jak przedstawiaja sie dane
plot(main_data)
plotRGB(main_data, stretch ="lin", r = 3, g = 2, b = 1)

#nalezy wczytac warstwe z poligonami, ktore posluza jako warstwy treningowe

