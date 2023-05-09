# Land_cover_classification_with_ML
Projekt realizowany na potrzeby przedmiotu "Zaawansowane analizy przestrzenne" w R

Projekt wykonali:
Adrian Szałkowski
Filip Lewczyk

Projekt wykorzystuje uczenie maszynowe nadzorowane, na podstawie pól treningowych wykonanych manualnie w qgis.
Zostało wyznaczone 7 klas, z dwudziestoma obiektami, a także czterdziestoma obiektami, by móc porównać czy większa ilość pól, wpływa na wyniki.

Analiza wykorzystuje metodę "Random Forest", a także "cross validation w funkcji z biblioteki CAST pod nazwą "ffs" forward feature selection.

Lista bibliotek:
library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(tmap)
library(terra)
library(gridExtra)

Badany obszar to obręb Władysławowa nad Morzem Bałtyckim.
