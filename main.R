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

trainSites <- read_sf("data/trainFields2.gpkg")
print(trainSites)

viewRGB(main_data, r = 3, g = 2, b = 1, map.types = "Esri.WorldImagery")+
  mapview(trainSites)

extr <- extract(main_data, trainSites, df=TRUE)

#head(extr$ID)

extr <- merge(extr, trainSites, by.x="ID", by.y="ID")
head(extr)

set.seed(100)
trainids <- createDataPartition(extr$ID,list=FALSE,p=0.05)
trainDat <- extr[trainids,]

predictors <- names(main_data)
response <- "typ"
indices <- CreateSpacetimeFolds(trainDat,spacevar = "ID",k=3,class="typ")
ctrl <- trainControl(method="cv", 
                     index = indices$index,
                     savePredictions = TRUE)
#View(extr)

# train the model
model <- train(trainDat[,predictors],
               trainDat[,response],
               method="rf",
               ntree=75)
model

model <-  train(trainDat[,predictors],trainDat[,response])

set.seed(100)
model <- ffs(trainDat[,predictors],
             trainDat[,response],
             method="rf",
             metric="Kappa",
             trControl=ctrl,
             importance=TRUE,
             ntree=75)

print(model)
plot(varImp(model))

cvPredictions <- model$pred[model$pred$mtry==model$bestTune$mtry,]
table(cvPredictions$pred,cvPredictions$obs)

prediction <- predict(main_data,model)
cols <- rev(c("palegreen", "grey", "blue", "forestgreen", "brown","beige","yellowgreen"))
AOA <- aoa(main_data,model)
tm_shape(prediction) +
  tm_raster(palette = cols,title = "LUC")+
  tm_scale_bar(bg.color="white",bg.alpha=0.75)+
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 0.75)

AOA <- aoa(main_data,model)
plot(AOA$AOA)
plot(AOA$DI)
plot(AOA$parameters)

predplot <- spplot(deratify(cvPredictions),col.regions=cols, main = list(label="Prediction (left), prediction only for the AOA (right) and RGB composite (bottom)",cex=0.8))
