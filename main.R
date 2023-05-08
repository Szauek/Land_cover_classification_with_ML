<<<<<<< Updated upstream
rm(list=ls())
=======
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

main_data2 <- stack("data/ryki_grd.grd")


#usunac
rgbplot_main_data <- spplot(main_data[[1]],  col.regions="transparent",
                     sp.layout =rgb2spLayout(sen_ms[[3:1]], quantiles = c(0.02, 0.98), alpha = 1))
??rgb2spLayout

# Sprawdzmy jak przedstawiaja sie dane
plot(main_data)
plotRGB(main_data, stretch ="lin", r = 3, g = 2, b = 1)

plot(main_data2)
plotRGB(main_data2, stretch ="lin", r = 3, g = 2, b = 1)

#nalezy wczytac warstwe z poligonami, ktore posluza jako warstwy treningowe

trainSites <- read_sf("data/trainFields2.gpkg")
print(trainSites)

trainSites2 <- read_sf("data/trainField_ryki2.gpkg")
print(trainSites2)

viewRGB(main_data, r = 3, g = 2, b = 1, map.types = "Esri.WorldImagery")+
  mapview(trainSites)

viewRGB(main_data2, r = 3, g = 2, b = 1, map.types = "Esri.WorldImagery")+
  mapview(trainSites2)

extr <- extract(main_data, trainSites, df=TRUE)

extr2 <- extract(main_data2, trainSites2, df=TRUE)
#head(extr$ID)

extr <- merge(extr, trainSites, by.x="ID", by.y="ID")
head(extr)

extr2 <- merge(extr2, trainSites2, by.x="ID", by.y="ID")
head(extr2)

set.seed(100)
trainids <- createDataPartition(extr$ID,list=FALSE,p=0.05)
trainDat <- extr[trainids,]

set.seed(100)
trainids2 <- createDataPartition(extr2$ID,list=FALSE,p=0.05)
trainDat2 <- extr2[trainids2,]

predictors <- names(main_data)
response <- "typ"
indices <- CreateSpacetimeFolds(trainDat,spacevar = "ID",k=3,class="typ")
ctrl <- trainControl(method="cv", 
                     index = indices$index,
                     savePredictions = TRUE)
#View(extr)

predictors <- names(main_data2)
response <- "typ"
indices <- CreateSpacetimeFolds(trainDat2,spacevar = "ID",k=3,class="typ")
ctrl <- trainControl(method="cv", 
                     index = indices$index,
                     savePredictions = TRUE)

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


model2 <- train(trainDat2[,predictors],
               trainDat2[,response],
               method="rf",
               ntree=75)
model2

model2 <-  train(trainDat2[,predictors],trainDat2[,response])

set.seed(100)
model2 <- ffs(trainDat2[,predictors],
             trainDat2[,response],
             method="rf",
             metric="Kappa",
             trControl=ctrl,
             importance=TRUE,
             ntree=75)

print(model2)
plot(varImp(model2))

cvPredictions <- model$pred[model$pred$mtry==model$bestTune$mtry,]
table(cvPredictions$pred,cvPredictions$obs)

cvPredictions2 <- model2$pred[model2$pred$mtry==model2$bestTune$mtry,]
table(cvPredictions2$pred,cvPredictions2$obs)

prediction <- predict(main_data,model)
cols <- rev(c("palegreen", "grey", "blue", "forestgreen", "brown","beige","yellowgreen"))
AOA <- aoa(main_data,model)
tm_shape(prediction) +
  tm_raster(palette = cols,title = "LUC")+
  tm_scale_bar(bg.color="white",bg.alpha=0.75)+
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 0.75)

prediction2 <- predict(main_data2,model2)
cols <- rev(c("palegreen", "grey", "blue", "forestgreen", "brown","beige","yellowgreen"))
AOA <- aoa(main_data2,model2)
tm_shape(prediction2) +
  tm_raster(palette = cols,title = "LUC")+
  tm_scale_bar(bg.color="white",bg.alpha=0.75)+
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 0.75)

AOA <- aoa(main_data,model)
plot(AOA$AOA)
plot(AOA$DI)
plot(AOA$parameters)

predplot <- spplot(deratify(cvPredictions),col.regions=cols, main = list(label="Prediction (left), prediction only for the AOA (right) and RGB composite (bottom)",cex=0.8))
>>>>>>> Stashed changes
