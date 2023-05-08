rm(list=ls())

library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(tmap)
library(terra)
library(gridExtra)

# Dane pobrane z EO Browser, Sentinel 2 o danych (...), Utworzono raster stack
#
main_data <- stack("data/raster_stack.grd")

# Sprawdzmy jak przedstawiaja sie dane
plot(main_data)
plotRGB(main_data, stretch ="lin", r = 3, g = 2, b = 1)

#nalezy wczytac warstwe z poligonami, ktore posluza jako warstwy treningowe

trainSites40 <- read_sf("data/trainFields2.gpkg")
print(trainSites40)

trainSites20 <- read_sf("data/trainFields.gpkg")
print(trainSites20)

viewRGB(main_data, r = 3, g = 2, b = 1, map.types = "Esri.WorldImagery")+
  mapview(trainSites40)

extr40 <- extract(main_data, trainSites40, df=TRUE)
extr20 <- extract(main_data, trainSites20, df=TRUE)
#head(extr$ID)
extr4
extr40 <- merge(extr40, trainSites40, by.x="ID", by.y="ID")
extr20 <- merge(extr20, trainSites20, by.x="ID", by.y="ID")
head(extr40)

set.seed(100)
trainids40 <- createDataPartition(extr40$ID,list=FALSE,p=0.1) #CARET
trainDat40 <- extr40[trainids40,]

trainids20 <- createDataPartition(extr20$ID,list=FALSE,p=0.1) #CARET
trainDat20 <- extr20[trainids20,]
# metoda cv czyli cross validacja, sluzy do lepszego przewidywania zmian
# zamiast na wszystkich zmiennych, model uczy się jedynie na polu treningowym
# Zapewnia większą pewność, by model lepiej pracował
predictors <- names(main_data)
response <- "typ"
indices40 <- CreateSpacetimeFolds(trainDat40,spacevar = "ID",k=3,class = "typ") #CAST
ctrl <- trainControl(method="cv", 
                     index = indices40$index,
                     savePredictions = TRUE)

indices20 <- CreateSpacetimeFolds(trainDat20,spacevar = "ID",k=3,class="typ") #CAST
ctrl <- trainControl(method="cv", 
                     index = indices20$index,
                     savePredictions = TRUE)
#View(extr)

# train the model
model40 <- train(trainDat40[,predictors],
               trainDat40[,response],
               method="rf",
               ntree=75)
model40
print(model40)
plot(model40)
plot(varImp(model40))

model20 <- train(trainDat20[,predictors],
                 trainDat20[,response],
                 method="rf",
                 ntree=75)
model20
print(model20)
plot(model20)
plot(varImp(model20))

model40 <-  train(trainDat40[,predictors],trainDat40[,response])

set.seed(100)
model40 <- ffs(trainDat40[,predictors],
             trainDat40[,response],
             method="rf",
             metric="Kappa",
             trControl=ctrl,
             importance=TRUE,
             ntree=75)

print(model40)
plot(model40)
plot(varImp(model40))

model20 <-  train(trainDat20[,predictors],trainDat20[,response])

set.seed(100)
model20 <- ffs(trainDat20[,predictors],
               trainDat20[,response],
               method="rf",
               metric="Kappa",
               trControl=ctrl,
               importance=TRUE,
               ntree=75)

print(model20)
plot(model20)
plot(varImp(model20))

# proba przewidzenia
cvPredictions40 <- model40$pred[model40$pred$mtry==model40$bestTune$mtry,]
table(cvPredictions40$pred,cvPredictions40$obs)
# 
cvPredictions20 <- model20$pred[model20$pred$mtry==model20$bestTune$mtry,]
table(cvPredictions20$pred,cvPredictions20$obs)

prediction40 <- predict(main_data,model40)
prediction20 <- predict(main_data,model20)
cols <- rev(c("palegreen", "grey", "blue", "forestgreen", "brown","beige","yellowgreen"))
AOA <- aoa(main_data,model40)
tm_shape(prediction40) +
  tm_raster(palette = cols,title = "Legenda")+
  tm_scale_bar(bg.color="white",bg.alpha=0.75)+
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 0.75)

tm_shape(prediction20) +
  tm_raster(palette = cols,title = "Legenda")+
  tm_scale_bar(bg.color="white",bg.alpha=0.75)+
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 0.75)

plot(AOA)
plot(AOA$AOA)
plot(AOA$DI)
plot(AOA$parameters)

predplot40 <- spplot(deratify(prediction40),col.regions=cols, main = list(label="Prediction 40",cex=0.8))
predplot20 <- spplot(deratify(prediction20),col.regions=cols, main = list(label="Prediction 20",cex=0.8))

predplotaoa <- spplot(deratify(prediction40),col.regions=cols)+
spplot(AOA$AOA,col.regions=c("grey", "transparent"))

plot(predplot)
plot(predplotaoa)

grid.arrange(predplot40,predplot20)
