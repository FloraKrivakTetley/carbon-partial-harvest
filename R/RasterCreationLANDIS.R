## FCI Partial Harvesting Project
## LANDIS-II model preparation
## Create raster with blocks of cells that have the same values
## Can be done in raster package, or with the newer terra

library(terra)

## Example: Create landscape with the right number of cells to end up with a 102x102 landscape that has stands
## with 6x6=36 pixels. Cell values: RoundUp(100/6)=17. 17x17=289.

landscape<- rast(ncol=17, nrow=17, vals=c(1:289))
plot(landscape, main="A raster with 289 cells")

landscape2<- disagg(landscape, 6)  ## disaggregates cells in both dimensions, by a factor of 6
ext(landscape2)<- c(0, 102, 0, 102)  ## set extent so resolution will be consistent
landscape2


dim(landscape2)
plot(landscape2)


## Check to see whether values are assigned to cells as expected

v<- values(landscape2)
v[1:10]

## Write files
writeRaster(landscape2,"eco.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape2,"init.img", overwrite=TRUE, datatype='INT2S')  #write initial conditions file (1 condition)


## If you need to crop this first

cropbox<-c(0, 100, 0, 100)
## cropbox<- c(0, 100, 2, 102)
cropped_landscape<- crop(landscape2, cropbox)

## Check that the expected values were cut off
v2<- values(cropped_landscape)
v2[1:102]

## Yes, you can tell the right hand edge was trimmed off because you only have four cells with a value of 17
## then you start back at 1 as the values start reading from the left hand edge again.


## FKT Raster creation for simple calibration landscape

library(terra)

landscape<- rast(ncol=10, nrow=10, vals=c(1:1))
plot(landscape, main="A raster with 100 cells")
ext(landscape)<- c(-5,5,-5,5)

writeRaster(landscape,"eco.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape,"init.img", overwrite=TRUE, datatype='INT2S')

## Raster creation for stand file, with stands numbered 1-100
landscape<- rast(ncol=10, nrow=10, vals=c(1:100))
plot(landscape, main="A raster with 100 cells")
ext(landscape)<- c(-5,5,-5,5)

writeRaster(landscape,"stand.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)

## Smallest, simplest landscape (1 cell)

landscape<- rast(ncol=1, nrow=1, vals=c(1))
plot(landscape, main="A raster with 1 cell")
ext(landscape)<- c(0,1,0,1)

writeRaster(landscape,"eco.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape,"init.img", overwrite=TRUE, datatype='INT2S')

## Creating an intermediate landscape (3*3 cells=9 cells) to use for attempted replication of Date Creek Results

landscape<- rast(ncol=3, nrow=3, vals=c(1:1))
plot(landscape, main="A raster with 9 cells")
ext(landscape)<- c(0,3,0,3)

writeRaster(landscape,"eco.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape,"init.img", overwrite=TRUE, datatype='INT2S')
writeRaster(landscape,"management.img", overwrite=TRUE, datatype='INT2S') 

landscape<- rast(ncol=3, nrow=3, vals=c(1:9))
writeRaster(landscape,"stand.img", overwrite=TRUE, datatype='INT2S') # write stand file w/ different numbered stands

## Scenario Landscape. 20x30 stands, with 9 cells/sites per stand. 
## Stand file: Numbered 1-300
## Management area file: Numbered 1 in all cells
## Ecoregion file: Numbered 1 in all cells
## Init file: Also have all cells numbered with 1 since we are using 1 species composition throughout

landscape_simple<- rast(ncol=20, nrow=30, vals=c(1))
plot(landscape_simple, main="A raster with 600 cells with value 1")
ext(landscape_simple)<- c(0,20,0,30)
landscape_simple<-disagg(landscape_simple,3)
ext(landscape_simple)<- c(0,60,0,90)
landscape_simple

writeRaster(landscape_simple,"ecoScenario.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape_simple,"initScenario.img", overwrite=TRUE, datatype='INT2S')
writeRaster(landscape_simple,"managementScenario.img", overwrite=TRUE, datatype='INT2S') 

landscape<- rast(ncol=20, nrow=30, vals=c(1:600))
plot(landscape, main="A raster with 600 cells")
ext(landscape)<- c(0,20,0,30)

landscape2<- disagg(landscape,3)  ## disaggregates cells in both dimensions, by a factor of 6
plot(landscape2, main="A raster with 9*600 cells")
ext(landscape2)<- c(0,60,0,90)  ## set extent so resolution will be consistent
landscape2

writeRaster(landscape2,"standScenario.img", overwrite=TRUE, datatype='INT2S')

## PHASE 1 FINAL Scenario Landscape. 50 x 50 stands (Total 2,500 stands), 16 ha each
## Total Landscape Size: 40,000 ha
## Stand file: Numbered 1-2500
## Management area file: Numbered 1 in all cells
## Ecoregion file: Numbered 1 in all cells
## Init file: Also have all cells numbered with 1 since we are using 1 species composition throughout
library(terra)

landscape_simple<- rast(ncol=50, nrow=50, vals=c(1))
plot(landscape_simple, main="A raster with 2500 cells with value 1")
ext(landscape_simple)<- c(0,50,0,50)
landscape_simple<-disagg(landscape_simple,4)
ext(landscape_simple)<- c(0,200,0,200)
landscape_simple

writeRaster(landscape_simple,"ecoScenario.img", overwrite=TRUE, datatype='INT2S')  #write ecoregion file (1 region)
writeRaster(landscape_simple,"initScenario.img", overwrite=TRUE, datatype='INT2S')
writeRaster(landscape_simple,"managementScenario.img", overwrite=TRUE, datatype='INT2S') 

## creating vectors for roads extension

# existing roads raster
roads_vec1<- paste(rep(c(0,9,0), times=c(100,1,99)))
roads_vec2<- paste(rep(c(0,8,9,0), times=c(99,1,1,99)))
roads_vec3<- rep(roads_vec1, times=199)
roads_vecFINAL<- paste(c(roads_vec3,roads_vec2))
roads_vecFINAL<- as.integer(roads_vecFINAL)
landscape_roads<- rast(ncol=200, nrow=200, vals=c(roads_vecFINAL))
ext(landscape_roads)<- c(0,200,0,200)
plot(landscape_roads)
landscape_roads

writeRaster(landscape_roads, "roads2.img", overwrite=TRUE, datatype='INT2S')

# More complex existing roads raster:
roads_vec1<- paste(rep(c(0,9,0), times=c(100,1,99))) # this is a vector for a row w/ just road in the middle
roads_vec2<- paste(rep(c(0,8,9,0), times=c(99,1,1,99))) ## this is the vector for the final row

roads_vecLEFT<- paste(rep(c(0,1,9,0), times=c(20,80,1,99)))
roads_vecRIGHT<- paste(rep(c(0,9,1,0), times=c(100,1,79,20)))

roads_vecCENTER<- rep(roads_vec1, times=19)

roads_vecCOMPLEX<- paste(c(roads_vecCENTER, roads_vec1, roads_vecLEFT, roads_vecCENTER, roads_vecCENTER, roads_vecRIGHT, roads_vecCENTER,
                         roads_vecLEFT, roads_vecCENTER,roads_vecCENTER, roads_vecRIGHT, roads_vec1, roads_vecLEFT, roads_vecCENTER,
                         roads_vecLEFT, roads_vecCENTER, roads_vecCENTER, roads_vecRIGHT, roads_vecCENTER, roads_vec2))

roads_vecCOMPLEX<- as.integer(roads_vecCOMPLEX)

landscape_roads<- rast(ncol=200, nrow=200, vals=c(roads_vecCOMPLEX))
ext(landscape_roads)<- c(0,200,0,200)
plot(landscape_roads)
landscape_roads

writeRaster(landscape_roads, "roadsCOMPLEX.img", overwrite=TRUE, datatype='INT2S')


# buildable zones raster: in this raster every cell is a buildable zone

buildable_zones<- rast(ncol=200, nrow=200, vals=c(1))
ext(buildable_zones)<- c(0,200,0,200)
plot(buildable_zones)
buildable_zones

writeRaster(buildable_zones, "zones.img", overwrite=TRUE, datatype='INT2S')

landscape<- rast(ncol=50, nrow=50, vals=c(1:2500))
plot(landscape, main="A raster with 2500 cells")

landscape2<- disagg(landscape,4)  ## disaggregates cells in both dimensions, expands each stand by a factor of 
plot(landscape2, main="A raster with 16*2500 cells")
ext(landscape2)<- c(0,200,0,200)  ## set extent so resolution will be consistent
landscape2

writeRaster(landscape2,"standScenario.img", overwrite=TRUE, datatype='INT2S')

## To create landscape with two stand types. init file with Map Codes 1 and 2
library(terra)

landscape_simple<- rast(ncol=50, nrow=50, vals=c(1,1,1,1,1,2,2,2,2,2))
plot(landscape_simple, main="A raster with 2500 cells with value 1")
ext(landscape_simple)<- c(0,50,0,50)
landscape_simple<-disagg(landscape_simple,4)
ext(landscape_simple)<- c(0,200,0,200)
landscape_simple
writeRaster(landscape_simple,"initComplex.img", overwrite=TRUE, datatype='INT2S')
