defineModule(sim, list(
  name = "focalStatsCalculation",
  description = paste0("This module is a revised and combined version of the prepTiles and",
                       " focalCalculation modules. It is flexible to take in a list of any rasters",
                       " and was developed once we realized that the original data had changed. ", 
                       "It will consider that the raster to calculate the focal stats has the ", 
                       "'years for the calculation' depicted as the raster's values, and that ", 
                       "those years represent the year that given pixel changed from 0 to 1", 
                       "This assumption is necessary and is different from the previous assumptions",
                       "where the rasters were indeed binary and the product was composed of 3 ",
                       "separated rasters that had the same information"),
  keywords = c("focal", "big data", "rasters"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", 
                                role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.1", focalStatsCalculation = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "focalStatsCalculation.Rmd")),
  reqdPkgs = list("PredictiveEcology/reproducible@development (>= 2.0.9.9001)", "googledrive"),
  parameters = rbind(
    defineParameter("doAssertions", "logical", TRUE, NA, NA,
                    paste0("Should it do assertions? This consumes time but increases testing")),
    defineParameter("cleanScratch", "character", NULL, NA, NA,
                    paste0("If the scratch dir should be claned after each focal operation, pass",
                           "the directory here")),
    defineParameter("classesToExcludeInLCC", "numeric", c(0, 20, 31, 32, 33, 40, 50, 80, 81, 100), NA, NA,
                    paste0("The landcover classes to convert to 0 in the RTM (so the focal does not)",
                           "consider these as habitat for the birds")),
    defineParameter("getDisturbanceRasterInModule", "logical", TRUE, NA, NA,
                    paste0("Creating the disturbanceRaster inside the module to deal with ",
                           "RAM and disk space constraints. If passing another layer, set this",
                           "to FALSE")),
    defineParameter("nx", "numeric", 2, 1, NA, 
                    paste0("The number of tiles to split raster into, along horizontal axis")),
    defineParameter("ny", "numeric", 2, 1, NA, 
                    paste0("the number of tiles to split raster into, along vertical axis")),
    defineParameter("rType", "character", "INT1U", NA, NA, 
                    paste0("pixel data type for splitRaster")),
    defineParameter("buffer", "numeric", 3, 0, NA, 
                    paste0("the number of cells to buffer tiles ",
                           "during splitRaster. Measured in cells, not distance")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be ",
                           "run with caching activated ? ",
                           "This is generally intended for ",
                           "data - type modules, where ",
                           "stochasticity and time are ",
                           "not relevant")),
    defineParameter("useParallel", "logical", FALSE, NA, NA, 
                    "Should we parallelize tile processing?"),
    defineParameter("focalDistance", "list", list(100), NA, NA, 
                    paste0("A list over all the distances at which to compute focal statistics,",
                           "in units of the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)", 
                           "It is important to note that if the element in the list is a length 2",
                           "vector, the first element will be the annulus of the second")),
    defineParameter("recoverTime", "numeric", 30, NA, NA, 
                    paste0("How long are we considering a forest to be unsuitable for birds?")),
    defineParameter("resampledRes", "numeric", 30, NA, NA, 
                    paste0("Normally, the module will get this info from the main raster, but to check",
                           " if the data already exists, we need to pass the information."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "disturbanceRaster", objectClass = "RasterLayer", 
                 desc = paste0("Raster where pixels are representing the year of the disturbance.",
                               "Currently, we are using Hermosilla et al. 2019, available on",
                               paste0(
                                 "https://opendata.nfis.org/downloads/",
                                 "forest_change/CA_forest_harvest_mask",
                                 "_year_1985_2015.zip"))),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", 
                 desc = paste0("Raster that has only 1's inside the study area.",
                               "Basically the study area shapefile rasterized.",
                               "It is needed for fixing the disturbance layers if these only have ",
                               "values when the pixel has been disturbed, and NA's otherwise")),
    expectsInput(objectName = "studyArea", objectClass = "", 
                 desc = paste0("Shapefile of the study area.",
                               "It is needed only if the disturbance raster is not provided.",
                               "Used in .inputObjects but not in the simuation")),
    expectsInput(objectName = "LandCoverRaster", objectClass = "", 
                 desc = paste0("Raster where pixels are representing the type of landcover ",
                               "to be used to mask the RTM to pixels where habitat is not available"))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "focalYearList", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "focal disturbances")),
    createsOutput(objectName = "splittedRTM", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one is a tile ")),
    createsOutput(objectName = "splittedDisturbance", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one is a tile ",
                                "At this time, it hasn't yet been processed")),
    createsOutput(objectName = "splittedLCC", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one is a tile ",
                                "At this time, it hasn't yet been processed"))
  )
))

doEvent.focalStatsCalculation = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim <- scheduleEvent(sim, start(sim), "focalStatsCalculation", "checkForData")
      
    },
    checkForData = {
      # Check for the final objects. If existing, no need to waste time!
      sim$focalYearList <- checkForFinalObject(years = time(sim), 
                                               focalDistances = P(sim)$focalDistance, 
                                               resampledRes = P(sim)$resampledRes, 
                                               pathData = dataPath(sim))

      if (is.null(sim$focalYearList)) {
        sim <- scheduleEvent(sim, start(sim), "focalStatsCalculation", "tile")
        sim <- scheduleEvent(sim, start(sim), "focalStatsCalculation", "calculatingFocal")
      } else {
        cat(crayon::green("All disturbance data was found, returning."))
      }
    },
    tile = {
      browser()
      sim$splittedDisturbance <- tryCatch({
        Cache(SpaDES.tools::splitRaster,
              r = sim$disturbanceRaster,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitDisturbance_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitDisturbance_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })

      sim$splittedLCC <- tryCatch({
        Cache(SpaDES.tools::splitRaster, #cacheId:c031e0b28d3e0283
              r = sim$LandCoverRaster,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitLCC_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitLCC_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })
      
      sim$splittedRTM <- tryCatch({
        Cache(SpaDES.tools::splitRaster, #cacheId:676d2b492f669e53
              r = sim$rasterToMatch,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitRTM_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitRTM_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })
    },
    calculatingFocal = {
      sim$focalYearList[[paste0("Year", time(sim))]] <- applyFocalToTiles(listTilesDisturbance = sim$splittedDisturbance,
                                                                          listTilesLCC = sim$splittedLCC,
                                                                          listTilesRTM = sim$splittedRTM,
                                                                          pathCache = Paths$cachePath,
                                                                          focalDistance = P(sim)$focalDistance,
                                                                          recoverTime = P(sim)$recoverTime,
                                                                          pathData = dataPath(sim),
                                                                          year = time(sim),
                                                                          classesToExcludeInLCC = P(sim)$classesToExcludeInLCC,
                                                                          doAssertions = P(sim)$doAssertions,
                                                                          cleanScratch = P(sim)$cleanScratch)
      
      sim <- scheduleEvent(sim, time(sim)+1, "focalStatsCalculation", "calculatingFocal")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere("studyArea", sim)){
    sim$studyArea <- Cache(prepInputs, url = "https://drive.google.com/file/d/1of3bIlPnLMDmumerLX-thILHtAoJpeiC", 
                           targetFile = "studyArea.shp", 
                           archive = "studyArea.zip", 
                           alsoExtract = "similar", 
                           destinationPath = getOption("reproducible.inputPaths"), 
                           fun = "terra::vect",
                           userTags = c("objectName:studyArea",
                                        cacheTags,
                                        "goal:sA"),
                           omitArgs = c("destinationPath"))
    
  }
  
  if (!suppliedElsewhere("disturbanceRaster", sim)){
    if (!P(sim)$getDisturbanceRasterInModule){
      disturbanceRasterPath <- Cache(prepInputs, url = paste0(
        "https://opendata.nfis.org/downloads/",
        "forest_change/CA_forest_harvest_mask",
        "_year_1985_2015.zip"),
        studyArea = sim$studyArea,
        userTags = c("objectName:disturbanceRaster",
                     cacheTags,
                     "outFun:Cache", "goal:prepDisturbanceRas"),
        omitArgs = c("overwrite", "destinationPath"),
        targetFile = "CA_harvest_year_1985_2015.tif",
        destinationPath = getOption("reproducible.inputPaths"))
    }
  }
  
  if (!suppliedElsewhere("rasterToMatch", sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/1hSn2Rdiyou9znGRfmJlsUJ-p2fcHX6Uy", 
                               targetFile = "processedRTM.tif", 
                               archive = "processedRTM.zip", 
                               studyArea = sim$studyArea,
                               destinationPath = getOption("reproducible.inputPaths"), 
                               fun = "terra::rast",
                               userTags = c("originPoint:global", "objectName:rasterToMatch"),
                               omitArgs = "destinationPath")
  }
  return(invisible(sim))
}
