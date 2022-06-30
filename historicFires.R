defineModule(sim, list(
  name = "historicFires",
  description = paste("Use static fire layers (e.g., from historic data or pregenerated simulated fires).",
                      "Default behaviour is to use historic fire perimeters unless the user provides their own."),
  keywords = c("wildfire"),
  authors = c(
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Christopher", "Mallon", email = "chris.mallon@gov.ab.ca", role = "aut"),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(historicFires = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "historicFires.Rmd"), ## same file
  reqdPkgs = list("fasterize", "raster", "reproducible", "sf",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.10.9008)"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("staticFireYears", "integer", 2011:2020, NA, NA,
                    "simulation years for which static fire maps will be used.")
  ),
  inputObjects = bindrows(
    expectsInput("fireMaps", "RasterStack",
                 desc = paste("Annual layers of fire perimeters (e.g., historic or presimulated).",
                              "Layer names correspond to simulation years for which that layer will be used."),
                 sourceURL = "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"),
    expectsInput("flammableRTM", "RasterLayer",
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1.",
                 sourceURL = NA),
    expectsInput("studyArea", "SpatialPolygonsDataFrame",
                 desc = paste("Polygon to use as the study area. Must be provided by the user."),
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("burnDT", "data.table", desc = "data.table with pixel IDs of most recent burn."),
    createsOutput("burnMap", "RasterLayer", desc = "A raster of cumulative burns"),
    createsOutput("burnSummary", "data.table", desc = "Describes details of all burned pixels."),
    createsOutput("fireMaps", "RasterStack",
                  desc = paste("Annual layers of fire perimeters (e.g., historic or presimulated).",
                               "Layer names correspond to simulation years for which that layer will be used.")),
    createsOutput("rstAnnualBurnID", "RasterLayer", desc = "annual raster whose values distinguish individual fires"),
    createsOutput("rstCurrentBurn", "RasterLayer", desc = "A binary raster with 1 values representing burned pixels.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.historicFires = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      sim$burnMap <- setValues(raster(sim$flammableRTM), getValues(sim$flammableRTM))
      sim$burnMap[getValues(sim$burnMap) == 0] <- NA ## make a map of flammable pixels with value 0
      sim$burnMap[!is.na(getValues(sim$burnMap)) & getValues(sim$burnMap) == 1] <- 0

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "historicFires", "loadFires", eventPriority = 5.13)
    },
    loadFires = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      pixelIDs <- which(!is.na(sim$fireMaps[[paste0("X", time(sim))]][]))
      fireIDs <- sim$fireMaps[[paste0("X", time(sim))]][pixelIDs]

      sim$rstAnnualBurnID <- sim$fireMaps[[paste0("X", time(sim))]]
      sim$rstCurrentBurn <- sim$fireMaps[[paste0("X", time(sim))]]
      sim$rstCurrentBurn[pixelIDs] <- 1
      sim$burnMap[pixelIDs] <- sim$burnMap[pixelIDs] + 1

      ## get fire year, pixels burned, area burned, poly ID of all burned pixels
      ## make burnSummary per SCFM and fireSense; based on spreadState DT -- see ?SpaDES.tools::spread2
      sim$burnDT <- data.table(fireID = fireIDs, ## will drop this column later; only used to get initial pixel ids
                               pixels = pixelIDs,
                               state = FALSE)
      sim$burnDT[, initialPixels := pixels[1], by = fireIDs] ## TODO: identify *actual* ignition pixel
      set(sim$burnDT, NULL, "fireID", NULL)

      tempDT <- sim$burnDT[, .(.N), by = "initialPixels"]
      tempDT$year <- time(sim)
      tempDT$areaBurnedHa <- tempDT$N * prod(res(sim$flammableRTM)) * 1e-4
      setnames(tempDT, c("initialPixels"), c("igLoc"))
      sim$burnSummary <- rbind(sim$burnSummary, tempDT)

      sim <- scheduleConditionalEvent(sim, "time(sim) %in% P(sim)$historicFireYears", "historicFires", "loadFires",
                                      eventPriority = 5.13,
                                      minEventTime = start(sim), maxEventTime = max(P(sim)$historicFireYears))

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere("studyArea", sim)) {
    stop("studyArea must be supplied.")
  }

  if (!suppliedElsewhere("flammableRTM", sim)) {
    stop("flammableRTM must be supplied. Create it using e.g., `LandR::defineFlammable()`.")
  }

  if (!suppliedElsewhere("fireMaps", sim)) {
    historicFires <- Cache(
      prepInputs,
      url = extractURL("fireMaps"),
      destinationPath = dPath,
      studyArea = sim$studyArea,
      useSAcrs = TRUE
    ) %>%
      st_cast("MULTIPOLYGON")

    yearsWithData <- unique(historicFires[["YEAR"]])
    noFireYears <- P(sim)$staticFireYears[!(P(sim)$staticFireYears %in% yearsWithData)]

    if (length(noFireYears)) {
      warning("No historic fires within study area for years: ", noFireYears)
    }

    historicFireRasters <- lapply(P(sim)$staticFireYears, function(year) {
      subst <- historicFires[historicFires$YEAR == year, ]
      subst$ID <- as.integer(factor(subst$FIRE_ID))
      fires <- if (nrow(subst) > 0) {
        suppressWarnings({
          fasterize::fasterize(subst, sim$flammableRTM, field = "ID", background = NA_integer_)
        })
      } else {
        raster::raster(sim$flammableRTM)
      }
      raster::mask(fires, sim$flammableRTM)
    })

    sim$fireMaps <- raster::stack(historicFireRasters)
    names(sim$fireMaps) <- paste0("X", fireMaps)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
