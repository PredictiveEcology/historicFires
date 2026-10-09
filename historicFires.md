---
title: "historicFires"
author: "Alex Chubaty"
date: "28 June 2022"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Creates raster layers of historic fires, which can be used with a wildfire simulator (*e.g.*, `fireSense` or `scfm`).

# Parameters

Provide a summary of user-visible parameters.


|paramName       |paramClass |default      |min |max |paramDesc                                                 |
|:---------------|:----------|:------------|:---|:---|:---------------------------------------------------------|
|staticFireYears |integer    |2011, 20.... |NA  |NA  |simulation years for which static fire maps will be used. |
|.useCache       |logical    |FALSE        |NA  |NA  |Should caching of events or module be used?               |

# Events

- `loadFires`: Loads the historic fire layer for the current simulation year.

# Data dependencies

## Input data


|objectName   |objectClass |desc                                                                                                                                             |sourceURL                                                                            |
|:------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------|
|fireMaps     |SpatRaster  |Annual layers of fire perimeters (e.g., historic or presimulated). Layer names correspond to simulation years for which that layer will be used. |https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip |
|flammableRTM |SpatRaster  |RTM without ice/rocks/urban/water. Flammable map with 0 and 1.                                                                                   |NA                                                                                   |
|studyArea    |sf          |Polygon to use as the study area. Must be provided by the user.                                                                                  |NA                                                                                   |

## Output data


|objectName      |objectClass |desc                                                                                                                                             |
|:---------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------|
|burnDT          |data.table  |data.table with pixel IDs of most recent burn.                                                                                                   |
|burnMap         |SpatRaster  |A raster of cumulative burns                                                                                                                     |
|burnSummary     |data.table  |Describes details of all burned pixels.                                                                                                          |
|fireMaps        |SpatRaster  |Annual layers of fire perimeters (e.g., historic or presimulated). Layer names correspond to simulation years for which that layer will be used. |
|rstAnnualBurnID |SpatRaster  |annual raster whose values distinguish individual fires                                                                                          |
|rstCurrentBurn  |SpatRaster  |A binary raster with 1 values representing burned pixels.                                                                                        |

# Links to other modules

Can be used with wildfire simulators (*e.g.*, `fireSense` or `scfm`) to simulate landscape disturbances.
