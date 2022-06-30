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


|paramName         |paramClass |default      |min |max |paramDesc                                   |
|:-----------------|:----------|:------------|:---|:---|:-------------------------------------------|
|.useCache         |logical    |FALSE        |NA  |NA  |Should caching of events or module be used? |
|historicFireYears |integer    |2011, 20.... |NA  |NA  |parameter description                       |

# Events

- `loadFires`: Loads the historic fire layer for the current simulation year.

# Data dependencies

## Input data


|objectName   |objectClass |desc                                                           |sourceURL |
|:------------|:-----------|:--------------------------------------------------------------|:---------|
|flammableRTM |RasterLayer |RTM without ice/rocks/urban/water. Flammable map with 0 and 1. |NA        |

## Output data


|objectName      |objectClass |desc                                                      |
|:---------------|:-----------|:---------------------------------------------------------|
|burnDT          |data.table  |data.table with pixel IDs of most recent burn.            |
|burnMap         |RasterLayer |A raster of cumulative burns                              |
|burnSummary     |data.table  |Describes details of all burned pixels.                   |
|historicFires   |RasterStack |stack of annual historic fire perimeters.                 |
|rstAnnualBurnID |RasterLayer |annual raster whose values distinguish individual fires   |
|rstCurrentBurn  |RasterLayer |A binary raster with 1 values representing burned pixels. |

# Links to other modules

Can be used with wildfire simulators (*e.g.*, `fireSense` or `scfm`) to simulate landscape disturbances.
