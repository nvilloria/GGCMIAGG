#' Runs all the functions needed for aggregating the GGCMI yields.
#'
#' The function sequentially runs the three functions needed to get
#' the aggregate yields.
#'
#' @param datafile Character string with the name of a RData file
#'     provided by Jonas, inclusive of path e.g
#'     "../data/dssat-pythia_gfdl-esm4_ssp126_default_production_and_yield_grid.RData"
#' @param crop One of "maize", "winter_wheat", "spring_wheat",
#'     "soybeans", or "rice".
#' @param regions Current options are are "countries" and
#'     "countriesAEZ18". Default is "countries".
#' @param weights Either NULL (when 'weights' = "none" in
#'     'agg.wrapper()') or the output of 'read.weights()'
#'
#' @return The output of \link[GGCMIAGG]{grid.agg}, that is, a dataframe with three
#'     columns: countries (or other regional identifier), year, and
#'     the grid-cell weighted average of relative yields.

agg.wrapper <- function(datafile, crop, regions = "countries", weights){
    weight.map <- read.weights(crop, weights)
    yielddat <- read.GGCMI.RData( datafile = datafile ,crop = crop )
    yielddat.agg <- grid.agg( data2agg = yielddat,
                             region.map= regions,
                             weight.map = weight.map)
    }
