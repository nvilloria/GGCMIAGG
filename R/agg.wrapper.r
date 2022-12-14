#' Runs all the functions needed for aggregating the GGCMI-CMIP6 yields.
#'
#' The function sequentially runs the three functions needed to get
#' the aggregate yields: \link[GGCMIAGG]{read.weights},
#' \link[GGCMIAGG]{read.AgMIP.nc},  and
#' \link[GGCMIAGG]{grid.agg}.
#'
#' @param datafile Character string with the name of a RData file
#'     provided by Jonas. May include a path e.g
#'     "../data/dssat-pythia_gfdl-esm4_ssp126_default_production_and_yield_grid.RData"
#' @param crop One of "maize", "soybeans", "wheat", or "rice".
#' @param region.map A regional mapping. Current options are
#'     "countries", "regionsGTAPV10.1" and "countriesAEZ18" or
#'     "custom". If "custom" is chosen, a custom map should be
#'     provided. Default is "countries". (See
#'     \link[GGCMIAGG]{grid.agg}).
#' @param custom.map A regional mapping from gridcells to user-defined
#'     region with columns labeled lon, lat, and id. (See
#'     \link[GGCMIAGG]{grid.agg})
#' @param weights One of "none", "area", "production".
#' @param type Either "relative" (default) or "physical". Relative are
#'     percentage changes relative to the reference period
#'     1983-2013. Physical are in MT/ha.
#' @param irrigation Either "total" (default), "rainfed", or
#'     "irrigated". It only matters when type="physical".
#'
#' @return The output of \link[GGCMIAGG]{grid.agg}, that is, a dataframe with three
#'     columns: countries (or other regional identifier), year, and
#'     the grid-cell weighted average of relative yields.
#' @export

agg.wrapper <- function(datafile, crop, region.map = "countries", custom.map = NULL,
                        weights, type="relative", irrigation="total"){
    weight.map <- read.weights(crop, weights)
    yielddat <- read.AgMIP.nc( datafile = datafile , targetcrop = crop )
    if(type=="physical"){
        data(observational_reference_yield)
        base.yields <- observational_reference_yield[,,crop,irrigation]
        yielddat <- simplify2array(apply(yielddat,3,function(x) (x/100 + 1)*base.yields,simplify=FALSE))
    }else{
        yielddat
        }
    yielddat.agg <- grid.agg( data2agg = yielddat,
                             region.map= region.map,
                             custom.map = custom.map,
                             weight.map = weight.map)
}
