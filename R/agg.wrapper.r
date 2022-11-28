#' Runs all the functions needed for aggregating the GGCMI-CMIP6 yields.
#'
#' The function sequentially runs the three functions needed to get
#' the aggregate yields: \link[GGCMIAGG]{read.weights},
#' \link[GGCMIAGG]{read.GGCMI.RData},  and
#' \link[GGCMIAGG]{grid.agg}.
#'
#' @param datafile Character string with the name of a RData file
#'     provided by Jonas. May include a path e.g
#'     "../data/dssat-pythia_gfdl-esm4_ssp126_default_production_and_yield_grid.RData"
#' @param crop One of "maize", "winter_wheat", "spring_wheat",
#'     "soybeans", "wheat", or "rice".
#' @param region.map A regional mapping. Current options are
#'     "countries", "regionsGTAPV10.1" and "countriesAEZ18" or
#'     "custom". If "custom" is chosen, a custom map should be
#'     provided. Default is "countries". (See
#'     \link[GGCMIAGG]{grid.agg}).
#' @param custom.map A regional mapping from gridcells to user-defined
#'     region with columns labeled lon, lat, and id. (See
#'     \link[GGCMIAGG]{grid.agg})
#' @param weights Either NULL (when 'weights' = "none" in
#'     'agg.wrapper()') or the output of 'read.weights()'
#'
#' @return The output of \link[GGCMIAGG]{grid.agg}, that is, a dataframe with three
#'     columns: countries (or other regional identifier), year, and
#'     the grid-cell weighted average of relative yields.
#' @export

agg.wrapper <- function(datafile, crop, region.map = "countries", custom.map = NULL,
                        weights){
    weight.map <- read.weights(crop, weights)
    if( crop == "wheat"){
        wwh <- read.GGCMI.RData(datafile = datafile, crop = "winter_wheat")
        swh <- read.GGCMI.RData(datafile = datafile, crop = "spring_wheat")
        data(spring_wheat.mask)
        data(winter_wheat.mask)
        ## Wheat masks are binary and mutually exclusive so only one
        ## type of wheat is grown:
        w <- wwh*c(winter_wheat.mask)
        s <- swh*c(spring_wheat.mask)
        yielddat <- w+s
        }else{
            yielddat <- read.GGCMI.RData( datafile = datafile ,crop = crop )
            }
    yielddat.agg <- grid.agg( data2agg = yielddat,
                             region.map= region.map,
                             custom.map = custom.map,
                             weight.map = weight.map)
}
