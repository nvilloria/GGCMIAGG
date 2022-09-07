#' Reads RData files with GGCMI-CMIP6 yields in RData format
#'
#' Reads yield data (relative changes in percent compared with the
#' reference period 1983-2013) from the CMIP6 generation of GGCMI runs
#' provided by Jonas Jagermeyr on June 01, 2022. See [Jagermeyr et
#' al.](https://www.nature.com/articles/s43016-021-00400-y) for
#' description.
#'
#' @param datafile Character string with the name of a RData file
#'     provided by Jonas, inclusive of path e.g
#'     "../data/dssat-pythia_gfdl-esm4_ssp126_default_production_and_yield_grid.RData"
#' @param crop One of "maize", "winter_wheat", "spring_wheat",
#'     "soybeans", "wheat", or "rice".
#' @return A four-column dataframe
#'     ("lon","lat","time","value"). "time" are all the years in the
#'     dataset (e.g., for future runs 2016:2099), and value are the
#'     relative yields for `crop`. The data is ready to be aggregated
#'     geographically by `grid.agg()`, which merges the lon/lat
#'     variables to a lower resolution with the weights produced by
#'     'read.weights()`.

read.GGCMI.RData <- function(datafile = NULL, crop = NULL){
    cropnames <- c("maize", "winter_wheat", "spring_wheat", "soybeans", "rice")
    if( !crop %in% cropnames ){
        stop('Specify one of maize, winter_wheat, spring_wheat, soybeans, rice')
        }
    if( is.null(datafile) ){
        stop('Needs a RData file provided by Jonas, for example
"dssat-pythia_gfdl-esm4_ssp126_default_production_and_yield_grid.RData"')
    }else{
        load(datafile)
    }
    ## Add names to the dimensions of the arrays to facilitate
    ## aggregation by grid.agg.2:
    lons <- seq(from = -179.75, to = 179.75, by = 0.5)
    lats <- seq(from = 89.75, to = -89.75, by = -0.5)
    years <- c(2016:2099)
    dimnames(yield_grid) <- list(lons, lats, years, cropnames)
    ## Select specific crop:
    yield_grid_c  <- yield_grid[ , , ,crop]
    return(yield_grid_c)

    ## ## Collapse the yield array so it becomes a column:
    ## ## require(reshape2, quietly=TRUE)
    ## yield.long <- reshape2::melt(yield_grid_c)
    ## names(yield.long) <- c("lon","lat","time","value")
    ## ## Eliminate NAs
    ## ## yield.long <- yield.long[complete.cases(yield.long),]
    ## return(yield.long)
    }
