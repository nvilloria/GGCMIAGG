#' Reads weights to aggregate GGCMI yields to regions
#'
#' This function should work with either CMIP5 or CMIP6 yields.
#'
#' Reads harvested harea (in ha) or production (in metric tones) uses
#' to weight relative yield changes in the aggregation from gridcells
#' to regions such as countries. The data for maize, soybeans, rice
#' and wheat is available in both ha and mt, and comes from [Monfreda
#' et al.](http://www.earthstat.org/harvested-area-yield-175-crops/)
#' Only area weights are available for winter and spring wheat; these
#' are from
#' [MIRCA](https://www.isimip.org/gettingstarted/details/14/).
#'
#' @param crop One of "maize", "winter_wheat", "spring_wheat",
#'     "soybeans", "wheat", or "rice".
#' @param weights One of "none", "area", or "production".
#' @return A dataset with three columns (lat, lon, weight) which is
#'     merged with the yield data for aggregation. In the
#'     \link[GGCMIAGG]{agg.wrapper} function, the argument `weight.map`
#'     takes the output of this function.

read.weights <- function(crop, weights){
    if( !weights %in% c("none", "area", "production") ){
        stop('The weights argument needs to be one of "none", "area", "production"')
        }
    if( weights == "none" ){
        weight.data <- NULL
    }else{

        if( crop %in% c("winter_wheat", "spring_wheat") & weights == "production" ){
            stop("Only area weights are available for winter and spring wheat.")
        }else{
            if(
                crop %in% c("winter_wheat", "spring_wheat") & weights == "area"
            ){
                weight.data.filename <- paste(crop, ".ha.mirca", sep = "")
                data( list = weight.data.filename, envir=environment())
                assign("weight.data",get(weight.data.filename))
            }else{
                weight.unit <- ifelse(weights == "area", "ha", "mt")
                weight.data.filename <- paste(crop, ".", weight.unit, ".monfreda", sep = "")
                data( list = weight.data.filename, envir=environment())
                assign("weight.data",get(weight.data.filename))
            }
        }
    }
    return(weight.data)
}
