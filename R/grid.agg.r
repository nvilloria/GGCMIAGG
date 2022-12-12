#' Aggregates GGCMI yield data to regions
#'
#' This function should work with either CMIP5 or CMIP6 yields.
#'
#' Aggregates from the gridcell to the regions in region.map. The
#' function \link[GGCMIAGG]{read.AgMIP.nc} produces the input to
#' `data2agg`. \link[GGCMIAGG]{read.weights} produces the data for teh
#' argument `weight.map`.
#'
#' @param data2agg A dataframe produced by `read.GGCMI.RData()`
#' @param region.map A regional mapping. Current options are
#'     "countries", "regionsGTAPV10.1" and "countriesAEZ18" or
#'     "custom". If "custom" is chosen, a custom map should be
#'     provided. Default is "countries".
#' @param custom.map A regional mapping from gridcells to user-defined
#'     region with columns labeled lon, lat, and id.
#' @param weight.map Either NULL (when 'weights' = "none" in
#'     'agg.wrapper()') or the output of 'read.weights()'
#' @return A dataframe with three columns: countries (or other
#'     regional identifier), year, and the grid-cell weighted average
#'     of relative yields.
#' @export

grid.agg <- function(data2agg=NULL, region.map= NULL, custom.map = NULL, weight.map=NULL){

    ## Collapse the yield array so it becomes a column:
    ## require(reshape2, quietly=TRUE)
    yield.long <- reshape2::melt(data2agg)
    names(yield.long) <- c("lon","lat","time","value")
    data2agg <- yield.long

    ## Read regional maps:
    if( region.map == "custom" ){
        region.data <- custom.map
    }else{
        data( list = region.map, envir=environment())
        assign("region.data",get(region.map))
        }
    require(dplyr, quietly=TRUE)
    ## Check data to be aggregated:
    if(ncol(data2agg)>4)
        stop('Data to be aggregated must have four columns labeled lon, lat, time, and value')
    if((c("lon") %in% names(data2agg) &
        c("lat") %in% names(data2agg) &
        c("time") %in% names(data2agg) &
        c("value") %in% names(data2agg))==FALSE)
        stop('Data to be aggregated must be labeled lon, lat, time, and id')
    ## Check regional mapping file:
    if(ncol(region.data)>4)
        stop('Regional mapping must have three columns labeled lon, lat, and id')
    if((c("lon") %in% names(region.data) &
        c("lat") %in% names(region.data) &
        c("id") %in% names(region.data))==FALSE)
        stop('Regional mapping must be labeled lon, lat, and id')
    ## Merge AgMIP yields with regional mapping:
    suppressMessages(d <- dplyr::left_join(data2agg, region.data, by.x=c("lon","lat"), by.y=c("lon","lat")))
    d <- d[complete.cases(d),]
    ## AGGREGATION
    if(is.null(weight.map)){
        agg <- d %>% group_by(id, time) %>%
            summarize(mean = mean(value, na.rm = TRUE))
    }else{
        ## Check weigths file:
        if(ncol(weight.map)>4)
            stop('Weights file must have three columns labeled lon, lat, and weight')
        if((c("lon") %in% names(weight.map) & c("lat") %in% names(weight.map) & c("weight") %in% names(weight.map))==FALSE)
            stop('Weights file must be labeled lon, lat, and weight')
        ## If weights file is correct, merge with yields and is data:
        suppressMessages(d <- dplyr::left_join(d,weight.map , by =c("lon","lat")))
        d <- d[complete.cases(d),]
        ## Weighted Average:
        agg <- d%>% group_by(id,time) %>% summarize(w.ave.yield = weighted.mean(value, weight, na.rm = TRUE))
    }
    agg
}
