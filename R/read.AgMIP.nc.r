#' Reads GGCMI yields from netcdf datafiles
#'
#' Reads yield data (relative changes in percent compared with the
#' reference period 1983-2013) from the CMIP6 generation of GGCMI runs
#' provided by Jonas Jaegermeyr on December 9th, 2022. See [Jaegermeyr
#' et al.](https://www.nature.com/articles/s43016-021-00400-y) for
#' description.
#'
#' @param datafile A NetCDF datafile with GGCMI yields for four crops
#'     (see cropsinnc) below, for 0.5 degree gridcells and years
#'     1983:2099. These are percentage changes in yields relative to
#'     1983-2013. These were provided by AgMIP GRIDded crop modeling
#'     initiative (Ag-GRID) coleads Jonas Jaegermeyr and Christoph
#'     Mueller.
#' @param start_year First year to be extracted. Defaults to 1983.
#' @param end_year Last year to be extracted. Defaults to 2099.
#' @param nc4varid The name of the variable containing the yields in
#'     the netcdf datafile. Defaults to "yield change"
#' @param targetcrop the crop (one of cropsinnc below) for which the
#'     change in relative yields are requested.
#' @param cropsinnc Available crops. Defaults to "maize", "wheat",
#'     "soybeans" and "rice".
#' @param var_lon Longitude variable in the NetCDF datafile. Defaults
#'     to "lon".
#' @param var_lat Latitude variable in the NetCDF datafile. Defaults
#'     to "lat".
#' @return A four-column dataframe
#'     ("lon","lat","time","value"). "time" are all the years in the
#'     dataset (e.g., for future runs 2016:2099), and value are the
#'     relative yields for `crop`. The data is ready to be aggregated
#'     geographically by `grid.agg()`, which merges the lon/lat
#'     variables to a lower resolution with the weights produced by
#'     'read.weights()`.
#' @export

read.AgMIP.nc <- function(datafile, start_year=1983, end_year=2099, nc4varid='yield change', targetcrop,
                          cropsinnc=c("maize", "wheat", "soybeans", "rice"),
                          var_lon="lon", var_lat="lat"){
    require(ncdf4, quietly=TRUE)
    suppressMessages(require(reshape, quietly=TRUE))
    ncdatafile <- nc_open(datafile)
    ## Get the longitudes and latitudes --- these are later used to
    ## identify the coordinate pairs for each climate observation:
    lon <- ncvar_get(ncdatafile, varid=var_lon)
    lat <- ncvar_get(ncdatafile, varid=var_lat)
    time <- c(start_year:end_year)
    ## Read yields (an array of 720X360X6):
    yield <- ncvar_get(ncdatafile, varid=nc4varid)
    ## Assign the longitudes and latitudes to facilitate merging with
    ## the other datafiles:
    dimnames(yield) <- list(lon,lat,time,cropsinnc)
    yield <- yield[,,,targetcrop]
    return(yield)
}
