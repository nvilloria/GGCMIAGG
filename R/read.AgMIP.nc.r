#' Reads GGCMI yields from netcdf files
#'
#' The function read.AgMIP.nc takes as an argument a file
#' name(character string) that identifies a unique ntcdf file, start
#' year, end year and variable id for yield, longitude, and latitude.
#' Look into the header of the ncdf4 file for these variables.  The
#' function converts the selected ncdf file into a table with four
#' columns: lon, lat, year, and projected.yield. NAs are eliminated.
#' This function is not used for the version of the tool using the
#' second generation of the GGCMI runs, but it is kept for legacy.
#' This function should work with either CMIP5 or CMIP6 yields.
#'
#' @param file A NetCDF file with GGCMI yields
#' @param start_year First year to be extracted.
#' @param end_year Last year to be extracted.
#' @param yield_crop The name of the variable containing the yields in
#'     the netcdf file. In the CMIP 5 yields it was a crop name. In
#'     the CMIP 6 yiels it's 'yield cahange'
#' @param var_lon Longitude variable in the NetCDF file.
#' @param var_lat Latitude variable in the NetCDF file.
#' @return A dataframe with four columns: lon, lat, year, and
#'     projected.yield. NAs are eliminated.
#' @export

read.AgMIP.nc <- function(file, start_year, end_year, yield_crop, var_lon, var_lat){
    require(ncdf4, quietly=TRUE)
    suppressMessages(require(reshape, quietly=TRUE))
    ncfile <- nc_open(file)
    ## Get the longitudes and latitudes --- these are later used to
    ## identify the coordinate pairs for each climate observation:
    lon <- ncvar_get(ncfile, varid=var_lon)
    lat <- ncvar_get(ncfile, varid=var_lat)
    time <- c(start_year:end_year)

    # print(lon)
    # print(lat)
    # print(time)
    # print(list(lon,lat,time))

    ## Read yields (an array of 720X360X6):
    yield <- ncvar_get(ncfile, varid=yield_crop)

    # Change to three dimensional data
    if(length(dim(yield))==2) {
        yield <- array(yield, dim=c(dim(yield)[1], dim(yield)[2], 1))
    }
    ## Assign the longitudes and latitudes to facilitate merging with
    ## the other files:

    dimnames(yield) <- list(lon,lat,time)

    ## Set non-land areas to NA before further processing:
    fillvalue <- ncatt_get(ncfile,yield_crop,"_FillValue")
    yield[yield==fillvalue$value] <- NA
    return(yield)
    ## ## Collapse the yield array so it becomes a column:
    ## yield.long <- melt(yield)
    ## names(yield.long) <- c("lon","lat","time","value")
    ## ## Eliminate NAs
    ## yield.long <- yield.long[complete.cases(yield.long),]
}
