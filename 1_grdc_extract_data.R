rm(list =ls())

library(CFtime)
library(ggplot2)
library(ncdf4)
library(tidyverse)

region_st_names <- c("BLINKWATER (27673117)",
    "GREY KIRK (27673119)",
    "KWA KHAYALETU (27672414)",
    "ADELAIDE (27673102)",
    "MATOLEMA'S LOCATION OUTSPAN (1160580)",
    "FARM 7 (1160600)")

grdc_dir <- "data"
grdc_fl <- "GRDC-Daily.nc"

# function definitions --------------------------------------------------------
extract_stations_df <- function(
    data_nc=runoff,
    nc_grdc_id=grdc_id,
    st_names=station_name,
    st_names_lookup=region_st_names
){

    i_station_grdc <- which(nc_grdc_id %in% nc_grdc_id[st_names %in% st_names_lookup])
    print(
        paste0(
            "GRDC stations found: ",
            paste(st_names[st_names %in% st_names_lookup], collapse="   ")
        )
    )
    
    res_df <- as.data.frame(
        t(data_nc[i_station_grdc, ]),
        optional=TRUE
    )
    colnames(res_df) <- st_names[st_names %in% st_names_lookup]

    # works to select rows where all columns are available
    #i_start_meaningful <- min(which(!is.na(rowSums(res_df))))

    i_start_meaningful <- min(which(rowSums(!is.na(res_df[, 1:4])) > 0))

    res_df[, "time"] <- nc_time_dates

    # trim leading NAs
    res_df <- res_df[i_start_meaningful:nrow(res_df), ]

    return(res_df)
}

# read data from nc-file ------------------------------------------------------
grdc_nc <- nc_open(file.path(grdc_dir, grdc_fl))

# differs from ids specified along with stations names
grdc_id <- ncvar_get(grdc_nc, "id")
# CF magic to get time work properly
nc_time_cf <- CFtime(grdc_nc$dim$time$units,
                     grdc_nc$dim$time$calendar,
                      grdc_nc$dim$time$vals)

runoff <- ncvar_get(grdc_nc, "runoff_mean")
river_name <- ncvar_get(grdc_nc, "river_name")
catchment_area <- ncvar_get(grdc_nc, "area")
station_name <- ncvar_get(grdc_nc, "station_name")

gauge_x <- ncvar_get(grdc_nc, "geo_x")
gauge_y <- ncvar_get(grdc_nc, "geo_y")
gauge_z <- ncvar_get(grdc_nc, "geo_z")

nc_close(grdc_nc)

# -----------------------------------------------------------------------------
# can take quite some time
nc_time_dates <- CFtimestamp(nc_time_cf)

#i_station_grdc <- which(grdc_id == grdc_id[station_name %in% c("BLINKWATER (27673117)")])

runoff_df <- extract_stations_df(
    data_nc=runoff,
    nc_grdc_id=grdc_id,
    st_names_lookup=region_st_names
)

write.csv(runoff_df, "data/grdc_records.csv", row.names=FALSE)








