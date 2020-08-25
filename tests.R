
require(ncdf4)
require(tidyverse)
require(sf)

source("app2/functionality.r")

ncdf4::nc_close(nc = nc)
con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"
nc <- ncdf4::nc_open(con)
names(nc$var)

nc$var$Si$size

# first dimension: layer
# second dimension: location
# third dimension: timestep

allVars <- map(nc$var, list("size"))

allVarsDims <- map(allVars, function(x) length(x)) %>% 
  unlist(use.names = T) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(variable = rowname, dims = ".")

#test
allvars <- allVars(nc)
vardims <- varDims(allvars)
df <- colbind_loc_vars(nc)

mapviewOptions(platform = "leaflet")
df %>%
  sf::st_as_sf(coords = c("station_x_coordinate", "station_y_coordinate"), crs = 4326) %>%
  mapview::mapview()



VolumeVars = allVarsDims %>%
  filter(dims == 3)



ncvar_get(nc, "station_x_coordinate")

locvars = c("station_x_coordinate", "station_y_coordinate",
      "station_name", "station_id")

colbind_loc_vars <- function(nc, vars){
  
  names(vars) = vars

  l <- lapply(vars, function(x) {
    ncdf4::ncvar_get(nc, x)
  }) 
  
  if(length(unique(lapply(l, dim))) == 1){
    df <- bind_cols(l)
  } else stop("not all variables have the same dimension")

  return(df)
}

df <- colbind_loc_vars(nc, locvars)

t <- ncdf4::ncvar_get(nc, "timestep")

ncdf4::ncvar_get(nc, "NH4") %>% dim()

ncdf4::nc_close(nc)

