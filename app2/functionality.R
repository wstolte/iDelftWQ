require(ncdf4)
require(tidyverse)

# ncdf4::nc_close(nc = nc1)


colbind_loc_vars <- function(nc, vars = c("station_x_coordinate", "station_y_coordinate", "station_name", "station_id")){
  names(vars) = vars
  l <- lapply(vars, function(x) {ncdf4::ncvar_get(nc, x)}) 
  if(length(unique(lapply(l, dim))) == 1){df <- bind_cols(l)} else 
    stop("not all variables have the same dimension")
  return(df)
}

#test
# locvars = c("station_x_coordinate", "station_y_coordinate", "station_name", "station_id")
# df <- colbind_loc_vars(nc, locvars)

# subsetting nc_his file for locations, layers, variables and time steps
nc_his2df = function(nc, vars, station_id, layer, start = NULL, end = NULL){
  
  require(ncdf4)
  
  # locations
  # check availability and raise an error if not
  # find index for locs
  locs <- ncvar_get(nc, "station_id")
  ilocation <- which(locs %in% station_id)

  # layers
  # check availability and raise an error if not
  # find index for layer (obvious)
  # ilayer = layer

  # time steps
  # check availability and raise an error if not
  # (find index for time steps)  ( for now: all time steps)
  time1 = 1

  # extract data for variables according to indices
  # make start indices in right dimension,
  # and number of data steps to retrieve in each dimension (-1 for all data in dimension)
  ivars <- expand.grid(vars, layer, ilocation, time1)
  names(ivars) <- c("variable", "layer", "location", "time1")
  ivars$variable <- as.character(ivars$variable)
  names <- expand.grid(vars, station_id) %>% mutate(name = paste(Var1, Var2, sep = "__")) %>% select(name) %>% unlist()
  ivars.list <- split(ivars, seq(nrow(ivars)))
  names(ivars.list) = names
  
  # combn <- cross2(ivars.list, vars)
  # map(ivars.list, ~ c(.x$layer, .x$location))
  
  times <- tibble(timestep = ncvar_get(nc, "time"))
  
  map(ivars.list, ~ ncdf4::ncvar_get(nc, .x$variable, start = c(.x$layer, .x$location, .x$time1), count = c(1, 1, -1))) %>%
  bind_rows() %>% bind_cols(times) %>%
    mutate(datetime = as.POSIXct(timestep, origin = '1995-12-25',tz='UTC')) %>% select(-timestep) %>%
    gather(key = varloc, value = value, -datetime) %>% 
    separate(varloc, c("variable", "location"), sep = "__")

  # make list with
  # location metadata
  # data
  # perhaps also model version etc?
}


#test
# con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"
# nc <- ncdf4::nc_open(con)
# vars = c("NO3", "NH4")
# station_id = c("NOORDWK20", "NOORDWK70")
# layer = 1
# start = 1
# end = NULL
# 
# ncvar_get(nc, "NO3", start = c(1,1,1), count = c(1,1,-1)) %>% dim()
# ncvar_get(nc, "time")
# lapply(vars, function(x) ncvar_get(nc, x, start = c(1,1,1), count = c(1,1,-1)))
# # 
# dff <- nc_his2df(nc, vars, station_id, layer)
# 
# class(station_id)
