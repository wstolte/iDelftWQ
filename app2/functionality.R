

getPackage <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
  return(TRUE)
}


getPackage("ncdf4")
getPackage("tidyverse")

# ncdf4::nc_close(nc = nc1)


colbind_loc_vars <- function(nc, vars = c("station_x_coordinate", "station_y_coordinate", "station_name", "station_id")){
  names(vars) = vars
  l <- lapply(vars, function(x) {ncdf4::ncvar_get(nc, x)}) 
  if(length(unique(lapply(l, dim))) == 1){df <- bind_cols(l)} else 
    stop("not all variables have the same dimension")
  return(df)
}

allVars <- function(nc){
  map(nc$var, list("size"))
}


varDims <- function(allvars){
  map(allvars, function(x) length(x)) %>%
    unlist(use.names = T) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(variable = rowname, dims = ".")
}

# subsetting nc_his file for locations, layers, variables and time steps
nc_his2df = function(nc, vars, station_id, layer, start = NULL, end = NULL){
  
  
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
    mutate(
      datetime = as.POSIXct(timestep, 
                            origin = sub("seconds since ", "", nc$dim$time$units)
      )
    ) %>% 
    select(-timestep) %>%
    gather(key = varloc, value = value, -datetime) %>% 
    separate(varloc, c("variable", "location"), sep = "__")

  # make list with
  # location metadata
  # data
  # perhaps also model version etc?
}


Ecoplot_bloom <- function(con, 
                     locmod, 
                     submod, 
                     limmod = c("Limit e", "Limit nit", "Limit pho", 
                                "Limit sil", "Limit gro", "Limit mor"), 
                     plottype = 1) {
  
  # use con to:
  # read limitation parameter data
  # read submod, locmod, limmod
  # Then
  # make plots for left and right
  
  
  
  
  
  if (require("plyr")) {
    print("plyr is loaded correctly")
  }
  else {
    print("trying to install plyr")
    install.packages("plyr")
    if (require(plyr)) {
      print("plyr installed and loaded")
    }
    else {
      stop("could not install plyr")
    }
  }
  df.y <- arr2df(arr = arr, locmod = locmod, submod = submod)
  lablim = mapvalues(limmod, c("Limit e", "Limit nit", "Limit pho", 
                               "Limit sil", "Limit gro", "Limit mor"), c("L", "N", "P", 
                                                                         "Si", "gro", "mor"))
  df.lim <- arr2df(arr, locmod, limmod)
  df.lim$variable <- factor(df.lim$variable)
  yy = range(ceiling(df.y$value * 10)/10, na.rm = T)
  steps <- seq(-yy[2]/10, -length(limmod) * yy[2]/10, by = -yy[2]/10)
  df.lim$step <- steps[as.numeric(as.factor(df.lim$variable))]
  colnames(df.lim) <- mapvalues(colnames(df.lim), from = "variable", 
                                to = "limitation")
  df.lim$limitation <- mapvalues(df.lim$limitation, c("Limit e", 
                                                      "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"), 
                                 c("light", "nitrogen", "phophorus", "silica", "growth", 
                                   "mortality"))
  library(ggplot2)
  library(scales)
  annotate.position.x <- as.POSIXct(as.numeric(min(df.lim$time)) - 
                                      as.numeric(min(df.lim$time))/1200, origin = "1970-01-01 00:00:00")
  z = ggplot(aes(time, value), data = df.y) + facet_grid(variable ~ 
                                                           location)
  if (plottype == 1) {
    z = z + geom_line(aes(x = time, y = step, color = limitation, 
                          size = value), data = df.lim)
  }
  if (plottype == 2) {
    z = z + geom_line(aes(x = time, y = step, color = limitation, 
                          alpha = value), data = df.lim, size = 3)
  }
  z = z + theme(text = element_text(size = 16)) + scale_x_datetime(breaks = date_breaks("2 months"), 
                                                                   minor_breaks = date_breaks("month"), labels = date_format("%b")) + 
    scale_y_continuous(expand = c(0.15, 0), breaks = pretty_breaks(n = 2)(yy)) + 
    scale_size_continuous(range = c(0, 4)) + annotate("text", 
                                                      x = annotate.position.x, y = steps, label = lablim, size = 3) + 
    theme_bw(base_size = 12, base_family = "") + theme(panel.border = element_blank(), 
                                                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                       axis.line = element_line(colour = "black"))
  z
}

#test
# con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"
# nc <- ncdf4::nc_open(con)
# vars = c("NO3", "NH4")
# station_id = c("NOORDWK20", "NOORDWK70")
# layer = 1
# start = 1
# end = NULL
# # 
# # ncvar_get(nc, "NO3", start = c(1,1,1), count = c(1,1,-1)) %>% dim()
# # ncvar_get(nc, "time")
# # lapply(vars, function(x) ncvar_get(nc, x, start = c(1,1,1), count = c(1,1,-1)))
# # # 
# dff <- nc_his2df(nc, vars, station_id, layer)
#  # 
# # class(station_id)
