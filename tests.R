
require(ncdf4)
require(tidyverse)
require(sf)

source("app2/functionality.r")

ncdf4::nc_close(nc = nc)

#test with north sea model output
con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"
nc <- ncdf4::nc_open(con)
vars = c("NO3", "NH4")
vars = "temperature"
station_id = c("NOORDWK20", "NOORDWK70")
layer = 1
start = 1
end = NULL
#

vars <- getVarNames(nc)
vars[vars$varName == "NO3",]


time = ncvar_get(nc, "time")
ncvar_get(nc, "Chlfa", start = c(1,165,1), count = c(1,1,-1)) %>% dim()
t_1 <- ncvar_get(nc, "temperature", start = c(1,217,1), count = c(1,1,-1))
t_20 <- ncvar_get(nc, "temperature", start = c(20,217,1), count = c(1,1,-1))
df <- bind_cols(time = time, t1 = t_1, t20 = t_20)

ggplot(df) +
  geom_line(aes(time, y = t1)) +
  geom_line(aes(time, y = t20), color = "red")

# onderin warmer???




ncvar_get(nc, "time")
# lapply(vars, function(x) ncvar_get(nc, x, start = c(1,1,1), count = c(1,1,-1)))
# #
dff <- nc_his2df(nc, vars, station_id, layer)
layer = 20
dff20 <- nc_his2df(nc, vars, station_id, layer)

ggplot() +
  geom_line(data = dff, aes(datetime, value), color = "red") +
  geom_line(data = dff20, aes(datetime, value), color = "blue")
# 
#  #
# # class(station_id)


df <- colbind_loc_vars(nc, locvars)

t <- ncdf4::ncvar_get(nc, "timestep")

ncdf4::ncvar_get(nc, "NH4") %>% dim()

ncdf4::ncvar_get(nc, "temperature", start = c(1, , .x$time1), count = c(1, 1, -1)))

ncdf4::nc_close(nc)

