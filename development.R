

# tidync is a new higher level package to connect to netcdf files and convert data into tidy objects.

# install.packages("tidync")

require(tidync)

con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"

src <- tidync(con)

ncmeta::nc_meta(con)

print(src$source)
print(src$axis)
print(src$grid)
print(src$dimension)
print(src$variable)
print(src$transforms) %>% View()
