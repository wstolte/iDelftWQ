

# tidync is a new higher level package to connect to netcdf files and convert data into tidy objects.

# install.packages("tidync")

require(tidync)

con <- "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"

con <- 'https://watersysteemdata.deltares.nl/thredds/dodsC/watersysteemdata/Noordzee/test/DCSMhis1.nc'

con <- "http://opendap.deltares.nl/thredds/dodsC/opendap/rijkswaterstaat/waterbase/30_Zwevende_stof_in_mg_l_in_oppervlaktewater/nc/id410-ZWARTHN.nc"

# dit werkt niet  -- nu weer wel! nieuwe thredds server! 
src <- tidync(con)
print(src$grid) %>% View()
print(src$dimension) %>% View()
print(src$variable) %>% View()
print(src$transforms) %>% View()

# maar dit werkt wel
ncmeta <- ncmeta::nc_meta(con)

# dit werkt ook. (wordt ook gebruikt door tidync)
nc = RNetCDF::open.nc(con)

# werkt niet bij opendap, wel bij Netcdf vanaf p schijf
RNetCDF::var.get.nc(nc, "NO3")
RNetCDF::var.get.nc(nc, variable = 49)
RNetCDF::print.nc(nc) %>% write_file(path = "ncdfSummary.txt")

src %>% hyper_tibble(select_var = "NO3") %>% View()

print(ncmeta$source) %>% View()
print(ncmeta$axis) %>% View()


