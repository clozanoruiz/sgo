# Get the OSTN15 for developers from Ordnance Survey
# https://www.ordnancesurvey.co.uk/business-and-government/help-and-support/navigation-technology/os-net/formats-for-developers.html

# OSTN15 coverage is extended from grid point (0,0) to (700000,1250000),
# although the accuracy far off shore should not be relied on more than about +/- 5m.

# The table of data supplied by the Ordnance Survey contains 876951 rows with entries
# for each km intersection between (0,0) and (700000, 1250000).

# Make sure this file exists in the folder (currently compressed in ./dev folder)
file <- "./data-raw/OSTN15_OSGM15_DataFile.txt"
OSTN15 <- read.csv(file)

# Encode shift data as integers representing the shift in m
ee <- OSTN15$ETRS89_OSGB36_EShift * 1000L
nn <- OSTN15$ETRS89_OSGB36_NShift * 1000L

min_e <- min(ee)  #  82140
min_n <- min(nn)  # -84180

ostn_east_shift  <- ee - min_e
ostn_north_shift <- nn - min_n

save(ostn_east_shift, ostn_north_shift, file = "./R/sysdata.rda",
     compress = "xz")
#load(file = "./R/sysdata.rda")
