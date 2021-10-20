# TODO

1) Confirm all of those functions (and trnasformations!) keep the 'sgo_points' class and the names in the class are always at least the (5-6) core columns. Plus any additional column

2) Test all the extended operators '\['. Check they do what is required from them and they keep all attributes/class of sgo, or basically test multiple ways of manipulating sgo objects (subset, merging, etc and see how it works)

3) Test bng where the OSGM heights are outside of the transformation area (flag16) test cases when BNG is out of boundaries: ie. 1000000 

---




### MORE:  
https://www.ordnancesurvey.co.uk/gps/transformation/ Ben Nevis: 216600,771200 (and try several heights)  
Regarding distances: https://www.movable-type.co.uk/scripts/gis-faq-5.1.html  
Distances between points British National Grid (BNG): https://gis.stackexchange.com/a/324054 and put examples like: shetland-jersey, thurso-carlyle, harris-aberdeen,edinburgh-london,belfats-london, etc.


### Tests:
```
# Area
e <- c(139533, 139859, 140135, 140491, 140392, 140163, 139950, 139755)
n <- c(933991, 934182, 934257, 934256, 934056, 934076, 934057, 933986)
# bng - sgo   : 133733 (without last being the same as first) 133733 (first and last are the same)
# bng - raster: 133733
```
```
lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546, -6.42248238, -6.42639092, -6.42998435, -6.43321409)
lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112, 58.21849188, 58.21853606, 58.21824033, 58.21748949)
# etrs89 - sgo:       133610.63 ("%.2f" and without last being the same as first) 133610.63 ("%.2f" last being same as first) (ok?)
# wgs84 - geosphere: 133610.64 ("%.2f" and without last being the same as first) 133610.64 ("%.2f" last being same as first)
# wgs84 - geodlib:   133610.6 (https://geographiclib.sourceforge.io/cgi-bin/Planimeter)
```