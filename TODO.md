# TODO

1) Confirm all of those functions (and trnasformations!) keep the 'sgo_points' class and the names in the class are always at least the (5-6) core columns. Plus any additional column

2) Test all the extended operators '\['. Check they do what is required from them and they keep all attributes/class of sgo, or basically test multiple ways of manipulating sgo objects (subset, merging, etc and see how it works)

---

- test bng where the OSGM heights are outside of the transformation area (flag16) test cases when BNG is out of boundaries: ie. 1000000 

 - Write in documentation somewhere:
**Accuracy**: Grid references rounded to whole metres will give lat/lon that are accurate to about 5 decimal places.  In the UK, 0.00001 of a degree of latitude is about 70cm, 0.00001 of a degree of longitude is about 1m.

- More NOTES:  
https://www.ordnancesurvey.co.uk/gps/transformation/ Ben Nevis: 216600,771200 (and try several heights)  
regarding distances: https://www.movable-type.co.uk/scripts/gis-faq-5.1.html

- Distances between points British National Grid (BNG): https://gis.stackexchange.com/a/324054 and put examples like: shetland-jersey, thurso-carlyle, harris-aberdeen,edinburgh-london,belfats-london, etc.

- OS spreadhseet this calcualtions another OS pdfs:  
F = s(grid distance)/S(True distance)   
S=s/F  
Lines up to 10km it is considerered constant at that distance, so lines up to 20km use F from the mid point. For greater accuracy: F = 1/6(F1 + 4Fm + F2) where Fm is scale factor at mid point between P1 and P2 (http://fgg-web.fgg.uni-lj.si/~/mkuhar/Zalozba/TM_projection.pdf)

- Comment that we consider **WGS84 AND ETRS89 EQUIVALENT**, but it is not really true, talk about ETRS89 (etrF89) and how it is diverging... so if more accuracy is requiered, then they should transform between 4326 and etrs89 with other tool.  
From Transformations and OSGM015™ User guide (https://www.ordnancesurvey.co.uk/business-government/tools-support/os-net/for-developers) says:  
"...In Europe, ETRS89 is a precise version of the better known WGS84 reference system optimised for use in Europe; however, for most purposes it can be considered equivalent to WGS84."  
Specifically, the motion of the European continental plate is not apparent in ETRS89, which allows a fixed relationship to be established between this system and Ordnance Survey mapping coordinate systems.  
Additional precise versions of WGS84 are currently in use, notably ITRS (International Terrestrial Reference System); these are not equivalent to ETRS89. The difference between ITRS and ETRS89 is in the order of 0.25 m (in 1999), and growing by 0.025 m per year in UK and Ireland. This effect is only relevant in international scientific applications. For all navigation, mapping, GIS, and engineering applications within the tectonically stable parts of Europe (including UK and Ireland), the term ETRS89 should be taken as synonymous with WGS84."  

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