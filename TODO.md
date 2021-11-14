# TODO

0) Confirm all of those functions (and transformations) keep the 'sgo_points'
class and the names in the class are always at least the (5-6) core columns.
Plus any additional column

1) Add function to check if coordinates are within OSTN?
OSTN15 covers grid point (0, 0) to (700000, 1250000)
out.of.bounds <- (e < 0 | e >= 700000) | (n < 0 | n >= 1250000)

2) Add function c() and others? (for objects with with the same CRS)
