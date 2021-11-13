# sgo 0.9.0.9000

* Now, when converting from 2D BNG to 3D BNG, the input is first converted to 
  3D  by filling z with zeros. Previous versions just added zeros to the output.
* Fixed: Points right on the upper boundaries of the OSTN grid are considered   
  outside of the OSTN15 rectangle.
* Improved some parts of sgo_transforms's documentation.
* Fixed the output text of the print function.

# sgo 0.9.0

* Initial release on CRAN.
