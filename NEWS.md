# sgo 0.9.2

- Replaced `testthat` with `tinytest` for Unit Testing.
- A little tidy-up of both code and documentation.


# sgo 0.9.1

- **Fixed**: Now, when converting from 2D BNG to 3D BNG, the input is first converted  
  to 3D  by filling z with zeros. Previous versions just added zeros to the output.
- **Fixed**: When converting from 2D to 3D coordinates with the same datum the z  
  column was not  being filled with zeroes.
- **Fixed**: Points right on the upper boundaries of the OSTN grid are considered   
  outside of the OSTN15 rectangle.
- **Fixed**: Output text of the print function.
- Improved slightly the speed of 3D calculations from/to BNG.
- Improved some parts of sgo_transforms's documentation.


# sgo 0.9.0

- Initial release on CRAN.
