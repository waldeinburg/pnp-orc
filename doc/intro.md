# Introduction to pnp-orc

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

Assumptions:

- Pages are either portrait and should be printed long edge or
  landscape and should be printed short edge.
- Embedded images are rendered using an at least approximately equal
  scale factor for the x- and y-axis.
- Embedded images are rendered using an at least approximately equal
  scale factor for each image.
- Embedded images are not so much scaled down that an 1 px line will be
  invisible when printed. That is, we can safely generate a bitmap with
  1 px cut lines and print that to scale.