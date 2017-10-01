dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Type: Package
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", role=c("aut","cre"), 
    email="shabbychef@gmail.com"))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Grab Bag of 'ggplot2' Functions
BugReports: https://github.com/shabbychef/PKG_NAME()/issues
Description: Extra geoms and scales for ggplot2, including 'geom_cloud',
  'ssqrt_trans', 'pseudolog10_trans', 'interp_trans', 'warp_trans',
  and an infix compose operator for scale transforms.
Depends:
  ggplot2 (>= 2.2.1)
Suggests:
  knitr,
  testthat
Imports:
  scales,
  grid
RoxygenNote: 5.0.1
URL: https://github.com/shabbychef/PKG_NAME()
dnl VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4
