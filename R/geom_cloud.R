# This file is part of ggallin.
#
# ggallin is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ggallin is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with ggallin.  If not, see <http://www.gnu.org/licenses/>.

# Created: 2017.09.24
# Copyright: Steven E. Pav, 2017
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

# get points equally spaced in density 
.equal_ses <- function(steps) {
	xend <- c(0,4)
	endpnts <- dnorm(xend)
# perhaps use ppoints instead?
	deql <- seq(from=endpnts[1],to=endpnts[2],length.out=steps+1)
	davg <- (deql[-1] + deql[-length(deql)])/2
# invert
	xeql <- unlist(lapply(davg,function(d) {
					 uniroot(f=function(x) { dnorm(x) - d },interval=xend)$root
	}))
	xeql
}

#' @title geom_cloud .
#'
#' @description 
#'
#' One sentence or so that tells you some more.
#'
#' @details
#'
#' Really detailed. \eqn{\zeta}{zeta}.
#'
#' A list:
#' \itemize{
#' \item I use \eqn{n}{n} to stand for blah.
#' \item and so forth....
#' }
#'
#' @usage
#'
#' geom_cloud(x, n, zeta, ...)
#'
#' @param x vector of blah
#' @param n number of blah
#' @param ... arguments passed on to ...
#' @inheritParams dt
#' @inheritParams same_package_function
#' @inheritParams anotherPackage::function
#' @template param-ope
#' @return \code{dsr} gives the density, \code{psr} gives the distribution function,
#' \code{qsr} gives the quantile function, and \code{rsr} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @keywords distribution 
#' @keywords io
#' @keywords plotting
#' @aliases psr qsr rsr
#' @seealso t-distribution functions, \code{\link{dt}, \link{pt}, \link{qt}, \link{rt}}
#' @note
#' This is a thin wrapper on the t distribution. 
#' @template etc
#' @template sr
#' @template R
#' @references
#'
#' Johnson, N. L., and Welch, B. L. "Applications of the non-central t-distribution."
#' Biometrika 31, no. 3-4 (1940): 362-389. \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}
#'
#' @examples 
#' y <- geom_cloud(20, 10)
#' \dontrun{
#' y <- geom_cloud(20, 10)
#' }
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @export
library(ggplot2)
library(grid)

geom_cloud <- function(mapping = NULL, data = NULL, ...,
											 na.rm = TRUE,
											 steps = 7, se_mult=1, max_alpha=1,
											 inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomCloud,
    position = "identity",
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
			steps = steps,
			se_mult = se_mult,
			max_alpha = max_alpha,
      ...
    )
  )
}


GeomCloud <- ggproto("GeomCloud", Geom,
  required_aes = c("x", "y", "ymin", "ymax"),
  non_missing_aes = c("fill"),
  default_aes = aes(
    colour = NA, fill = NA, alpha = 1, size=1, linetype=1
  ),
	setup_data = function(data,params) {
		if (params$na.rm) {
			ok_row <- !(is.na(data$x) | is.na(data$y) | (is.na(data$ymin) & is.na(data$ymax)))
			data <- data[ok_row,]
		}
		ses <- .equal_ses(params$steps)
		data$up_se <- (1/params$se_mult) * (data$ymax - data$y)
		data$dn_se <- (1/params$se_mult) * (data$y - data$ymin)
		# a trick to get positions ok: puff up the ymax and ymin for now
		maxse <- max(ses)
		data$ymax  <- data$y + maxse * data$up_se
		data$ymin  <- data$y - maxse * data$dn_se
		data
	},
  draw_group = function(data, panel_scales, coord,
												na.rm = TRUE,
												steps = 5, se_mult=1, max_alpha=1) {

		data$alpha <- max_alpha / steps
		# 2FIX: use the coordinate transform? or just forget it?
		ses <- .equal_ses(steps)
		grobs <- Map(function(myse) {
									 this_data <- data
									 this_data$ymax <- this_data$y + myse * this_data$up_se
									 this_data$ymin <- this_data$y - myse * this_data$dn_se
									 ggplot2::GeomRibbon$draw_group(this_data, panel_scales, coord, na.rm=na.rm)
												},ses)
		do.call("gList",grobs)
  },
	draw_key = draw_key_polygon
)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
