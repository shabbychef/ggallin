# /usr/bin/r
#
# Copyright 2019-2019 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav 
#
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
#
# Created: 2019.11.17
# Copyright: Steven E. Pav, 2019
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

# helper function.

.comp_qq <- function(sample,qnt_func=qnorm,dens_func=dnorm,k=9,ppp=ppoints(k)) {
	# just in case
	ppp <- sort(ppp)

  nnn <- length(sample)
  xi <- qnt_func(ppp)
  xi_hat  <- quantile(sample,ppp)
  sigma2  <- ppp * (1-ppp) / (dens_func(xi)^2)
  serr <- sqrt(sigma2 / nnn)
  retv <- data.frame(ppp=ppp,
										 xi=xi,
										 xi_hat=xi_hat,
										 serr=serr,
										 delta=xi_hat - xi,
										 iii=seq_len(nnn))
  attr(retv,'nnn') <- nnn
  retv
}

#' @title stat_subqq .
#'
#' @description 
#'
#' A Q-Q plot for large sample sizes.
#'
#' @section Aesthetics:
#' \code{geom_cloud} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{sample}}
#'   \item \code{color}
#'   \item \code{alpha}
#' }
#'
#' @details
#'
#' Implements the large sample Q-Q plot of Velez and Correa Morales,
#' where the difference between sample quantiles and theoretical
#' quantiles are plotted versus decile or vigintile number,
#' along with standard error bars around the \eqn{x} axis.
#'
#' @usage
#'
#' stat_subqq(x, n, zeta, ...)
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
#' @template ggallin
#' @template ref-velez
#' @examples 
#' y <- stat_subqq(20, 10)
#' \dontrun{
#' y <- stat_subqq(20, 10)
#' }
#' @export
stat_subqq <- function(mapping=NULL, data=NULL, ..., 
											 position = 'dodge', 
											 na.rm = TRUE, 
											 distribution = stats::qnorm,
											 density = stats::dnorm,
											 cuts = 9,
											 inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomSubQQ,
    position = "identity",
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
			distribution = distribution,
			density = density,
			cuts = cuts,
      ...
    )
  )
}
#' ggallin ggproto objects
#' @rdname ggallin-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomSubQQ <- ggproto("GeomSubQQ", Geom,
  required_aes = c("sample"),
  non_missing_aes = c("color"),
  default_aes = aes(
    colour = NA, alpha = 1, size=1, linetype=1
  ),
	setup_data = function(data,params) {
		if (params$na.rm) {
			ok_row <- !(is.na(data$sample))
			data <- data[ok_row,]
		}

		# super hack. 
		if (is.null(params$cuts)) {
			params$cuts <- ifelse(nrow(data) > 1e4,19,9)
		}

		retv <- .comp_qq(sample=data$sample,
										 qnt_func=params$distribution,
										 dens_func=params$density,
										 k=params$cuts)
	},
  draw_group = function(data, panel_scales, coord,
												na.rm = TRUE,
												distribution, density, cuts) {
		gList(ggplot2::GeomPoint$draw_group(data, panel, scales, coord, na.rm=na.rm),
					ggplot2::GeomErrorBar$draw_group(data, panel, scales, coord, na.rm=na.rm))
  },
	draw_key = draw_key_polygon
)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
