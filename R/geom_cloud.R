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

#' @title geom_cloud 
#'
#' @description 
#'
#' Draw a normal uncertainty cloud as a ribbon
#'
#' Draws overlapping ribbons of the same identity to create 
#' a cloud of (Gaussian) uncertainty. Similar to an errorbar geom in
#' use, but visually less distracting (sometimes).
#'
#' @section Aesthetics:
#' \code{geom_cloud} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{ymin}}
#'   \item \strong{\code{ymax}}
#'   \item \code{fill}
#' }
#' Only one of \code{ymin} and \code{ymax} is strictly required.
#'
#' @details
#'
#' Assumes that \code{ymin} and \code{ymax} are plotted at a 
#' fixed number of standard errors away from \code{y}, then computes
#' a Gaussian density with that standard deviation, plotting a cloud
#' (based on \code{geom_ribbon}) with alpha proportional to the density.
#' This appears as a vertical \sQuote{cloud} of uncertainty. In use,
#' this geom should be comparable to \code{geom_errorbar}.
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param steps The integer number of steps, or equivalently, the number of
#'        overlapping ribbons. A larger number makes a smoother cloud
#'        at the possible expense of rendering time. Values larger than
#'        around 20 are typically not necessary.
#' @param max_alpha The maximum alpha at the maximum density. The cloud
#'        will have alpha no greater than this value.
#' @param se_mult The \sQuote{multiplier} of standard errors of the given
#'        \code{ymin} and \code{ymax}. If these are at one standard error,
#'        then let \code{se_mult} take the default value of 1.
#'
#' @keywords plotting
#'
#' @seealso
#'  \code{\link[ggplot2]{geom_ribbon}}: The underlying geom
#' @note
#' This is a thin wrapper on the \code{geom_ribbon} geom.
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @export
#' @examples
#' set.seed(2134)
#' nobs <- 200
#' mydat <- data.frame(grp=sample(c(0,1),nobs,replace=TRUE),
#'   colfac=sample(letters[1:2],nobs,replace=TRUE),
#'   rowfac=sample(letters[10 + (1:3)],nobs,replace=TRUE)) 
#' mydat$x <- seq(0,1,length.out=nobs) + 0.33 * mydat$grp
#' mydat$y <- 0.25 * rnorm(nobs) + 2 * mydat$grp) 
#' mydat$grp <- factor(mydat$grp)
#' mydat$se  <- sqrt(x)
#'
#' ggplot(mydat,aes(x=x,y=y,ymin=y-se,ymax=y+se,color=grp)) +
#' facet_grid(rowfac ~ colfac) + 
#' geom_line() + 
#' geom_errorbar() + 
#' labs(title='uncertainty by errorbar')
#'
#' ggplot(mydat,aes(x=x,y=y,ymin=y-se,ymax=y+se,fill=grp)) +
#' facet_grid(rowfac ~ colfac) + 
#' geom_line() + 
#' geom_cloud(steps=15,max_alpha=0.85) +
#' labs(title='uncertainty by cloudr')
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


#' Geom Proto
#' @rdname geom_cloud
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
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
