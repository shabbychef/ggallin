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

# Created: 2017.09.25
# Copyright: Steven E. Pav, 2017
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @title Various scale transforms.
#'
#' @description 
#'
#' Various scale transformations.
#'
#' @details
#'
#' The available transforms:
#' \itemize{
#' \item \code{ssqrt_trans} a signed square root transform appropriate for
#' negative or positive numbers.
#' \item \code{pseudolog10_trans} an \code{asinh} transformation, which is like
#' a logarithm, but appropriate for negative or positive numbers.  This
#' transformation was taken from the Win Vector blog, 
#' \url{https://win-vector.com/2012/03/01/modeling-trick-the-signed-pseudo-logarithm/}.
#' }
#'
#' @keywords plotting
#' @seealso \code{\link[scales]{trans_new}}.
#' @return 
#' A scale transformation object.
#'
#' @examples 
#' set.seed(1234)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=ssqrt_trans)
#'
#' set.seed(1234)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=pseudolog10_trans)
#'
#' @template etc
#' @rdname misc_transforms
#' @export
ssqrt_trans <- scales::trans_new(name      = 'signed square root',
																 transform = function(x) sign(x) * sqrt(abs(x)),
																 inverse   = function(y) sign(y) * y^2,
																 domain    = c(-Inf,Inf))

#' @seealso \url{https://win-vector.com/2012/03/01/modeling-trick-the-signed-pseudo-logarithm/}
#' @export
#' @rdname misc_transforms
pseudolog10_trans <- scales::trans_new(name      = 'pseudo log10',
																			 transform = function(x) asinh(x/2)/log(10),
																			 inverse   = function(y) 2 * sinh(y * log(10)),
																			 domain    = c(-Inf,Inf))


#' @title Interpolation based scale transforms.
#'
#' @description 
#'
#' Interpolation based scale transformations. The user supplies \eqn{x} and
#' \eqn{y} (which should be monotonic increasing or decreasing in \eqn{x}) 
#' to create a scale transformation based on linear interpolation. 
#'
#' A \sQuote{warp} transformation is also supported wherein the user supplies
#' \eqn{x} and \eqn{w} where, after sorting on \eqn{x}, the cumulative sum
#' of \eqn{w} are used as the \eqn{y} in an interpolation transformation.
#' Here \eqn{w} are the rate of increase, or \sQuote{weights}.
#'
#' @keywords plotting
#' @seealso \code{\link[scales]{trans_new}}.
#' @return 
#' A scale transformation object.
#'
#' @examples 
#' set.seed(1234)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=interp_trans(x=seq(-10,10,by=1),y=cumsum(runif(21))))
#'
#' set.seed(1234)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=warp_trans(x=seq(-10,10,by=1),w=runif(21)))
#'
#' # equivalently:
#' set.seed(1234)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=warp_trans(data=data.frame(x=seq(-10,10,by=1),w=runif(21))))
#'
#' # this is like trans_sqrt:
#' set.seed(1234)
#' myx <- seq(0,5,by=0.01)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_y_continuous(trans=interp_trans(x=myx,y=sqrt(myx)))
#'
#' @template etc
#' @rdname interpolation_transforms
#' @param x  the \eqn{x} coordinates for linear interpolation. 
#' @param y  the \eqn{y} coordinates for linear interpolation. 
#' @param data  A \code{data.frame} with columns of \code{x} and \code{y}
#' for \code{interp_trans} or \code{x} and \code{w} for
#' \code{warp_trans}. If \code{data} is given, it takes precedence over
#' the given \code{x, y, w}.
#' @param na.rm If \code{TRUE}, then missing \code{x} or \code{y} will
#' be removed.
#' @inheritParams scales::trans_new
#' @usage interp_trans(x=NULL,y=NULL,data=NULL,na.rm=TRUE,breaks=NULL,format=NULL)
#' 
#' @export
interp_trans <- function(x=NULL,y=NULL,data=NULL,na.rm=TRUE,breaks=NULL,format=NULL) {
	if (!is.null(x) && is.data.frame(x) && is.null(y) && is.null(data)) {
		# wrong order!
		data <- x
		x <- NULL
	}
	if (!is.null(data) && is.data.frame(data)) {
		stopifnot(all(c('x','y') %in% colnames(data)))
		x <- data$x
		y <- data$y
	}
	if (na.rm) {
		okxy <- !(is.na(x) | is.na(y))
		x <- x[okxy]
		y <- y[okxy]
	}
	afun <- approxfun(x=y,y=x,method='linear',rule=2)
	datex <- ('Date' %in% class(x))

	if (datex) {
		tfun <- approxfun(x=as.numeric(x),y=y,method='linear',rule=2)
		transfo <- function(x) tfun(as.numeric(x))
	} else {
		transfo <- approxfun(x=x,y=y,method='linear',rule=2)
	}
	if (datex) {
		ifun <- function(y) { as.Date(afun(y),origin=as.POSIXct('1970-01-01',tz='UTC')) }
	} else {
		ifun <- afun
	}

	domain <- c(min(as.numeric(x)),max(as.numeric(x)))
	if (is.null(breaks)) { breaks <- scales::trans_breaks(transfo,afun) }
	if (is.null(format)) { format <- scales::format_format() }
	scales::trans_new(name      = 'interpolated scale',
										transform = transfo,
										inverse   = ifun,
										breaks    = breaks,
										format    = format,
										domain    = domain)
}

#' @param w  the \eqn{w} coordinates for the \sQuote{warp} interpolation.
#' The cumulative sum of \code{w} are computed then fed to the
#' interpolation transform.
#' @usage warp_trans(x=NULL,w=NULL,data=NULL,na.rm=TRUE,breaks=NULL,format=NULL) 
#' @export
#' @rdname interpolation_transforms
warp_trans <- function(x=NULL,w=NULL,data=NULL,na.rm=TRUE,breaks=NULL,format=NULL) {
	if (!is.null(x) && is.data.frame(x) && is.null(w) && is.null(data)) {
		# wrong order!
		data <- x
		x <- NULL
	}
	if (!is.null(data) && is.data.frame(data)) {
		stopifnot(all(c('x','w') %in% colnames(data)))
		x <- data$x
		w <- data$w
	}
	if (na.rm) {
		okxy <- !(is.na(x) | is.na(w))
		x <- x[okxy]
		w <- w[okxy]
	}
	# check monotonicity
	stopifnot(all(w >= 0) || all(w<=0))
	# re-sort
	if (is.unsorted(x)) {
		xx <- sort(x,index.return=TRUE)
		x <- xx$x
		w <- w[xx$ix]
	}
	interp_trans(x=x,y=cumsum(w),na.rm=na.rm,breaks=breaks,format=format)
}

#' @title Composition of scale transforms.
#'
#' @description 
#'
#' A binary infix operator that allows one to compose together two
#' scale transformations. We should have that the transformation
#' \code{atrans \%of\% btrans} first applies \code{btrans}, then
#' applies \code{atrans} to the results. This is useful for
#' reversing scales, for example, along with other transformations.
#'
#' @keywords plotting
#' @seealso \code{\link[scales]{trans_new}}.
#'
#' @examples 
#' set.seed(1234)
#' # compose transformatins with %of%:
#' ggplot(data.frame(x=rnorm(100),y=exp(rnorm(100,mean=-2,sd=4))),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_y_continuous(trans=scales::reverse_trans() %of% scales::log10_trans())
#'
#' @param atrans a transformation object.
#' @param btrans a transformation object.
#'
#' @template etc
#' @return a transformation object that perfroms \code{atrans} on the output of \code{btrans}.
#' @usage atrans \%of\% btrans
#' @export
#' @rdname compose_transforms
`%of%` <- function(atrans,btrans) {
	awise_domain <- btrans$inverse(atrans$domain)
	bwise_domain <- btrans$domain
	domain <- c(max(min(awise_domain),min(bwise_domain)),
							min(max(awise_domain),max(bwise_domain)))
	scales::trans_new(name      = paste(atrans$name,'of',btrans$name),
										transform = function(x) atrans$transform(btrans$transform(x)),
										inverse   = function(y) btrans$inverse(atrans$inverse(y)),
										breaks    = btrans$breaks,
										format    = btrans$format,
										domain    = domain)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
