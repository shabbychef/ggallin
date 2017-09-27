# /usr/bin/r
#
# Created: 2017.09.25
# Copyright: Steven E. Pav, 2017
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

#' @title transforms .
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
#' a logarithm, but appropriate for negative or positive numbers.
#' \item \code{interp_trans} a function which generates a transform. The
#' user must supply \eqn{x} and \eqn{y}; the output transform performs the
#' linear interpolation from \eqn{x} to \eqn{y}. So, for example, to 
#' achieve something like a square root scale, let \eqn{x} span the domain
#' of interest and let \eqn{y = \sqrt{x}}.
#' \item \code{warp_trans} a function which generates a transform. This
#' is similar to \code{interp_trans}, but it accepts an \eqn{x} and 
#' non-negative \eqn{w} (\sQuote{weights}) which give the 
#' \emph{rate} of increase.  The data are resorted along \eqn{x}, 
#' then the cumulative sum of the \eqn{w} are computed and used as \eqn{y}
#' in linear interpolation via \code{interp_trans}.
#' }
#'
#'
#' @keywords plotting
#' @seealso \code{\link{scales::trans_new}}.
#' @return 
#' A scale transformation object.
#'
#' @examples 
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + geom_point() + scale_x_continuous(trans=ssqrt_trans)
#'
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=interp_trans(x=seq(-10,10,by=1),y=cumsum(runif(21))))
#'
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=warp_trans(x=seq(-10,10,by=1),w=runif(21)))
#'
#' # equivalently:
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_x_continuous(trans=warp_trans(data=data.frame(x=seq(-10,10,by=1),w=runif(21))))
#'
#' # this is like trans_sqrt:
#' myx <- seq(0,5,by=0.01)
#' ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
#'   geom_point() + 
#'   scale_y_continuous(trans=interp_trans(x=myx,y=sqrt(myx)))
#'
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @rdname transforms
#' @export
ssqrt_trans <- scales::trans_new(name      = 'signed square root',
																 transform = function(x) sign(x) * sqrt(abs(x)),
																 inverse   = function(y) sign(y) * y^2,
																 domain    = c(-Inf,Inf))

#' @seealso \url{http://www.win-vector.com/blog/2012/03/modeling-trick-the-signed-pseudo-logarithm/}
#' @export
#' @rdname transforms
pseudolog10_trans <- scales::trans_new(name      = 'pseudo log10',
																			 transform = function(x) asinh(x/2)/log(10),
																			 inverse   = function(y) 2 * sinh(y * log(10)),
																			 domain    = c(-Inf,Inf))


#' @param x  the \eqn{x} coordinates for linear interpolation. 
#' @param y  the \eqn{y} coordinates for linear interpolation. 
#' @param data  A \code{data.frame} with columns of \code{x} and \code{y}
#' for \code{interp_trans} or \code{x} and \code{w} for
#' \code{warp_trans}. If \code{data} is given, it takes precedence over
#' the given \code{x, y, w}.
#' 
#' @export
#' @rdname transforms
interp_trans <- function(x=NULL,y=NULL,data=NULL,na.rm=TRUE) {
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
	tfun <- approxfun(x=x,y=y,method='linear',rule=2)
	ifun <- approxfun(x=y,y=x,method='linear',rule=2)
	domain <- c(min(x),max(x))
	scales::trans_new(name      = 'interpolated scale',
										transform = function(x) tfun(x),
										inverse   = function(y) ifun(y),
										domain    = domain)
}

#' @param w  the \eqn{w} coordinates for the \sQuote{warp} interpolation.
#' The cumulative sum of \code{w} are computed then fed to the
#' interpolation transform.
#' @export
#' @rdname transforms
warp_trans <- function(x=NULL,w=NULL,data=NULL,na.rm=TRUE) {
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
	stopifnot(all(w >= 0))
	if (is.unsorted(x)) {
		xx <- sort(x,index.return=TRUE)
		x <- xx$x
		w <- w[xx$ix]
	}
	interp_trans(x=x,y=cumsum(w),na.rm=na.rm)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
