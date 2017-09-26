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
#' }
#'
#'
#' @param ... arguments passed on to ...
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @keywords plotting
#' @seealso ...
#' @return 
#' foo
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
#'   scale_x_continuous(trans=warp_trans(x=seq(-10,10,by=1),y=runif(21)))
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


#' @export
#' @rdname transforms
interp_trans <- function(x,y,na.rm=TRUE) {
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

#' @export
#' @rdname transforms
warp_trans <- function(x,w,na.rm=TRUE) {
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
