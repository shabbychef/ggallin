# Copyright 2017 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

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

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2017.09.29
# Copyright: Steven E. Pav, 2017-2017
# Author: Steven E. Pav
# Comments: Steven E. Pav

# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
#UNFOLD

library(ggplot2)
library(scales)

# setup
tmpf <- tempfile(fileext='.png')
#cat('printing to ',tmpf,'\n')
png(tmpf)

context("geom_cloud runs")#FOLDUP
test_that("basic usage",{#FOLDUP
	set.char.seed("b78cd6ea-03b8-4346-a23f-3190bafcd74e")

	set.seed(2134)
	nobs <- 200
	mydat <- data.frame(grp=sample(c(0,1),nobs,replace=TRUE),
		colfac=sample(letters[1:2],nobs,replace=TRUE),
		rowfac=sample(letters[10 + (1:3)],nobs,replace=TRUE)) 
	mydat$x <- seq(0,1,length.out=nobs) + 0.33 * mydat$grp
	mydat$y <- 0.25 * rnorm(nobs) + 2 * mydat$grp
	mydat$grp <- factor(mydat$grp)
	mydat$se  <- sqrt(mydat$x)

	ph <- ggplot(mydat,aes(x=x,y=y,ymin=y-se,ymax=y+se,color=grp)) +
		facet_grid(rowfac ~ colfac) + 
		geom_line() + 
		geom_errorbar() + 
		labs(title='uncertainty by errorbar')
	print(ph)

	ph <- ggplot(mydat,aes(x=x,y=y,ymin=y-se,ymax=y+se,fill=grp)) +
		facet_grid(rowfac ~ colfac) + 
		geom_line() + 
		geom_cloud(steps=15,max_alpha=0.85) +
		labs(title='uncertainty by cloudr')
	print(ph)
	# sentinel
	expect_true(TRUE)
})#UNFOLD
#UNFOLD

context("misc transforms work")#FOLDUP
test_that("basic usage",{#FOLDUP
	set.char.seed("5ad1ee23-bd0b-4b08-ac77-0d2addcbed7d")
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=ssqrt_trans)
	print(ph)


	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=pseudolog10_trans)
	print(ph)
	# sentinel
	expect_true(TRUE)
})#UNFOLD
#UNFOLD

context("interpolation transforms work")#FOLDUP
test_that("interp trans",{#FOLDUP
	set.char.seed("75745a61-3841-4e19-8fff-9e5781945182")

	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=interp_trans(x=seq(-10,10,by=1),y=cumsum(runif(21))))
	print(ph)

	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=interp_trans(data=data.frame(x=seq(-10,10,by=1),y=cumsum(runif(21)))))
	print(ph)

	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=interp_trans(data.frame(x=seq(-10,10,by=1),y=cumsum(runif(21)))))
	print(ph)

	# this is like trans_sqrt:
	set.seed(1234)
	myx <- seq(0,5,by=0.01)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_y_continuous(trans=interp_trans(x=myx,y=sqrt(myx)))
	print(ph)

	# try interp trans on dates !
	myx <- as.Date(seq(0,1000,by=1),origin='1970-01-01')
	ph <- ggplot(data.frame(x=myx[1:100],y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_y_continuous(trans=interp_trans(x=myx,y=sqrt(seq_along(myx))))
	print(ph)
	

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("warp trans",{#FOLDUP
	set.char.seed("421d6cc8-70cc-4a03-90ac-97c58e9bfa5b")

	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=warp_trans(x=seq(-10,10,by=1),w=runif(21)))
	print(ph)

	# equivalently:
	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=warp_trans(data=data.frame(x=seq(-10,10,by=1),w=runif(21))))
	print(ph)

	# negatives ok
	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=warp_trans(data=data.frame(x=seq(-10,10,by=1),w=-runif(21))))
	print(ph)

	# not sorted x
	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=warp_trans(data=data.frame(x=rev(seq(-10,10,by=1)),w=runif(21))))
	print(ph)

	# equivalently:
	set.seed(1234)
	ph <- ggplot(data.frame(x=rnorm(100),y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_x_continuous(trans=warp_trans(data.frame(x=seq(-10,10,by=1),w=runif(21))))
	print(ph)

	# try warp trans on dates !
	myx <- as.Date(seq(0,1000,by=1),origin='1970-01-01')
	ph <- ggplot(data.frame(x=myx[1:100],y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_y_continuous(trans=warp_trans(x=myx,w=0.1*sqrt(seq_along(myx))))
	print(ph)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
#UNFOLD

context("compose transforms work")#FOLDUP
test_that("basic usage",{#FOLDUP
	set.char.seed("f8c79fa5-680c-41b0-8dd6-e337b315cd08")

	# compose transformatins with %of%:
	ph <- ggplot(data.frame(x=rnorm(100),y=exp(rnorm(100,mean=-2,sd=4))),aes(x=x,y=y)) + 
		geom_point() + 
		scale_y_continuous(trans=scales::reverse_trans() %of% scales::log10_trans())
	print(ph)

	# reverse a warp trans on dates
	myx <- as.Date(seq(0,1000,by=1),origin='1970-01-01')
	ph <- ggplot(data.frame(x=myx[1:100],y=runif(100)),aes(x=x,y=y)) + 
		geom_point() + 
		scale_y_continuous(trans=scales::reverse_trans() %of% warp_trans(x=myx,w=0.1*sqrt(seq_along(myx))))
	print(ph)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
#UNFOLD

# teardown:
dev.off()
unlink(tmpf)

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
