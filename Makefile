######################
# 
# Created: 2017.09.24
# Copyright: Steven E. Pav, 2017
# Author: Steven E. Pav
######################

############### FLAGS ###############

VMAJOR 						 = 0
VMINOR 						 = 1
VPATCH  					 = 1
#VDEV 							 = 
VDEV 							 = .1001
PKG_NAME 					:= ggallin

RPKG_USES_RCPP 		:= 1

include ./rpkg_make/Makefile

# experimenting with building README.md in docker. 
# not working yet b/c I do not have the requisite packages in my docker image. sigh.
reame : $(PKG_INSTALLED) $(DOCKER_IMG)
	$(DOCKER) run -it --rm \
		--volume $(PWD):/srv:rw \
		--volume $$(pwd $(RLIB_D)):/opt/R/lib:rw \
		$(DOCKER_ENV) \
		--entrypoint="r" $(USER)/$(PKG_LCNAME)-crancheck \
		"-l" "knitr" "-l" "$(PKG_NAME)" \
		"-e" 'setwd(".");if (require(knitr)) { knit("README.Rmd") }'

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
