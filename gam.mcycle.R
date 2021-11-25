library("magrittr")
data("mcycle", package="MASS")

gam_mdl <- mgcv::gam(accel~s(times), data=mcycle) 
plot(gam_mdl, residuals=T, pch=1)

coefficients(gam_mdl)

gam2_mdl <- mgcv::gam(accel~s(times, k=1), data=mcycle) 
plot(gam2_mdl, residuals=T, pch=1)


gam3_mdl <- mgcv::gam(accel~s(times, sp=.001), data=mcycle) 
plot(gam3_mdl, residuals=T, pch=1)