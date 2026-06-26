#-------------------------------------------------------------------------------------
#DSEM
#
#Krista, Oct 2025
#-------------------------------------------------------------------------------------
#Notes:
#On a mac I struggled to download dSEM/RTMB until I updated R to a version specifically made for the M1/M2 chip
#-------------------------------------------------------------------------------------

#install.packages("RTMB")
#install.packages("dsem")

library(dsem)
library(RTMB)

#copied bering sea example

# data(bering_sea)
# Z = ts( bering_sea )
# family = rep('fixed', ncol(bering_sea))
#              # Specify model
#              
#              sem = "
# # Link, lag, param_name
# log_seaice -> log_CP, 0, seaice_to_CP
# log_CP -> log_Cfall, 0, CP_to_Cfall
# log_CP -> log_Esummer, 0, CP_to_E
# log_PercentEuph -> log_RperS, 0, Seuph_to_RperS
# log_PercentCop -> log_RperS, 0, Scop_to_RperS
# log_Esummer -> log_PercentEuph, 0, Esummer_to_Suph
# log_Cfall -> log_PercentCop, 0, Cfall_to_Scop
# log_SSB -> log_RperS, 0, SSB_to_RperS
# log_seaice -> log_seaice, 1, AR1, 0.001
# log_CP -> log_CP, 1, AR2, 0.001
# log_Cspring -> log_Cspring, 1, AR3, 0.001
# log_Cfall -> log_Cfall, 1, AR4, 0.001
# log_Esummer -> log_Esummer, 1, AR5, 0.001
# log_SSB -> log_SSB, 1, AR6, 0.001
# log_RperS -> log_RperS, 1, AR7, 0.001
# log_PercentEuph -> log_PercentEuph, 1, AR8, 0.001
# log_PercentCop -> log_PercentCop, 1, AR9, 0.001
# "
#              # Fit
#              fit = dsem( sem = sem,
#                          tsdata = Z,
#                          family = family,
#                          control = dsem_control(use_REML=FALSE, quiet=TRUE) )
#              #> Warning in nlminb(start = startpar, objective = fn, gradient = gr, control = nlminb.control, : NA/NaN
#              #> Warning in nlminb(start = startpar, objective = fn, gradient = gr, control = nlminb.control, : NA/NaN
#              ParHat = fit$obj$env$parList()
#              # summary( fit )
#              # Timeseries plot
#              oldpar <- par(no.readonly = TRUE)
#              par( mfcol=c(3,3), mar=c(2,2,2,0), mgp=c(2,0.5,0), tck=-0.02 )
#              for(i in 1:ncol(bering_sea)){
#                tmp = bering_sea[,i,drop=FALSE]
#                tmp = cbind( tmp, "PSEM"=ParHat$x_tj[,i] )
#                SD = as.list(fit$opt$SD,what="Std.")$x_tj[,i]
#                tmp = cbind( tmp, outer(tmp[,2],c(1,1)) +
#                               outer(ifelse(is.na(SD),0,SD),c(-1,1)) )
#                #
#                plot( x=rownames(bering_sea), y=tmp[,1], ylim=range(tmp,na.rm=TRUE),
#                      type="p", main=colnames(bering_sea)[i], pch=20, cex=2 )
#                lines( x=rownames(bering_sea), y=tmp[,2], type="l", lwd=2,
#                       col="blue", lty="solid" )
#                polygon( x=c(rownames(bering_sea),rev(rownames(bering_sea))),
#                         y=c(tmp[,3],rev(tmp[,4])), col=rgb(0,0,1,0.2), border=NA )
#              }

#AI attempt======

#DAG is in power point sablefish_DAGs S4

Z = ts( AI_df[,-c(1:2)] ) #using placeholder names SSB and SST for now, need that data

family = rep('fixed', ncol(AI_df[,-c(1:2)])) #EDIT THIS LINE NOT SURE WHAT TO DO
 # Specify model
sem = "
             # Link, lag, param_name
             Annual_Eddy_Kinetic_Energy_Amchitka_Satellite -> Rec, 2, EKE_to_rec
             SSB -> Rec, 2, SSB_to_rec
             SST -> Rec, 1, SST_to_rec_lag1
             SST -> Rec, 2, SST_to_rec_lag2
             Annual_Eddy_Kinetic_Energy_Amchitka_Satellite -> Annual_Eddy_Kinetic_Energy_Amchitka_Satellite, 1, AR1, 0.001
             SSB -> SSB, 1, AR2, 0.001
             SST -> SST, 1, AR3, 0.001
             "

             # Fit
             fit = dsem( sem = sem,
                         tsdata = Z,
                         family = family,
                         control = dsem_control(use_REML=FALSE, quiet=TRUE) )
ParHat = fit$obj$env$parList()
# summary( fit )


#BS attempt======

#DAG is in power point sablefish_DAGs S4

#look at covars against yr
ggplot(BS_df, aes(Year, Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery)) + geom_point() + geom_line()

ggplot(BS_df, aes(Year, Spring_Temperature_Surface_SEBS_Satellite)) + geom_point() + geom_line()

ggplot(BS_df, aes(Year, SSB)) + geom_point() + geom_line()

ggplot(BS_df, aes(Year, Rec)) + geom_point() + geom_line()


#look at covars against Rec BUT NOT LAGGED

ggplot(BS_df, aes(Rec, Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery)) + geom_point() 

ggplot(BS_df, aes(Rec, Spring_Temperature_Surface_SEBS_Satellite)) + geom_point() 

ggplot(BS_df, aes(Rec, SSB)) + geom_point() 


#against each other BUT NOT LAGGED

ggplot(BS_df, aes(Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery, Spring_Temperature_Surface_SEBS_Satellite)) + geom_point() 

ggplot(BS_df, aes(Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery, SSB)) + geom_point() 

ggplot(BS_df, aes(Spring_Temperature_Surface_SEBS_Satellite, SSB)) + geom_point() 

#look again but logged, jump down to run log

#look at covars against Rec

ggplot(log_BS_df, aes(Rec, incidental_catch_plus_small_const)) + geom_point() 
ggplot(log_BS_df, aes(Rec, Spring_Temperature_Surface_SEBS_Satellite)) + geom_point() 
ggplot(log_BS_df, aes(Rec, SSB)) + geom_point() 

#against each other

ggplot(log_BS_df, aes(incidental_catch_plus_small_const, Spring_Temperature_Surface_SEBS_Satellite)) + geom_point() 
ggplot(log_BS_df, aes(incidental_catch_plus_small_const, SSB)) + geom_point() 
ggplot(log_BS_df, aes(Spring_Temperature_Surface_SEBS_Satellite, SSB)) + geom_point() 

Z = ts( BS_df[,-c(1:2)] ) #not scaled covariates
#
family = rep('fixed', ncol(BS_df[,-c(1:2)])) #EDIT THIS LINE fixed assumes no obs error

#getting error about very different variances and to consider rescaling, also error some params might not be identifiable (likely related)

#let's log covars
#logging incidental catch doesn't work b/c zeros
BS_df$incidental_catch_plus_small_const <- BS_df$Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery + 0.0001

log_BS_df <- log(BS_df[,-c(1:2)]) 

Z = ts( log_BS_df[,-c(3,5)] ) #logged covars, drop incidental without small constant
#
family = rep('fixed', ncol(log_BS_df[,-c(3,5)])) #EDIT THIS LINE fixed assumes no obs error


# Specify model
sem = "
             # Link, lag, param_name
             Spring_Temperature_Surface_SEBS_Satellite -> Rec, 1, SST_to_rec_lag1 
             Spring_Temperature_Surface_SEBS_Satellite -> Rec, 2, SST_to_rec_lag2
             #Spring_Temperature_Surface_SEBS_Satellite -> incidental_catch_plus_small_const, 1, SST_to_bycatch #DROPPING
             SSB -> Rec, 1, SSB_to_rec 
             #incidental_catch_plus_small_const -> Rec, 1, bycatch_to_rec #DROPPING
             SSB -> SSB, 1, AR1, 0.001
             Spring_Temperature_Surface_SEBS_Satellite  -> Spring_Temperature_Surface_SEBS_Satellite , 1, AR2, 0.001
             "

# Fit
#getting an error about NAs in i and j in sparse Matrix? Doublecheck all covariates are spelled/cased correctly in SEM
fit = dsem( sem = sem,
            tsdata = Z,
            family = family,
            control = dsem_control(use_REML=FALSE, quiet=FALSE) )
ParHat = fit$obj$env$parList()
summary( fit )


#try BS again with ADFG large mesh - in case they get advected into the BS OR are leaving the BS

log_BS_df <- log(BS_df[,-c(1:2)]) 

Z = ts( log_BS_df[,-c(3)] ) #logged covars, drop incidental without small constant
#
family = rep('fixed', ncol(log_BS_df[,-c(3)])) #EDIT THIS LINE fixed assumes no obs error


# Specify model
sem = "
             # Link, lag, param_name
             Spring_Temperature_Surface_SEBS_Satellite -> Rec, 1, SST_to_rec_lag1 
             Spring_Temperature_Surface_SEBS_Satellite -> Rec, 2, SST_to_rec_lag2
             Spring_Temperature_Surface_SEBS_Satellite -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SST_to_largemesh
             SSB -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SSB_to_largemesh 
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 1, largemesh_to_rec1 
                          Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 2, largemesh_to_rec2 
                                       Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 3, largemesh_to_rec3 
             SSB -> SSB, 1, AR1, 0.001
             Spring_Temperature_Surface_SEBS_Satellite  -> Spring_Temperature_Surface_SEBS_Satellite , 1, AR2, 0.001
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, AR3, 0.001
             "

# Fit
#getting an error about NAs in i and j in sparse Matrix? Doublecheck all covariates are spelled/cased correctly in SEM
fit = dsem( sem = sem,
            tsdata = Z,
            family = family,
            control = dsem_control(use_REML=FALSE, quiet=FALSE) )
ParHat = fit$obj$env$parList()
summary( fit )



#WGOA attempt==============

#look at covars
names(WGOA_df)

ggplot(WGOA_df, aes(Year, Spring_Temperature_Surface_GOA_Satellite)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, summer_NGAO)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, summer_DW)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, SSB)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, Summer_Sablefish_CPUE_Juvenile_GOA_Survey)) + geom_point() + geom_line()
ggplot(WGOA_df, aes(Year, Annual_Copepod_Community_Size_WGOA_Survey)) + geom_point() + geom_line()

#log covars
#NOT LOGGING for now, causes NaNs in copepods, DW, NGAO
#log_WGOA_df <- log(WGOA_df[,c(3,4,5,6,7,8,12,13)]) 

Z = ts( WGOA_df[,c(3,4,5,6,7,8,12,13)] ) #
#
family = rep('fixed', ncol(WGOA_df[,c(3,4,5,6,7,8,12,13)])) #EDIT THIS LINE fixed assumes no obs error


# Specify model
sem = "
             # Link, lag, param_name
             Spring_Temperature_Surface_GOA_Satellite -> Rec, 1, SST_to_rec_lag1 
             Spring_Temperature_Surface_GOA_Satellite -> Annual_Copepod_Community_Size_WGOA_Survey, 1, SST_to_Copepod_lag1 
             Annual_Copepod_Community_Size_WGOA_Survey -> Rec, 1, Copepod_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 1, largemesh_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Rec, 1, GOAsurv_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Annual_Copepod_Community_Size_WGOA_Survey, 1, largemesh_to_Copepod_lag1 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Annual_Copepod_Community_Size_WGOA_Survey, 1, GOAsurv_to_Copepod_lag1 
             
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SST_to_largemesh_lag1 
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SST_to_GOAsurv_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SSB_to_largemesh_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SSB_to_GOAsurv_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, NGAO_to_largemesh_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, NGAO_to_GOAsurv_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, DW_to_largemesh_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, DW_to_GOAsurv_lag1 
             
             SSB -> SSB, 1, AR1, 0.001
             Spring_Temperature_Surface_GOA_Satellite -> Spring_Temperature_Surface_GOA_Satellite, 1, AR2, 0.001
             summer_NGAO -> summer_NGAO, 1, AR3, 0.001
             summer_DW -> summer_DW, 1, AR4, 0.001
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, AR5, 0.001
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, AR6, 0.001
             Annual_Copepod_Community_Size_WGOA_Survey -> Annual_Copepod_Community_Size_WGOA_Survey, 1, AR7, 0.001
             "

# Fit
#getting an error about NAs in i and j in sparse Matrix? Doublecheck all covariates are spelled/cased correctly in SEM
fit = dsem( sem = sem,
            tsdata = Z,
            family = family,
            control = dsem_control(use_REML=FALSE, quiet=FALSE) )
ParHat = fit$obj$env$parList()
summary( fit )

Z = ts( WGOA_df[,c(3,4,5,6,7,8,12,13)] ) #
#
family = rep('fixed', ncol(WGOA_df[,c(3,4,5,6,7,8,12,13)])) #EDIT THIS LINE fixed assumes no obs error


#AGAIN w multiple lags
# Specify model
sem = "
             # Link, lag, param_name
             Spring_Temperature_Surface_GOA_Satellite -> Rec, 1, SST_to_rec_lag1 
              Spring_Temperature_Surface_GOA_Satellite -> Rec, 2, SST_to_rec_lag2 
             Spring_Temperature_Surface_GOA_Satellite -> Annual_Copepod_Community_Size_WGOA_Survey, 1, SST_to_Copepod_lag1 
             Annual_Copepod_Community_Size_WGOA_Survey -> Rec, 1, Copepod_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 1, largemesh_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Rec, 1, GOAsurv_to_rec_lag1 
                          Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 2, largemesh_to_rec_lag2 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Rec, 2, GOAsurv_to_rec_lag2 
                          Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 3, largemesh_to_rec_lag3 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Rec, 3, GOAsurv_to_rec_lag3 
             
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SST_to_largemesh_lag1 
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SST_to_GOAsurv_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SSB_to_largemesh_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SSB_to_GOAsurv_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, NGAO_to_largemesh_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, NGAO_to_GOAsurv_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, DW_to_largemesh_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, DW_to_GOAsurv_lag1 
             
             SSB -> SSB, 1, AR1, 0.001
             Spring_Temperature_Surface_GOA_Satellite -> Spring_Temperature_Surface_GOA_Satellite, 1, AR2, 0.001
             summer_NGAO -> summer_NGAO, 1, AR3, 0.001
             summer_DW -> summer_DW, 1, AR4, 0.001
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, AR5, 0.001
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, AR6, 0.001
             Annual_Copepod_Community_Size_WGOA_Survey -> Annual_Copepod_Community_Size_WGOA_Survey, 1, AR7, 0.001
             "

# Fit
#getting an error about NAs in i and j in sparse Matrix? Doublecheck all covariates are spelled/cased correctly in SEM
fit = dsem( sem = sem,
            tsdata = Z,
            family = family,
            control = dsem_control(use_REML=FALSE, quiet=FALSE) )
ParHat = fit$obj$env$parList()
summary( fit )


#CGOA=======


Z = ts( CGOA_df[,c(3,4,5,6,7,11, 12)] ) #
#
family = rep('fixed', ncol(CGOA_df[,c(3,4,5,6,7,11, 12)])) #EDIT THIS LINE fixed assumes no obs error


# Specify model
sem = "
             # Link, lag, param_name
             Spring_Temperature_Surface_GOA_Satellite -> Rec, 1, SST_to_rec_lag1 
             #Spring_Temperature_Surface_GOA_Satellite -> Annual_Copepod_Community_Size_WGOA_Survey, 1, SST_to_Copepod_lag1 
             #Annual_Copepod_Community_Size_WGOA_Survey -> Rec, 1, Copepod_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Rec, 1, largemesh_to_rec_lag1 
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Rec, 1, GOAsurv_to_rec_lag1 
             
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SST_to_largemesh_lag1 
             Spring_Temperature_Surface_GOA_Satellite -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SST_to_GOAsurv_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, SSB_to_largemesh_lag1 
             SSB -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, SSB_to_GOAsurv_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, NGAO_to_largemesh_lag1 
             summer_NGAO -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, NGAO_to_GOAsurv_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, DW_to_largemesh_lag1 
             summer_DW -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, DW_to_GOAsurv_lag1 
             
             SSB -> SSB, 1, AR1, 0.001
             Spring_Temperature_Surface_GOA_Satellite -> Spring_Temperature_Surface_GOA_Satellite, 1, AR2, 0.001
             summer_NGAO -> summer_NGAO, 1, AR3, 0.001
             summer_DW -> summer_DW, 1, AR4, 0.001
             Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey -> Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey, 1, AR5, 0.001
             Summer_Sablefish_CPUE_Juvenile_GOA_Survey -> Summer_Sablefish_CPUE_Juvenile_GOA_Survey, 1, AR6, 0.001
             #Annual_Copepod_Community_Size_WGOA_Survey -> Annual_Copepod_Community_Size_WGOA_Survey, 1, AR7, 0.001
             "

# Fit
#getting an error about NAs in i and j in sparse Matrix? Doublecheck all covariates are spelled/cased correctly in SEM
fit = dsem( sem = sem,
            tsdata = Z,
            family = family,
            control = dsem_control(use_REML=FALSE, quiet=FALSE) )
ParHat = fit$obj$env$parList()
summary( fit )






#model

#SPATIAL version=======

#return to this after coding each region individually

#copied from Thorson et al 2024 to edit

#something seems to be missing maybe missed a () upstream

data(sea_otter)
Z = ts( sea_otter[,-1] )
# Specify model
sem = "
Pycno_CANNERY_DC -> log_Urchins_CANNERY_DC, 0, x2
log_Urchins_CANNERY_DC -> log_Kelp_CANNERY_DC, 0, x3
log_Otter_Count_CANNERY_DC -> log_Kelp_CANNERY_DC, 0, x4

Pycno_CANNERY_UC -> log_Urchins_CANNERY_UC, 0, x2
log_Urchins_CANNERY_UC -> log_Kelp_CANNERY_UC, 0, x3
log_Otter_Count_CANNERY_UC -> log_Kelp_CANNERY_UC, 0, x4

Pycno_HOPKINS_DC -> log_Urchins_HOPKINS_DC, 0, x2
log_Urchins_HOPKINS_DC -> log_Kelp_HOPKINS_DC, 0, x3
log_Otter_Count_HOPKINS_DC -> log_Kelp_HOPKINS_DC, 0, x4

Pycno_HOPKINS_UC -> log_Urchins_HOPKINS_UC, 0, x2
log_Urchins_HOPKINS_UC -> log_Kelp_HOPKINS_UC, 0, x3
log_Otter_Count_HOPKINS_UC -> log_Kelp_HOPKINS_UC, 0, x4

Pycno_LOVERS_DC -> log_Urchins_LOVERS_DC, 0, x2
log_Urchins_LOVERS_DC -> log_Kelp_LOVERS_DC, 0, x3
log_Otter_Count_LOVERS_DC -> log_Kelp_LOVERS_DC, 0, x4
Pycno_LOVERS_UC -> log_Urchins_LOVERS_UC, 0, x2
log_Urchins_LOVERS_UC -> log_Kelp_LOVERS_UC, 0, x3
log_Otter_Count_LOVERS_UC -> log_Kelp_LOVERS_UC, 0, x4
Pycno_MACABEE_DC -> log_Urchins_MACABEE_DC, 0, x2
log_Urchins_MACABEE_DC -> log_Kelp_MACABEE_DC, 0, x3
log_Otter_Count_MACABEE_DC -> log_Kelp_MACABEE_DC, 0, x4
Pycno_MACABEE_UC -> log_Urchins_MACABEE_UC, 0, x2
log_Urchins_MACABEE_UC -> log_Kelp_MACABEE_UC, 0, x3
log_Otter_Count_MACABEE_UC -> log_Kelp_MACABEE_UC, 0, x4
Pycno_OTTER_PT_DC -> log_Urchins_OTTER_PT_DC, 0, x2
log_Urchins_OTTER_PT_DC -> log_Kelp_OTTER_PT_DC, 0, x3
log_Otter_Count_OTTER_PT_DC -> log_Kelp_OTTER_PT_DC, 0, x4
Pycno_OTTER_PT_UC -> log_Urchins_OTTER_PT_UC, 0, x2
log_Urchins_OTTER_PT_UC -> log_Kelp_OTTER_PT_UC, 0, x3
log_Otter_Count_OTTER_PT_UC -> log_Kelp_OTTER_PT_UC, 0, x4
Pycno_PINOS_CEN -> log_Urchins_PINOS_CEN, 0, x2
log_Urchins_PINOS_CEN -> log_Kelp_PINOS_CEN, 0, x3
log_Otter_Count_PINOS_CEN -> log_Kelp_PINOS_CEN, 0, x4
Pycno_SIREN_CEN -> log_Urchins_SIREN_CEN, 0, x2
log_Urchins_SIREN_CEN -> log_Kelp_SIREN_CEN, 0, x3
log_Otter_Count_SIREN_CEN -> log_Kelp_SIREN_CEN, 0, x4
# AR1
Pycno_CANNERY_DC -> Pycno_CANNERY_DC, 1, ar1
log_Urchins_CANNERY_DC -> log_Urchins_CANNERY_DC, 1, ar2
log_Otter_Count_CANNERY_DC -> log_Otter_Count_CANNERY_DC, 1, ar3
log_Kelp_CANNERY_DC -> log_Kelp_CANNERY_DC, 1, ar4
Pycno_CANNERY_UC -> Pycno_CANNERY_UC, 1, ar1
log_Urchins_CANNERY_UC -> log_Urchins_CANNERY_UC, 1, ar2
log_Otter_Count_CANNERY_UC -> log_Otter_Count_CANNERY_UC, 1, ar3
log_Kelp_CANNERY_UC -> log_Kelp_CANNERY_UC, 1, ar4
Pycno_HOPKINS_DC -> Pycno_HOPKINS_DC, 1, ar1
log_Urchins_HOPKINS_DC -> log_Urchins_HOPKINS_DC, 1, ar2
log_Otter_Count_HOPKINS_DC -> log_Otter_Count_HOPKINS_DC, 1, ar3
log_Kelp_HOPKINS_DC -> log_Kelp_HOPKINS_DC, 1, ar4
Pycno_HOPKINS_UC -> Pycno_HOPKINS_UC, 1, ar1
15
log_Urchins_HOPKINS_UC -> log_Urchins_HOPKINS_UC, 1, ar2
log_Otter_Count_HOPKINS_UC -> log_Otter_Count_HOPKINS_UC, 1, ar3
log_Kelp_HOPKINS_UC -> log_Kelp_HOPKINS_UC, 1, ar4
Pycno_LOVERS_DC -> Pycno_LOVERS_DC, 1, ar1
log_Urchins_LOVERS_DC -> log_Urchins_LOVERS_DC, 1, ar2
log_Otter_Count_LOVERS_DC -> log_Otter_Count_LOVERS_DC, 1, ar3
log_Kelp_LOVERS_DC -> log_Kelp_LOVERS_DC, 1, ar4
Pycno_LOVERS_UC -> Pycno_LOVERS_UC, 1, ar1
log_Urchins_LOVERS_UC -> log_Urchins_LOVERS_UC, 1, ar2
log_Otter_Count_LOVERS_UC -> log_Otter_Count_LOVERS_UC, 1, ar3
log_Kelp_LOVERS_UC -> log_Kelp_LOVERS_UC, 1, ar4
Pycno_MACABEE_DC -> Pycno_MACABEE_DC, 1, ar1
log_Urchins_MACABEE_DC -> log_Urchins_MACABEE_DC, 1, ar2
log_Otter_Count_MACABEE_DC -> log_Otter_Count_MACABEE_DC, 1, ar3
log_Kelp_MACABEE_DC -> log_Kelp_MACABEE_DC, 1, ar4
Pycno_MACABEE_UC -> Pycno_MACABEE_UC, 1, ar1
log_Urchins_MACABEE_UC -> log_Urchins_MACABEE_UC, 1, ar2
log_Otter_Count_MACABEE_UC -> log_Otter_Count_MACABEE_UC, 1, ar3
log_Kelp_MACABEE_UC -> log_Kelp_MACABEE_UC, 1, ar4
Pycno_OTTER_PT_DC -> Pycno_OTTER_PT_DC, 1, ar1
log_Urchins_OTTER_PT_DC -> log_Urchins_OTTER_PT_DC, 1, ar2
log_Otter_Count_OTTER_PT_DC -> log_Otter_Count_OTTER_PT_DC, 1, ar3
log_Kelp_OTTER_PT_DC -> log_Kelp_OTTER_PT_DC, 1, ar4
Pycno_OTTER_PT_UC -> Pycno_OTTER_PT_UC, 1, ar1
log_Urchins_OTTER_PT_UC -> log_Urchins_OTTER_PT_UC, 1, ar2
log_Otter_Count_OTTER_PT_UC -> log_Otter_Count_OTTER_PT_UC, 1, ar3
log_Kelp_OTTER_PT_UC -> log_Kelp_OTTER_PT_UC, 1, ar4
Pycno_PINOS_CEN -> Pycno_PINOS_CEN, 1, ar1
log_Urchins_PINOS_CEN -> log_Urchins_PINOS_CEN, 1, ar2
log_Otter_Count_PINOS_CEN -> log_Otter_Count_PINOS_CEN, 1, ar3
log_Kelp_PINOS_CEN -> log_Kelp_PINOS_CEN, 1, ar4
Pycno_SIREN_CEN -> Pycno_SIREN_CEN, 1, ar1
log_Urchins_SIREN_CEN -> log_Urchins_SIREN_CEN, 1, ar2
log_Otter_Count_SIREN_CEN -> log_Otter_Count_SIREN_CEN, 1, ar3
log_Kelp_SIREN_CEN -> log_Kelp_SIREN_CEN, 1, ar4
"
# Fit model
fit = dsem( sem = sem,
tsdata = Z,
control = dsem_control(use_REML=FALSE, quiet=TRUE) )
# summary( fit )

