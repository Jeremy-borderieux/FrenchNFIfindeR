####  New NFI data presentation, period 2005 - 2022 ####

#### Spatial data

SER_gis<-read_sf(file.path("Raw_data","GIS"),layer="ser_l93")

#### NFI data
NFI_ecologie<-data.table(read.table(unz(file.path("Raw_data","data_NFI_2005_2020","export_dataifn_2005_2020.zip"),"ECOLOGIE.csv"),sep=";",header=T))
NFI_plot_info<-data.table(read.table(unz(file.path("Raw_data","data_NFI_2005_2020","export_dataifn_2005_2020.zip"),"PLACETTE.csv"),sep=";",header=T))
NFI_arbres<-data.table(read.table(unz(file.path("Raw_data","data_NFI_2005_2020","export_dataifn_2005_2020.zip"),"ARBRE.csv"),sep=";",header=T))
NFI_cover<-data.table(read.table(unz(file.path("Raw_data","data_NFI_2005_2020","export_dataifn_2005_2020.zip"),"COUVERT.csv"),sep=";",header=T))

NFI_flore<-readRDS(file.path("Raw_data","data_NFI_2005_2020","harmonized_NFI_survey_2005_2020.RData"))
NFI_climate<-readRDS(file.path("Raw_data","data_NFI_2005_2020","climatic_database_NFI_2020.RData"))

colnames(NFI_ecologie)<-tolower(colnames(NFI_ecologie));colnames(NFI_ecologie)[1]<-"campagne";NFI_ecologie[,x:=NULL]
colnames(NFI_plot_info)<-tolower(colnames(NFI_plot_info));colnames(NFI_plot_info)[1]<-"campagne";NFI_plot_info[,x:=NULL]
colnames(NFI_arbres)<-tolower(colnames(NFI_arbres));colnames(NFI_arbres)[1]<-"campagne";NFI_arbres[,x:=NULL]
colnames(NFI_cover)<-tolower(colnames(NFI_cover));colnames(NFI_cover)[1]<-"campagne";NFI_cover[,x:=NULL]

NFI_plot_info<-NFI_plot_info[visite==1,]
NFI_plot_info[,campagne_2 := floor(idp/100000)+2005]

precise_elevation<-fread(file.path("Raw_data","altitudes_exactes.csv"))
precise_elevation[,idp:=as.numeric(idp)]
colnames(precise_elevation)<-c("idp","alti")
canopy_cover<-NFI_cover[strate=="R",.(couverttot=sum(tcl),couverttot_abs=sum(tca)),by=idp]

NFI_ecologie[,campagne:=NULL]

# pas de sfo en 2005 mais pas grave , analyse rege commencent en 2006
#regroupement des modalités de gestion pour traiter
NFI_plot_info[,gest_struct:=sfo]
NFI_plot_info[,tmp:=switchv(as.character(sver),"0",`NA`="0",`0`="0",`X`="0",`2`="1",`3`="4",`4`="2",`5`="3",`6`="1")]
NFI_plot_info$gest_struct[NFI_plot_info$campagne>2013]<-NFI_plot_info$tmp[NFI_plot_info$campagne>2013]
NFI_plot_info[,gest_struct:=switchv(as.character(gest_struct),`NA`=NA,`0`="debois",`1`="FR",`2`="FIR",`3`="TSF",`4`="T")]
NFI_plot_info[,gest_struct:=as.character(gest_struct)]

# #changer la vairable dc, les 0 sont codés en NA en 2006
# placette[is.na(dc)&campagne==2006,"dc"]<-0
#
# # ajout d'un facteur coupe recente/pas de coupes recente (peut etre amelioré en différentes intensités)
# placette[,dc_fact:=factor(as.character(dc!=0),levels = c("FALSE","TRUE"),labels=c("no_coupe_5y","coupe_5y"))]
# placette[,dc_bool:=as.numeric(dc!=0)]
# placette[,plaine_mont:=ifelse(greco%in% c("D","E","G","H","I","K"),"Montagne","Plaine")]
#





# setkey(arbresVivants, "espar");
# arbresVivants <- droplevels(arbresVivants[c("2", "3", "4", "5", "6", "7", "9"), espar:=paste(0,espar,sep="")])

# Imputation aux arbres simplifi?s de la valeur moyenne des mesures des arbres de la m?me placette, essences, et classe de taille
NFI_arbres[, dimess := cut(c13, breaks = c(0, 70.5, 117.5, 164.5, 1000), labels = c("PB", "BM", "GB", "TGB"), right = FALSE)]
NFI_arbres[, htot := ifelse(is.na(htot), mean(htot, na.rm = TRUE), htot), by = c("idp", "espar", "dimess")]
NFI_arbres[, ir5 := ifelse(is.na(ir5), mean(ir5, na.rm = TRUE), ir5), by = c("idp", "espar", "dimess")]

NFI_arbres<-NFI_arbres[veget=="0",]


NFI_arbres[,campagne_2 := floor(idp/100000)+2005]
# Definition d'une classe de sous-?tage
NFI_arbres[,strate:=ifelse(htot>0.5*max(htot, na.rm = T), "etagePrincipal", "sousEtage"),keyby=idp]


### Calcul des surfaces terri?res
NFI_arbres[,gFinal:=c13*c13*w/(4*pi)]
NFI_arbres[,gInitial:=pi/10000*(c13/(2*pi)-ir5/10)^2*w]

### Valeurs dendro ?chelle placette
NFI_dendro <- NFI_arbres[,.(basal_area=sum(gFinal),
                           Ntot=sum(w),
                           Gupper=sum(ifelse(strate=="etagePrincipal",gFinal,0)),
                           Nupper=sum(ifelse(strate=="etagePrincipal",w,0))),  by = idp]

NFI_dendro[, DgUpperFinal:=sqrt(4*Gupper/(pi*Nupper))*100]
NFI_dendro[, DgTotFinal:=sqrt(4*basal_area/(pi*Ntot))*100]





# verif strange coords nfi
#
# test_coords<-merge(NFI_plot_info[,c("idp","campagne","ser","greco","xl","yl")],placette[,c("idp","campagne","xl93_blurred","yl93_blurred")],by="idp",all.x=T,all.y=T)
# test_coords[,same_x:=signif(xl)==signif(xl93_blurred)]
# test_coords[,same_y:=signif(yl)==signif(yl93_blurred)]
#
# test_coords[,x_dif:=(xl)-(xl93_blurred)]
# test_coords[,y_dif:=(yl)-(yl93_blurred)]
# test_coords[,y_dif:=floor(y_dif)]
# test_coords[,x_dif:=floor(x_dif)]
# test_coords[,x_dif_table:=ifelse(x_dif>900,"more_than_100m",ifelse(x_dif< -900,"more_than_-100m","Coord_ok"))]
# test_coords[,y_dif_table:=ifelse(x_dif>900,"more_than_1000m",ifelse(y_dif< -900,"more_than_-1000m","Coord_ok"))]
#
# table(test_coords$x_dif_table,test_coords$campagne)
# table(test_coords$y_dif_table,test_coords$campagne)
# table(y=test_coords$y_dif_table,x=test_coords$x_dif_table)
#
# write.table(table(test_coords$y_dif_table,test_coords$campagne),"ifn_campagne.csv",sep=";")
# table((test_coords$y_dif_table))
# hist(test_coords$y_dif)
#
# head(test_coords$yl93_blurred)
#
# table(NFI_plot_info$tpespar2)
#
# View(test_coords[ y_dif_table=="more_than_-100m",])
#

# table(camp_ign=NFI_plot_info$campagne,camp_idp=NFI_plot_info$campagne_2)
# sum(NFI_arbres$idp%in%NFI_flore$idp)




