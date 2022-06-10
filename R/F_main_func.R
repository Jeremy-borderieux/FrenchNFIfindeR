#' Get the latest url, copy it here
#' @noRd
get_latest_url<-function()return("https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2005_2020.zip")



#' @title Download the latest NFI
#' @details The raw downloaded data are stored inside the NFI_data/Raw_data folder
#' @return The path to where the data have been downloaded
#' @author J Borderieux (jeremy.borderieux@@agroparistech.fr)
#' @param dir The path indicated where the raw NFI is gonna be stored
#' @param nfi_url The NFI download NFI, a default value is provided
#' @seealso  {\link[=get_NFI]{get_NFI}}
#' @references https://inventaire-forestier.ign.fr/
#' @examples \dontrun{
#' download_NFI(dir=getwd())
#' }
#' @export
download_NFI<-function(dir=getwd(),
                       nfi_url=get_latest_url()){
  dir.create(file.path(dir,"NFI_data"),showWarnings = F)
  dir.create(file.path(dir,"NFI_data","Raw_data"),showWarnings = F)
  path_zip<-file.path(dir,"NFI_data","Raw_data","data_NFI.zip")
  download.file(nfi_url,path_zip)
  return(path_zip)

}


#' @title Get and format the NFI
#' @details The raw downloaded data are stored inside the NFI_data/Raw_data folder
#' The NFI is seperated into different csv files or R data.table, that you can merge by idp
#' \itemize{
#' \item{**NFI_plot_info**}{ The metadata such as position, or time of measurment of a NFI plot, plot are identified with a unique ID called idp}
#' \item{**NFI_ecologie**}{ Informations regarding soil, understory cover observations}
#' \item{**NFI_arbres**}{ Living tree measurements and informations}
#' \item{**NFI_dendro**}{ Dendrologic variable computed with NFI_arbres}
#' \item{**NFI_cover**}{ Relative and absolute cover of every tree species recorded}
#' \item{**NFI_flora**}{ Florisitic surveys of the NFI plot}
#' }
#'
#' @return If \code{export_to_env}  is FALSE, a list of 6 data.table
#' @author J Borderieux (jeremy.borderieux@@agroparistech.fr)
#' @param dir The path indicated where the raw and formated NFI is gonna be stored
#' @param nfi_url The NFI download NFI, a default value is provided
#' @param write_csv Logical, should the NFI be return in .csv files in the NFI_data folder
#' @param export_to_env Logical, should the NFI be directly imported into the global environement
#' @param visit 1 to get the first NFI measure (default), 2 for the revisist or c(1,2) for both
#' @references https://inventaire-forestier.ign.fr/
#' @examples \dontrun{
#' get_NFI(dir=getwd(),write_csv=F,export_to_env=T,visit=1)
#' }
#' @export
get_NFI<-function(dir=getwd(),
                  nfi_url=get_latest_url(),
                  write_csv=F,
                  export_to_env=T,
                  visit=1){

  ## download
  path_data<-file.path(dir,"NFI_data")
  path_raw_data<-file.path(dir,"NFI_data","Raw_data")
  path_zip<-file.path(dir,"NFI_data","Raw_data","data_NFI.zip")
  already_NFI<-!length(list.files(path_raw_data))==0
  if(!already_NFI)  download_NFI(dir,nfi_url)
  if(already_NFI) {
    user_input<- trimws(as.character(readline(prompt="raw NFI data already here, type 1 to use them or 2 to download them again : ")))
    if(user_input=="2") download_NFI(dir,nfi_url)
    if(!user_input%in%c("1","2")) user_input<- readline(prompt="raw NFI data already here, type 1 to use them or 2 to download them again : ")

  }


  ## reading them
  NFI_ecologie<-data.table(read.table(unz(path_zip,"ECOLOGIE.csv"),sep=";",header=T,stringsAsFactors =F))
  NFI_plot_info<-data.table(read.table(unz(path_zip,"PLACETTE.csv"),sep=";",header=T,stringsAsFactors =F))
  NFI_arbres<-data.table(read.table(unz(path_zip,"ARBRE.csv"),sep=";",header=T,stringsAsFactors =F))
  NFI_cover<-data.table(read.table(unz(path_zip,"COUVERT.csv"),sep=";",header=T,stringsAsFactors =F))
  NFI_flora<-data.table(read.table(unz(path_zip,"FLORE.csv"),sep=";",header=T,stringsAsFactors =F))

  if( !any(c(visit==1 , visit==2 , visit==c(1,2)) ))stop("Need a valid visit code")

  colnames(NFI_ecologie)<-tolower(colnames(NFI_ecologie));colnames(NFI_ecologie)[1]<-"campagne";NFI_ecologie[,x:=NULL]
  colnames(NFI_plot_info)<-tolower(colnames(NFI_plot_info));colnames(NFI_plot_info)[1]<-"campagne";NFI_plot_info[,x:=NULL]
  colnames(NFI_arbres)<-tolower(colnames(NFI_arbres));colnames(NFI_arbres)[1]<-"campagne";NFI_arbres[,x:=NULL]
  colnames(NFI_cover)<-tolower(colnames(NFI_cover));colnames(NFI_cover)[1]<-"campagne";NFI_cover[,x:=NULL]
  colnames(NFI_flora)<-tolower(colnames(NFI_flora));colnames(NFI_flora)[1]<-"campagne";NFI_flora[,x:=NULL]

  NFI_plot_info<-NFI_plot_info[visite%in%visit,]

  canopy_cover<-NFI_cover[strate=="R",.(couverttot=sum(tcl),couverttot_abs=sum(tca)),by=idp]
  #
  #   NFI_plot_info[,gest_struct:=sfo]
  #   NFI_plot_info[,tmp:=switchv(as.character(sver),"0",`NA`="0",`0`="0",`X`="0",`2`="1",`3`="4",`4`="2",`5`="3",`6`="1")]
  #   NFI_plot_info$gest_struct[NFI_plot_info$campagne>2013]<-NFI_plot_info$tmp[NFI_plot_info$campagne>2013]
  #   NFI_plot_info[,gest_struct:=switchv(as.character(gest_struct),`NA`=NA,`0`="debois",`1`="FR",`2`="FIR",`3`="TSF",`4`="T")]
  #   NFI_plot_info[,gest_struct:=as.character(gest_struct)]
  #   NFI_plot_info[,tmp:=NULL]

  NFI_ecologie[,campagne:=NULL]

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
                              Ntot=sum(w)),  by = idp]


  NFI_dendro[, DgTotFinal:=sqrt(4*basal_area/(pi*Ntot))*100]


  ## getting the metada for taxonomic purposes
  meta_data<-data.table(read.table(unz(path_zip,"metadonnees.csv"),sep=";",skip=17,fill=T,quote="",row.names = NULL,encoding = "UTF-8",stringsAsFactors =F))
  colnames(meta_data)<-c("code","value","description","advanced_description")

  flora_taxo<-meta_data[code=="CDREF13",]
  colnames(flora_taxo)[3]<-c("species_name")
  flora_taxo[,French_vernacular_name:=stringr::str_detect(species_name," \\(.*")]
  flora_taxo[,French_vernacular_name:=ifelse(French_vernacular_name,stringr::str_extract_all(species_name," \\(.*",simplify = T),NA)]
  flora_taxo[,French_vernacular_name:=stringr::str_remove_all(French_vernacular_name," \\(")]
  flora_taxo[,French_vernacular_name:=stringr::str_remove_all(French_vernacular_name,"\\)")]
  flora_taxo[,species_name:=stringr::str_remove_all(species_name," \\(.*")]
  flora_taxo[,value:=as.numeric(value)]

  NFI_flora<-merge(NFI_flora,flora_taxo[,c("value","species_name")],by.x="cd_ref",by.y="value")



  tree_NFI_codetaxo<-data.table(read.table(unz(path_zip,"espar-cdref13.csv"),sep=";",quote="",skip=1,row.names = NULL,encoding = "UTF-8",stringsAsFactors =F))
  colnames(tree_NFI_codetaxo)<-c("espar","french_name","cd_ref","species_name")
  tree_NFI_codetaxo[,espar:=ifelse(nchar(espar)==1,paste0("0",espar),espar)]

  NFI_arbres<-merge(NFI_arbres,tree_NFI_codetaxo[,c("espar","species_name","french_name")],by="espar",all.x = T)

  NFI_plot_info<<-NFI_plot_info[order(idp),]
  NFI_ecologie<<-NFI_ecologie[order(idp),]
  NFI_arbres<<-NFI_arbres[order(idp),]
  NFI_dendro<<-NFI_dendro[order(idp),]
  NFI_cover<<-NFI_cover[order(idp),]
  NFI_flora<<-NFI_flora[order(idp),]

  if(write_csv){
    write.table(NFI_plot_info,file=file.path(path_data,"plot_info.csv"),sep=";",row.names = F)
    write.table(NFI_ecologie,file=file.path(path_data,"ecology.csv"),sep=";",row.names = F)
    write.table(NFI_arbres,file=file.path(path_data,"tree.csv"),sep=";",row.names = F)
    write.table(NFI_dendro,file=file.path(path_data,"dendro.csv"),sep=";",row.names = F)
    write.table(NFI_cover,file=file.path(path_data,"canopy_cover.csv"),sep=";",row.names = F)
    write.table(NFI_flora,file=file.path(path_data,"flora.csv"),sep=";",row.names = F)
  }

  if(export_to_env){
    NFI_plot_info<<-NFI_plot_info[order(idp),]
    NFI_ecologie<<-NFI_ecologie[order(idp),]
    NFI_arbres<<-NFI_arbres[order(idp),]
    NFI_dendro<<-NFI_dendro[order(idp),]
    NFI_cover<<-NFI_cover[order(idp),]
    NFI_flora<<-NFI_flora[order(idp),]
  }

  if(!export_to_env)return(list(NFI_plot_info,NFI_ecologie,NFI_arbres,NFI_dendro,NFI_cover,NFI_flora)) else return(NULL)

}



