#' @title Download the latest NFI
#' @descriptions Download the latest French NFI database
#' @details The raw downloaded data are stored inside the NFI_data/Raw_data folder
#' @return The path to where the data have been downloaded
#' @author J Borderieux (jeremy.borderieux@@agroparistech.fr)
#' @param dir
#' @param nfi_url The url
#' @note
#' @seealso  {\link[=get_NFI]{get_NFI}}
#' @references https://inventaire-forestier.ign.fr/
#' @examples \dontrun{
#' download_NFI(dir=getwd())
#' }
#' @export
download_NFI<-function(dir=getwd(),nfi_url=get_latest_url()){
  dir.create(file.path(dir,"NFI_data"),showWarnings = F)
  dir.create(file.path(dir,"NFI_data","Raw_data"),showWarnings = F)
  path_zip<-file.path(dir,"NFI_data","Raw_data","data_NFI.zip")
  download.file(nfi_url,path_zip)
  return(path_zip)

}
