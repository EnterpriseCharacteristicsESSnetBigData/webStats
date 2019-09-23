#' @title URL Pre-processing For Web-scraping
#' 
#' @description  prepOuputSearcher is used to transform URLs into a adjusted character vector for web searching. A given Key characterize each vector. It can be seen as a pre-processing part for a web-scraping procedure.
#'
#' @param pathSearchRes file path to ouptus of JAVA procedure for searching URLs
#' @param file output file name, if specified ouptut be saved as .csv.
#' @param IDName character spezifying the ID column name
#' @param excludeURLs a character vector containing URLs or words. URLs from the JAVA output which contain these strings will automatically be discarded 
#' @param excludeSuffix URL suffixes to be exluded in the output, e.g. `excludeSuffix="org"` all URLs with suffix .org will automatically be discarded.
#'                      If `NULL` all suffixes are allowed.
#' 
#' @return data.table containing an ID, URLs, and position of search result
#' 
#' @export
#'
#' @examples
#' 
#'
#' 

prepOutputURLSearcher <- function(pathSearchRes,file=NULL,IDName="ID",excludeURLs=NULL,excludeSuffix=NULL){
  
  lf <- list.files(pathSearchRes)
  IDs <- gsub("\\.txt","",lf)
  URLs <- list()
  
  # read in data
  for(i in seq_along(IDs)){
    URLs[[IDs[i]]] <- fread(file.path(pathSearchRes,lf[i]),
                            colClasses="character",sep=NULL,
                            blank.lines.skip=TRUE,
                            header = FALSE)
  }
  
  URLs <- rbindlist(URLs,idcol = IDName)
  
  # parse output strings to get URLs without additioanl paths
  URLs <- URLs[,parseBasicUrl(V1,clean=excludeSuffix),by=c(IDName)]
  
  # remove duplicates per ID
  URLs[, drop:=duplicated(URL),by=c(IDName)]
  URLs <- URLs[drop==FALSE]
  
  # remove blacklisted URLs
  if(!is.null(excludeURLs)){
    excludePattern <- paste(excludeURLs,collapse="|")
    URLs[,drop:=grepl(excludePattern,URL)]
    URLs[,drop_all:=all(drop),by=c(IDName)]
    appendURLs <- URLs[drop_all==TRUE,.(URL=NA_character_,Position=NA_integer_),by=c(IDName)]
    URLs[,drop_all:=NULL]
    URLs <- URLs[drop==FALSE]
    URLs <- rbindlist(list(URLs,appendURLs),fill=TRUE,use.names=TRUE)
  }
  URLs[,drop:=NULL]
  
  # write output
  if(!is.null(file)){
    fwrite(URLs,file=file)
  }
  return(invisible(URLs))
  
}

# Funktion zum Herauslesen des BaseUrl (irgendo gibt es schon sowas, aber in welchen R Paket?!)
# 
parseBasicUrl <- function(urls, clean = c("org","gv.at","place","wien")){
  
  urls <- urltools::url_parse(urls)
  urlsExtract <- urltools::suffix_extract(urls$domain)
  setDT(urlsExtract)
  urlsKeep <- urlsExtract[,!(is.na(domain)|domain==""|suffix %in% clean)] & !is.na(urls[["scheme"]])
  
  output <- url_compose(urls[urlsKeep,])
  
  if(length(output)==0){
    return(data.table(URL=NA_character_,Position=NA_integer_))
  }
  position <- 1:length(output)
  
  # remove additional paths
  path(output) <- NULL # remove
  
  output <- data.table(URL=output,Position=position)
  
  # remove duplicate URLs ~ which have same domain
  urlsKeep <- !duplicated(urlsExtract[urlsKeep,"domain"])
  output <- output[urlsKeep,]
  
  return(output)
}

