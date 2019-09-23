#' Title
#'
#' @param files 
#' @param fileNames 
#' @param tag 
#' @param tagLinks
#' @param domain 
#' @param dropLink 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples

getSocialMedia <- function(files,fileNames=NULL,tag="body",tagLinks=c("a","base","link","area"),
                           domain,dropLink,
                           verbose=TRUE,colFile="File"){
  
  filePaths <- list.files(files,full.names=TRUE)
  if(length(filePaths)==0){
    filePaths <- files
  }
  if(!is.null(fileNames)){
    filePaths <- filePaths[grepl(fileNames,filePaths)]
  }

  fileNames <- gsub(".*/","",filePaths)
  fileNames <- tools::file_path_sans_ext(fileNames)
  
  # paste inputs together to use as regular expressions
  domain <- paste(domain,collapse="|")
  dropLink <- paste(dropLink,collapse="|")
  
  # initialize output 
  output <- list()
  for(i in 1:length(fileNames)){
    
    htmlCode <- readRDS(filePaths[i])
    
    output_i <- getSocialMedia_work(htmlCode=htmlCode[[1]],
                          url=names(htmlCode),
                          tag=tag,tagLinks=tagLinks,
                          domain=domain,
                          dropLink=dropLink)
    
    output_i[,c(colFile):=fileNames[i]]
    output[[i]] <- output_i
    
    if(verbose&i%%10==0){
      cat("File Number: ",i,"\n")
    }
  }
  
  # build final output
  output <- rbindlist(output,use.names=TRUE,fill=TRUE)
  return(output)
}
