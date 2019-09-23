#' @title Function to identify/validate websites
#' 
#' Scrapped data from websites is loaded and search for unique identifiers or company names and adresses.
#' 
#' @param pathToFiles 
#' @param tag 
#' @param UnitName 
#' @param checkID 
#' @param patternID 
#' @param datID 
#' @param IDvars 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
#' 

validateURL <- function(files,fileNames=NULL,tag="body",checkID=NULL,patternID=NULL,
                        matchNames=NULL,IDcol=NULL,verbose=TRUE){
  
  filePaths <- list.files(files,full.names=TRUE)
  if(length(filePaths)==0){
    filePaths <- files
  }
  if(!is.null(fileNames)){
    filePaths <- filePaths[grepl(fileNames,filePaths)]
  }

  fileNames <- gsub(".*/","",filePaths)
  fileNames <- tools::file_path_sans_ext(fileNames)
  
  if(!is.null(matchNames)){
    foundCol <- sapply(matchNames,function(z){
      any(z%in%fileNames)
    })
    colNames <- names(foundCol[!foundCol])
    colNames <- colNames[colNames!=IDcol]
    colFile <- names(foundCol[foundCol])
    setkeyv(matchNames,colFile)
  }else{
    colFile <- "File"
  }
  
  # initialize output 
  output <- list()
  for(i in 1:length(fileNames)){
    
    htmlCode <- readRDS(filePaths[i])
    
    matchNames_i <- NULL
    if(!is.null(matchNames)){
      matchNames_i <- list()
      IDs <- matchNames[.(fileNames[i]),][[IDcol]]
      matchNames_i[IDs] <- transpose(matchNames[.(fileNames[i]),mget(colNames)])
    }
    
    
    output_i <- extractID(htmlCode=htmlCode[[1]],
                          url=names(htmlCode),tag=tag,
                          checkID=checkID,
                          patternID=patternID,
                          matchNames=matchNames_i)
    
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
