#' @title Text Pre-processing For Web-scraping
#' 
#' @description Creats the input data files necessary to run the JAVA application for URL searching. 
#' 
#' 
#' @param dat dataframe or data.table containing an id column and furhter columns from which to build the search string for the BING.
#' @param id column name in `dat` spezifing the observation identifier.
#' @param searchCols a character vector of column names in `dat` which will be pasted together using `paste(...,sep=" ")` to produce the search string for BING. The columns will be pasted together from left to right.
#' @param path file path where output files are written into
#' @param fileNameID filename where identifier for search strings are written into
#' @param fileNameInput filename where search strings are written into
#' @param pattern optional character vector containing regular expressions which will be removed for a single colum specified in `cleanCol`.
#' @param cleanCol column name in `dat` for which the regular expressions in `pattern` will be removed before building the search string.
#'
#' @return create two separat files for the id and the compromised output dataset. 
#'
#' @examples
#' 

makeInputURLSearcher <- function(dat, id, searchCols, path=getwd(),fileNameID="kzz.txt", fileNameInput = "input.txt",
                            pattern = NULL,cleanCol=NULL){
  
  if(!is.null(pattern)&&!is.character(pattern)){
    stop("pattern needs to be a character vector")
  }
  
  if( !is.character(id) | !is.character(searchCols) | !any((class(dat) %in% c("data.frame","data.table")))){
    
    stop("At least one parameter has an incorrect data type!")
    
  }
  if(!all(( c(id,searchCols) %in% colnames(dat)))){
    
    stop("The selected columns do not exist in dat")
    
  }
  if(!is.null(cleanCol)&&!cleanCol%in%searchCols){
    warning(cleanCol,"not in\n",searchCols,"\nVariable cleanCol will be ignored")
  }
  
  setDT(dat)
  
  # build search string
  # clean specific column
  if(!is.null(cleanCol)&!is.null(pattern)){
    for(i in 1:length(pattern)){
      dat[,c(cleanCol):=gsub(pattern[i],"",get(cleanCol))]
    }
  }
  prepInputText <- dat[,do.call(paste,.SD), .SDcols=c(searchCols)]

  # build id
  id <- as.character(dat[,get(id)])
  
  # write files
  cat("Writing files to:",path,"\n")
  writeLines(prepInputText,con=file.path(path,fileNameInput))
  writeLines(id,con=file.path(path,fileNameID))
  cat("Finished!")
}
