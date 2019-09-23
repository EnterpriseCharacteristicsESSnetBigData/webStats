#' @title Run BING search via JAVA program
#' 
#' Specify file paths for inputs for JAVA program as well as output folder, log files and JAVA binaries and search queries through BING 
#'
#' @param IDInput path to input file holding IDs
#' @param SearchInput path to input file holding search strings
#' @param outputFolder path to output folder
#' @param logFiles path where log files should be written into
#' @param jarFile path to java program
#'
#' @return
#' @export
#'
#' @examples
searchURL <- function(IDInput,SearchInput,
                           outputFolder,logFiles=path.expand("~/essnet_wpc/URLSearcher/"),
                           jarFile=path.expand("~/essnet_wpc/URLSearcher/UrlSearcher.jar")){
  
  # building urlSearcherConf.properties
  cat("writing urlSearcherConf.properties\n")
  prop <- c(
    # the path of the 2 input files
    paste0("FIRM_NAMES_FILE_PATH=",path.expand(SearchInput)),
    paste0("FIRM_IDS_FILE_PATH=",path.expand(IDInput)),
    
    # the path of the folder within which the intermediate txt files used to produce the seed file will be created
    paste0("TXT_FILES_FOLDER_PATH=",path.expand(outputFolder)),

    # the path of the folder within which the produced seed file will be created
    paste0("SEED_FILE_FOLDER_PATH=",path.expand(logFiles)),
    
    # the fullpath of the log file that will be created
    paste0("LOG_FILE_PATH=",path.expand(logFiles),"logUrlSearcher.log")
  )
  writeLines(prop,con=file.path(logFiles,"urlSearcherConf.properties"))
  cat("Done!\n\n")
  
  # execute java program
  cat("Executing JAVA program through terminal\n")
  command <- paste0("java -jar -Xmx1024m ",jarFile," ",file.path(logFiles,"urlSearcherConf.properties"))
  cat(command,"\n")
  t <- Sys.time()
  system(command)
  cat("Done in:",format(Sys.time()-t))
}



