#' @title Scrape multiple URLs
#' 
#' Scrape multiple URLs contained in a data.table. Scraped results are saved seperately as well as a lookupTable to link the scraped results to the original inputs.
#'
#' @param remDr an RSelenium remote driver created with `RSelenium::remoteDriver()`:
#' @param eCaps list containing extra options for chrome browser, directly used for parameter `extraCapabilities` in `RSelenium::remoteDriver()`
#' @param dat data.table with at least one column containing URLs.
#' @param urlCol column name of `dat` containing the URLs
#' @param outputPath folder where scraped results are saved
#' @param fileNames leading string of filenames of scraped results. Each url is only scraped once and saved in a file named `fileNames`digits where the digits are defined by the scraping order.
#' @param impressumLinks character vector containing words which will most likely appear in links, in the link itself or the text extracted from the inner html of a link-tag,  which point to the impressum. These links are always scrapped first.
#' @param onlyImpressum boolean, if `TRUE` only links which likely hold the impressum are scrapped, e.g. links containing words from `impressumLinks`
#' @param otherLinks character vector containing words which are searched for in sublinks and corresponding text. Links which contain parts of those words will always be scrapped.   
#' @param linkTags tags where links should be searched for
#' @param robots boolean, if `TRUE` the robots exclusion protocoll is considered when scraping
#' @param subLinkNumber numer of sublinks which will be scrapped
#' @param randomDelay numeric vector of length 2 defining `min` and `max` for random time delay while navigating to a new link. Time delay is generated using `runif(1,randomDelay[1],randomDelay[2])`.
#' @param linkDepth not yet implemented
#' @param verbose boolean, if `TRUE` links which are currently scrapped are printed
#' 
#'
#' @return
#' @export
#'
#' @examples
#' 

scrapeURL <- function(remDr,eCaps,dat,urlCol="URL",outputPath,
                      fileNames="WebData",
                      impressumLinks=c("contact","impressum","kontakt","imprint","legal-notice","legal","notice","disclaimer"),
                      otherLinks=NULL,onlyImpressum=FALSE,linkTags=c("a","base","link","area"),
                      robots=TRUE,subLinkNumber=25,
                      randomDelay=c(.3,1.5),linkDepth=1,verbose=FALSE){
  
  
  # prep data so
  # pages are scraped only once
  dat[,urlExists:=!is.na(get(urlCol))&!get(urlCol)==""]
  dat[urlExists==TRUE,GROUP:=.GRP,by=c(urlCol)]
  
  # randomly shuffle data
  setorder(dat,GROUP)
  urls <-  dat[!duplicated(get(urlCol))&urlExists==TRUE][[c(urlCol)]]
  # how to save results
  fileNames <- paste0(fileNames,dat[!duplicated(get(urlCol))&urlExists==TRUE,GROUP])
  
  
  # loop through urls
  for(i in seq_along(urls)){
    
    # scrape url
    if(verbose){
      cat("\nscraping URL:",urls[i])
    }
    outputScraper <- runScraper(remDr,url=urls[i],
                                impressumLinks=impressumLinks,
                                onlyImpressum=onlyImpressum,
                                otherLinks=otherLinks,
                                linkTags=linkTags,
                                robots=robots,
                                subLinkNumber=subLinkNumber,
                                randomDelay=randomDelay,
                                linkDepth=linkDepth,
                                verbose=verbose)
    if(verbose){
      cat("\nDONE!\n")
    }
    
    if(is.null(outputScraper)){
      outputScraper <- list(outputScraper)
    }
    if(is.null(names(outputScraper))){
      names(outputScraper) <- urls[i] 
    }
    
    errorOccured <- sapply(outputScraper,function(z){
      is(z, "try-error")
    })
    if(any(errorOccured)){
      # if error occured during scrapping
      # restart browser 
      # and skipp url for later treatment
      if(verbose){
        cat("\nRestarting Driver")
      }
      remDr <- startBrowser(port,eCaps=eCaps)
    }
    
    # save file
    saveRDS(outputScraper,file=file.path(outputPath,paste0(fileNames[i],".RDS")))
    
  }
  
  # save lookup table which corresponds to saved files
  dat[,File:=fileNames[GROUP]]
  dat[,c("GROUP","urlExists"):=NULL]
  saveRDS(dat,file=file.path(outputPath,"LookupTable.RDS"))
  
}

