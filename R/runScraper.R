#' @title Scrape URL with RSelenium
#' 
#' Scrapes a single url using an `Rselenium` remote driver. Links to impressum or contact information are especially considered.
#'
#' @param remDr an RSelenium remote driver created with `RSelenium::remoteDriver()`:
#' @param url a single url
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
#' @return
#' @export
#'
#' @examples
#' 

runScraper <- function(remDr,url,
                       impressumLinks=c("contact","impressum","kontakt","imprint","legal-notice","legal","notice","disclaimer"),
                       onlyImpressum=FALSE,otherLinks=NULL,
                       linkTags=c("a","base","link","area"),
                       robots=TRUE,subLinkNumber=25,
                       randomDelay=c(.3,1.5),linkDepth=1,verbose=FALSE){
  
  scrappedData <- list()
  
  if(length(impressumLinks)>1){
    impressumLinks <- paste(impressumLinks,collapse = "|")
  }
  if(length(otherLinks)>1){
    otherLinks <- paste(otherLinks,collapse="|")
  }
  
  ###############
  # check robots.txt
  rtxt <- try(robotstxt(url),silent=TRUE)
  if(robots&!is(rtxt, "try-error")){
    if(!rtxt$check()){
      scrappedData[[1]] <- "disallowed by robotstxt"
      scrappedData <- list(scrappedData)
      names(scrappedData) <- url
      return(scrappedData)
    }
  }
  
  ###############
  # scrape main page
  if(verbose){
    cat("\n",url," ... ")  
  }
  
  errOccured <- try(remDr$navigate(url),silent=TRUE)
  if(!is.null(errOccured)){
    return(errOccured)
  }
  
  # get current homepage - important to avoid http vs https ect...
  url <- try(remDr$getCurrentUrl()[[1]],silent=TRUE)
  if(is(url, "try-error")){
    return(url)
  }
  
  scrappedData[[url]] <- try(remDr$getPageSource(),silent=TRUE)
  if(is(scrappedData[[url]], "try-error")){
    return(scrappedData[[url]])
  }
  
  if(verbose){
    cat("scrapped!")
  }
  ###############
  ## get all links on main page
  links <- getLinks(remDr,url,linkTags=linkTags,
                    rtxt=rtxt,robots=robots)
  if(!is.list(links)){
    scrappedData <- list(scrappedData)
    names(scrappedData) <- url
    return(scrappedData)
  }
  linksHref <- links$linksHref
  linksLabels <- links$linksLabels
  
  
  ###############
  ## scrape links
  if(length(linksHref)>0){

    # get possible impressum links
    impressumFound <- rep(FALSE,length(linksHref))
    if(!is.null(impressumLinks)){
      impressumFound <- selectLinks(linksHref,linksLabels,links=impressumLinks)
    }

   
    # get other tags
    othersFound <- rep(FALSE,length(linksHref))
    if(!is.null(otherLinks)){
      othersFound <- selectLinks(linksHref,linksLabels,links=otherLinks)
    }
    
    # reorder links
    linksHref <- c(linksHref[impressumFound],
                   linksHref[impressumFound&othersFound],
                   linksHref[othersFound],
                   linksHref[!(impressumFound|othersFound)])
    
    if(onlyImpressum){
      linksHref <- linksHref[1:sum(impressumFound)]
    }else{
      # get impressum links and maximum subLinkNumber  
      linksHref <- linksHref[1:min(max(sum(impressumFound|othersFound),subLinkNumber),length(linksHref))]
    }

    # scrape sublinks
    for(j in seq_along(linksHref)){
      # go to url
      if(verbose){
        cat("\n",linksHref[j]," ... ")  
      }
      
      errOccured <- try(remDr$navigate(linksHref[j]),silent=TRUE)
      if(is(errOccured, "try-error")){
        scrappedData[[linksHref[j]]] <- errOccured
      }
      
      scrappedData[[linksHref[j]]] <- try(remDr$getPageSource(),silent=TRUE)
      
      # randomly weight
      Sys.sleep(runif(1,randomDelay[1],randomDelay[2]))
      if(verbose){
        cat("scrapped!")
      }
    }
  }else{
    scrappedData[[2]] <- "disallowed by robotstxt"
  }
  
  scrappedData <- list(scrappedData)
  names(scrappedData) <- url
  return(scrappedData)
}
