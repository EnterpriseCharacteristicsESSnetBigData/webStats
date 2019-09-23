######################################
# helper functions

# help-function to start or restart browser
# if some sort of disconnect occures
startBrowser <- function(port,remDr=NULL,eCaps=list()){
  
  if(!is.null(remDr)){
    remDr$close()
  }
  
  remDr <- remoteDriver(browserName = "chrome", port = port, 
                        extraCapabilities = eCaps)
  remDr$open()
  
  return(remDr)
}

# help function to extract links from a page or subpage
getElements <- function(remDr,tagNames = c("a","base","link","area")){
  tagNames <- c("a","base","link","area")
  links <- list()
  for(tn in tagNames){
    
    # get possible links
    links_tn <- try(remDr$findElements(using="tag name",tn),silent=TRUE)
    if(is(links_tn, "try-error")){
      links_tn <- NULL
    }
    links <- c(links,links_tn)
  }
  return(links)
}


# helpfunction to extract links from tags
extractLinks <- function(tags){
  
  linksHref <- linksLabels <- c()
  for(l in 1:length(tags)){
    # get href
    href <- try(tags[[l]]$getElementAttribute('href'),silent=TRUE)
    if(!is(href, "try-error")&length(href)>0&isTRUE(href!="")){
      linksHref <- c(linksHref,href[[1]])
      
      # get text from between tags
      tagText <- try(tags[[l]]$getElementText(),silent=TRUE)
      if(is(tagText, "try-error")|length(tagText)==0){
        tagText <- ""
      }
      linksLabels <- c(linksLabels,tagText[[1]])
    }
  }
  
  dropDup <- !duplicated(linksHref)
  linksLabels <- linksLabels[dropDup]
  linksHref <- linksHref[dropDup]
  
  return(list(linksHref=linksHref,linksLabels=linksLabels))
}


# helpfunction to check links for
# remove anker points
# remove links which point to other homepages
# against robots.txt
checkLinks <- function(linksHref,url){
  
  urlParsed <- urltools::url_parse(url)
  linksParsed <- urltools::url_parse(linksHref)
  linksExtract <- urltools::suffix_extract(linksParsed$domain)
  
  # same domain as url
  sameDomain <- linksParsed$domain==urlParsed$domain
  
  # spezific file endings allowed
  # no png, css, ...
  noFile <- (!grepl("\\.",linksParsed$path))|(grepl("(\\.shtm|\\.htm|\\.dhtm|\\.xhtm|\\.php)",linksParsed$path))
  
  # remove fragments 
  nofragment <- is.na(linksParsed$fragment)
  
  # identical sub paths
  subPath <- linksParsed$path
  subPath <- !is.na(subPath) & !duplicated(gsub("/$","",subPath))
  
  # rebuild URLs
  # this removes anker points
  linksSelect <- sameDomain & noFile & nofragment & subPath
  
  return(linksSelect)
}


# helpfunction to get links on page or subpage
getLinks <- function(remDr,url,linkTags=c("a","base","link","area"),
                     rtxt=NULL,robots=TRUE){
  
  # get tags which can contain links
  tags <- getElements(remDr,tagNames=linkTags)
  
  # if no such tags are found return NULL
  if(length(tags)==0){
    return(NULL)
  }
  
  # otherwise extract links from tags
  links <- extractLinks(tags)
  linksHref <- links$linksHref
  linksLabels <- links$linksLabels
  
  # check links for validity
  # remove anker points
  # remove links which point to other homepages
  # ..
  linksSelect <- checkLinks(linksHref,url=url)
  
  linksHref <- linksHref[linksSelect]
  linksLabels <- linksLabels[linksSelect]
  
  # if no valid sublinks available
  # skip URL otherwise proceed
  if(length(linksHref)==0){
    return(NULL)
  }
  
  # check remaining links against robots.txt
  if(!is(rtxt, "try-error")&robots){
    allowed <- sapply(linksHref, function(x) {
      rtxt$check(paths = x, bot = "*")} 
    )
    linksHref <- linksHref[allowed]
    linksLabels <- linksLabels[allowed]
  }

  
  # if no valid sublinks available
  # return not allowed
  if(length(linksHref)==0){
    return("disallowed by robotstxt")
  }else{
    
    # else return links
    return(list(linksHref=linksHref,linksLabels=linksLabels))
  }
}


# help function to get links to impressum
selectLinks <- function(linksHref,linksLabels,links){
  
  inlink <- grepl(links,linksHref,ignore.case = TRUE)
  inlabel <- grepl(links,linksLabels,ignore.case = TRUE)
  impressumFound <- inlink|inlabel
  
  return(impressumFound)
  
}



