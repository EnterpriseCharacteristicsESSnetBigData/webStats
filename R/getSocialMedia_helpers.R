#' @title Get Social Media Links from html/xml code
#' 
#' Extract Social Media Links from html/xml code 
#'
#' @param htmlCode list containing html/xml code from a webpage, each list is a single page of the website
#' @param url string containing url from which html/xml code was scraped from
#' @param tag character specifying in which tag identifiers should be searched for, e.g `tag="body"` searches only inside `<body>...</body>`.
#' @param tagLinks character specifying in which tags links should be searched for.
#' @param domain character vector specifying domain of social media platforms
#' @param dropLink character vector specifying strings for which social media link should be droped.
#'
#' @return
#' @export
#'
#' @examples
#' 

getSocialMedia_work <- function(htmlCode,url,tag="body",tagLinks=c("a","base","link","area"),
                                domain,dropLink){
  
  # build preliminary output
  if(is.null(url)){
    url <- NA_character_
  }
  output <- data.table(URL=url)
  
  # check if error occured during scrapping
  gotError <- sapply(htmlCode,is,class2="try-error")
  htmlCode <-  htmlCode[!gotError]
  if(any(gotError)){
    output[["Error"]] <- TRUE
  }
  if(length(htmlCode)==0){
    return(output) # return if no information is left
  }
  
  # check if scrapping was not allowed
  disallowed <- lapply(htmlCode,`%in%`,c("disallowed","disallowed by robotstxt"))
  disallowed <- unlist(disallowed)
  if(any(disallowed)){
    output[["Disallowed"]] <- TRUE
    htmlCode <- htmlCode[!disallowed]
  }
  if(length(htmlCode)==0){
    return(output) # return if no information is left
  }
  
  # parse htmlCode Code
  htmlCode <- lapply(htmlCode,parseHTML,onlyText=FALSE,tag=tag)
  htmlCode <- htmlCode[!sapply(htmlCode,is.null)] 
  
  socialMediaLinks <- c()
  if(!is.null(htmlCode)&length(htmlCode)>0){
    
    # loop over different tags
    socialMediaLinks <- sapply(tagLinks,function(l){
      
      # loop over pages
      out <- sapply(htmlCode,getSMLinks,tagLinks=l,domain=domain,dropLink=dropLink,simplify=FALSE)
      out <- unique(unlist(out))
      return(out)
    },simplify = FALSE)
    socialMediaLinks <- unlist(socialMediaLinks)
  }
  if(length(socialMediaLinks)==0){
    socialMediaLinks <- NA_character_
  }
  
  output <- data.table(URL=url,SocialMediaLink=socialMediaLinks)
  return(output)
}


# helpfunction to get social media links
getSMLinks <- function(htmlCode,tagLinks,domain,dropLink){
  # extract social media links
  
  # search for tag
  extractedTags <- rvest::html_nodes(htmlCode, tagLinks)
  # get link in tag
  possibleLinks <- rvest::html_attr(extractedTags,"href")
  
  # get links which contain social media site in domain
  parsedLinks <- urltools::url_parse(possibleLinks)
  selectLinks <- grepl(domain,parsedLinks$domain)
  possibleLinks <- possibleLinks[selectLinks]
  
  # drop links which contain unwanted words like policy, plugins, terms, developer
  gotUnwantedAttrib <- grepl(dropLink,possibleLinks)
  # and which just contain the social media domain
  parsedLinks <- parsedLinks[selectLinks,c("port","path","parameter","fragment")] 
  onlyDomain <- rowSums(is.na(parsedLinks))==ncol(parsedLinks)
  
  extractedLink <- possibleLinks[(!gotUnwantedAttrib)&(!onlyDomain)]
  
  # return links
  return(extractedLink)
}

