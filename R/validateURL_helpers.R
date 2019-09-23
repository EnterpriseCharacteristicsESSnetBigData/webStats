#' @title Extract identifiers from html/xml code
#' 
#' Extract direct identifiers like VAT or commercial register number or search for names and adresses in html/xml code 
#'
#' @param htmlCode list containing html/xml code from a webpage, each list is a single page of the website
#' @param url string containing url from which html/xml code was scraped from
#' @param tag character specifying in which tag identifiers should be searched for, e.g `tag="body"` searches only inside `<body>...</body>`.
#' @param checkID list of characters vectors containing patterns which indicate text parts in which unique identifiers might be present  
#' @param patternID list of character vectors containing pattern of unique identifier which is searched for in text snippets resulting from applying patterns in checkID
#' @param matchNames character containing enterprise name and/or adress or patterns thereof which are searched for in the text of the html/xml code
#'
#' @return
#' @export
#'
#' @examples
#' 

extractID <- function(htmlCode,url=NULL,tag="body",checkID=NULL,patternID=NULL,matchNames=NULL){
  
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
  
  # get text from html code
  htmlCode <- lapply(htmlCode,parseHTML,onlyText=TRUE,tag=tag)
  
  if(!is.null(htmlCode)){
    
    foundID <- foundMatch <- NULL
    # extract specific identifiers
    if(!is.null(checkID)&!is.null(patternID)){
      foundID <- lapply(htmlCode,function(text){
        
        out <- mapply(FUN=extractID_work,checkID,patternID,MoreArgs = list(text=text),SIMPLIFY = FALSE)
        
        out <- lapply(out,null2NA)
        if(length(out)>0){
          fillNA <- max(lengths(out))
          out <- lapply(out,`length<-`,fillNA)
        }
        as.data.table(out)
      })
      foundID <- rbindlist(foundID,use.names=TRUE,fill=TRUE)
      foundID <- unique(foundID)
      
      NArows <- rowSums(is.na(foundID))==ncol(foundID)
      if(all(NArows)){
        foundID <- head(foundID,1)
      }else{
        foundID <- foundID[!NArows,]
      }
    }
    
    
    # search for specific Identifiers
    if(!is.null(matchNames)){
      foundMatch <- sapply(matchNames,function(IDpatterns){

        sapply(htmlCode,function(text){
          all(unlist(sapply(IDpatterns,grepl,x=text,ignore.case=TRUE)))
        })
        
      })
      if(!is.null(dim(foundMatch))){
        foundMatch <- colSums(foundMatch)
      }
      foundMatch <- names(matchNames[foundMatch>0])
      if(length(foundMatch)==0){
        foundMatch <- NA_character_
      }
      foundMatch <- data.table(foundMatch=foundMatch)
    }
    
    
    # bring output to same length for cbind
    if(!is.null(foundMatch)&!is.null(foundID)){
      maxRows <- max(nrow(foundMatch),nrow(foundID))
      foundMatch <- addNARows(foundMatch,maxRows-nrow(foundMatch))
      foundID <- addNARows(foundID,maxRows-nrow(foundID))
    }

    output <- do.call(cbind,list(output,foundMatch,foundID))

  }
  
  return(output)
  
}

createExtractPattern <- function(pattern,nmin,nmax){
  
  pattern <- tolower(pattern)
  paste0("(?=(",pattern,".{",nmin,",",nmax,"}))")
}

null2NA <- function(x){
  
  if(is.null(x)){
    return(NA_character_)
  }else{
    return(x)
  }
}

addNARows <- function(x,n=0){
  xNA <- data.table(matrix(NA_character_,ncol=ncol(x),nrow=n))
  x <- rbindlist(list(x,xNA),use.names=FALSE)
  return(x)
}

#' @title Extract text from html/xml code
#'
#' Helpfunction to extract text frin html/xml. If specified text will be extracted from a certain tag only and also ignored inside specific tags 
#' 
#' @param x string containing html or xml code
#' @param tag character specifying a certain tag from which text should be extracted, for instance `tag="body"` for all text inside `<body>...</body>`.  
#' @param rmNodes tag or nodes which should be excluded from the html or xml code, `rmNodes="script"` will remove all `<script>...</script>` and childnodes inside.
#'
#' @return
#' @export
#'
#' @examples
parseHTML <- function(x,onlyText=TRUE,tag=NULL,rmNodes=c("script","style","noscript"),
                      options="HUGE"){

  xOutput <- tryCatch({
    # read web page
    xml2::read_html(unlist(x),options = options)
  },error=function(cond){
    NULL
  })
  
  # if error occurs return NULL
  if(is.null(xOutput)){
    return(xOutput)
  }
  
  # extract tag if tag is specified
  if(!is.null(tag)){
    xOutput <- rvest::html_nodes(xOutput,tag)
  }
  
  if(!is.null(rmNodes)){
    # remove specific nodes
    xpathExpr <- paste0('ancestor::',rmNodes,' or name()="',rmNodes,'"')
    xpathExpr <- paste(xpathExpr,collapse=" or ")
    xpathExpr <- paste0('//*[not(',xpathExpr,')]')
    if(onlyText){
      xpathExpr <- paste0(xpathExpr,'/text()')
    }
    xOutput <- rvest::html_nodes(xOutput,xpath=xpathExpr)
  }

  if(onlyText){
    xOutput <- rvest::html_text(xOutput,trim=TRUE)
    xOutput <- paste(xOutput,collapse="")
  }
  
  return(xOutput)
}


# helpfunction to use substring function with mapply
getSubstring <- function(start,length,x){
  substr(x,start,start+length)
}

# helpfunction to extract regex patterns
# supports overlapping patterns
regCapturedMatches <- function(pattern,text,perl=TRUE){
  
  m <- gregexpr(pattern, text,perl=perl)
  
  startMatches <- as.vector(sapply(m, function(x) {attr(x, "capture.start")}))
  lengthMatches <- as.vector(sapply(m, function(x) {attr(x, "capture.length")}))
  
  if(is.null(startMatches)|is.null(lengthMatches)){
    return(character(0))
  }
  
  output <- mapply(getSubstring,start=startMatches,length=lengthMatches,MoreArgs=list(x=text))
  return(output)
}

# help function to extract specific identifiers
extractID_work <- function(checkForID,patternForID="(ATU[0-9]{8})|(?<!\\d)[0-9]{8}(?!\\d)",text){
  
  text <- gsub("\\s","",tolower(text))
  
  textSub <- sapply(checkForID,regCapturedMatches,text=text,simplify = TRUE)
  textSub <- unlist(textSub)
  textSub <- stringr::str_extract_all(textSub,patternForID)
  textSub <- unique(unlist(textSub))
  
  if(length(textSub)>0){
    return(textSub)
  }else{
    return(NULL)
  }
}


# helpfunction to prepare searching for names
getSearchNames <- function(dat,lookupTable,
                            nameCols=c("NAME","RFS_PLZ|RFS_ORT","RFS_STRASSE","RFS_HNR"),
                            removeChar=NULL,replSpaces=NULL,
                            IDcol="KZ_Z",fileCol="File"){
  
  
  # load lookup table
  if(is.character(lookupTable)){
    if(tools::file_ext(lookupTable)=="RDS"){
      lookupTable <- readRDS(lookupTable)
    }else if(tools::file_ext(lookupTable)=="csv"){
      lookupTable <- fread(lookupTable)
    }else{
      stop("lookupTable must be stored as .RDS or .csv")
    }
  }else{
    lookupTable <- data.table(lookupTable)
  }
  
  # prep colnames
  prepNames <- grepl("\\|",nameCols)
  nameCols_help <- strsplit(nameCols[prepNames],"\\|")
  nameCols <- nameCols[!prepNames]
  
  # makeNewNames
  for(i in seq_along(nameCols_help)){
    newName <- paste(nameCols_help[[i]],collapse = "|")
    dat[,c(newName):=do.call(paste,args=c(.SD,sep="|")),.SDcols=c(nameCols_help[[i]])]
    nameCols <- c(nameCols,newName)
  }
  
  datNames <- dat[,mget(c(IDcol,nameCols))]
  
  # remove character
  if(!is.null(removeChar)){
    removeChar <- paste(removeChar,collapse="|")
    datNames[,c(nameCols):=lapply(.SD,function(z){
      z <- gsub(removeChar,"",z)
      return(z)
    }),.SDcols=c(nameCols)]
  }
  
  # replaces spaces
  if(!is.null(replSpaces)){
    datNames[,c(nameCols):=lapply(.SD,function(z){
      z <- gsub("\\s+",".*",z)
      return(z)
    }),.SDcols=c(nameCols)]
  }
  
  # combine data
  datNames <- merge(datNames,lookupTable,by=IDcol)
  
  keepNames <- c(nameCols,fileCol,IDcol)
  datNames <- datNames[,mget(keepNames)]
  
  # return
  return(datNames)
  
}


# helpfunction to merge output of validateURL() with data set
mergeIdentifier <- function(dat,outputValidate,
                            datCol,validateCol,
                            urlCol="URL"){
  
  datMerge <- list()
  for(i in 1:length(datCol)){
    datIndex <- outputValidate[get(validateCol[i])!=""&!is.na(get(validateCol[i])),
                               mget(c(urlCol,validateCol[i]))]
    datIndex <- unique(datIndex)
    setnames(datIndex,validateCol[i],datCol[i])
    datIndex[,c(datCol[i]):=as.character(get(datCol[i]))]
    dat[,c(datCol[i]):=as.character(get(datCol[i]))]
    
    datMerge_i <- dat[datIndex,,
                      on=c(datCol[i]),
                      nomatch=NULL]
    setcolorder(datMerge_i,colnames(dat))
    datMerge <- c(datMerge,list(datMerge_i))
  }
  
  datMerge <- rbindlist(datMerge)
  datMerge <- unique(datMerge)
  datMerge <- datMerge[dat,,on=c(colnames(dat))]
  
  return(datMerge)
}


