#' Create an SFTP connection to Bloomberg Data License
#' 
#' An sftp connection to Bloomberg Datalicense is established. On some Linux systems, this may
#' not work out of the box, as libcurl does not natively support sftp. In that case, you need to 
#' compile curl with SFTP support. 
#' See here for details: http://askubuntu.com/questions/195545/how-to-enable-sftp-support-in-curl
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password assigned by Bloomberg 
#' @param host The connection host
#' @param port The connection port
#' @param protocol The connection protocol
#' @param verbose logical. Should R report extra information on progress?
#' 
#' @return logical. Is the connection succesful?
#' 
#' @examples 
#' \dontrun{
#' # These are dummy credentials. Replace with the credentials received from Bloomberg 
#' RblConnect(user = 'dl000000', pw = '0000000000000000') 
#' }
#' 
#' @import RCurl
#' 
#' @export
#' 
RblConnect <- function(user, pw, host = 'sftp.bloomberg.com', port = '22', protocol = 'sftp', verbose = TRUE) {
    
    # ftp://User:Password@FTPServer/Destination.html
    url <- paste0(protocol, '://', user, ':', pw, '@', host, ':', port)
    
    # test connection
    if( class(try(getURL(url), silent = T)) == 'try-error' ){ 
      if(verbose) cat('Authentication failure: check your configurations and whitelist the IP address in use. Contact Bloomberg support for help.')
      return(FALSE)
    }
      
    # set credentials
    options(RblUrl = url, RblUser = user)
    return(TRUE)
    
}


#' Get Bloomberg connection Url
#' 
#' Retrieve the Bloomberg connection Url.
#' 
#' @return Url string
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' RblUrl()
#' }
#' 
#' @export
RblUrl <- function(){
  
  if(is.null(x <- getOption('RblUrl')))
    stop('Url not found. Run RblConnect first.')
  
  return(x)
  
}


#' Get Bloomberg User
#' 
#' Retrieve the Bloomberg User. 
#' 
#' @return User string
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' RblUser()
#' }
#' 
#' @export
RblUser <- function(){
  
  if(is.null(x <- getOption('RblUser')))
    stop('User not found. Run RblConnect first.')
  
  return(x)
  
}


#' List files available at Bloomberg
#' 
#' Retrieve files available at Bloomberg. 
#' 
#' @return Vector of character strings representing the filenames available at Bloomberg
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' RblFiles()
#' }
#' 
#' @import RCurl
#' 
#' @export
RblFiles <- function(){
  
  f <- getURL(RblUrl(), dirlistonly = F)
  f <- strsplit(f, "\r*\n")[[1]]
  
  names <- sapply(f, function(x){strsplit(x, '\\s+')[[1]][9]}, USE.NAMES = FALSE)
  types <- sapply(f, function(x){strsplit(x, '\\s+')[[1]][1]}, USE.NAMES = FALSE)
  
  return(names[!startsWith(x = types, prefix = 'd')])
  
}



#' Build a request file to query Bloomberg
#' 
#' The request file is generated according to Bloomberg Data License Documentation.
#' Refer to the Documentation to build the request.
#' 
#' @param header named vector of headers. E.g. c(FIRMNAME = RblUser(), PROGRAMNAME = 'getdata')
#' @param fields vector of Bloomberg fields. E.g. c('PX_LAST', 'PX_OPEN', 'PX_HIGH', 'PX_LOW')
#' @param identifiers vector of Bloomberg identifiers. E.g. c('SXXE Index', 'SX5E Index') 
#' @param overrides named vector of Bloomberg overrides. E.g. c('END_DT' = '20100101')
#' 
#' @return character string representing the request file. Upload it to query Bloomberg (see \code{\link{RblUpload}})
#'  
#' @examples
#' \dontrun{
#' # Run RblConnect first
#' 
#' # Build a request file to download the daily closing prices of 
#' #  EURO STOXX Index from 2005-01-01 to 2015-12-31.
#' RblRequest <- 
#'      RblRequestBuilder(
#'           header = c(FIRMNAME = RblUser(), 
#'                      PROGRAMNAME = 'gethistory', 
#'                      DATERANGE = '20050101|20151231'),
#'           fields = c('PX_LAST'), 
#'           identifiers = c('SXXE Index')
#'      )
#' RblRequest
#' }
#' 
#' @export
#' 
RblRequestBuilder <- function(header, fields, identifiers, overrides = c()) {
  
  #start of file
  req <- 'START-OF-FILE'
  
  #header
  for (key in names(header)) req <- paste0(req, '\n', key, '=', header[[key]])
  req <- paste0(req, '\n')
  
  #start of fields
  req <- paste0(req, '\n', 'START-OF-FIELDS')
  
  #fields
  for (field in fields) {
    req <- paste0(req, '\n', field)
  }
  
  #end of fields
  req <- paste0(req, '\n', 'END-OF-FIELDS', '\n')
  
  #start of data
  req <- paste0(req, '\n', 'START-OF-DATA')
  
  #overrides
  n <- length(overrides)
  if( n > 0 ) {
    overrides <- paste0('||', n, '|', paste(names(overrides), collapse = '|'), '|', paste(overrides, collapse = '|'), '|')
  }
  
  #data
  for (identifier in identifiers) {
    req <- paste0(req, '\n', identifier, overrides)
  }  
  
  #end of data
  req <- paste0(req, '\n', 'END-OF-DATA', '\n')
  
  #end of file
  req <- paste0(req, '\n', 'END-OF-FILE')
  
  return (req)
}



#' Upload a request file to Bloomberg
#' 
#' Upload a request file to query Bloomberg. A response file will be generated by Bloomberg. 
#' The request file can be user-defined following the Bloomberg Data License documentation or generated with the \code{\link{RblRequestBuilder}} function.
#' The response file needs to be downloaded (see \code{\link{RblDownload}}) and parsed (see \code{\link{RblParse}}) to import the data in R.
#' 
#' @param RblRequest character string representing the request file according to Bloomberg Datalicense documentation. Can be generated with the \code{\link{RblRequestBuilder}} function
#' @param filename name assigned to the remote file. Only alphanumeric characters and underscores are allowed. Invalid characters are removed. 
#' @param verbose logical. Should R report extra information on progress?
#' 
#' @return A list with components
#' \describe{
#'  \item{req}{the request filename}
#'  \item{out}{the response filename}
#' }
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' 
#' # Build a request file to download the daily closing prices of 
#' #  EURO STOXX Index from 2005-01-01 to 2015-12-31.
#' RblRequest <- 
#'      RblRequestBuilder(
#'           header = c(FIRMNAME = RblUser(), 
#'                      PROGRAMNAME = 'gethistory', 
#'                      DATERANGE = '20050101|20151231'),
#'           fields = c('PX_LAST'), 
#'           identifiers = c('SXXE Index')
#'      )
#'                                 
#' # Upload the request file
#' req <- RblUpload(RblRequest)
#' req
#' }
#' 
#' @import RCurl
#' 
#' @export
#' 
RblUpload <- function(RblRequest, filename = format(Sys.time(), "%m%d%H%M%S"), verbose = TRUE) {
  
  # request file
  if(nchar(filename)>14) stop("filename cannot exceed 14 characters")
  filename <- paste0("Rbl_", gsub("[^[:alnum:]_]","", filename))
  requestFileName <- paste0(filename, ".req")
  
  # stop if the request is already there
  if(any(grepl(RblFiles(), pattern = sprintf('^%s\\..*', filename))))
    stop(sprintf("%s is already there. Can't overwrite existing file.", filename))
  
  # uploading
  if (verbose) cat(paste0("Uploading file ", requestFileName, "\r\n" ))
  url <- paste(RblUrl(), requestFileName, sep = '/')
  ftpUpload(I(RblRequest), url)
  if (verbose) cat(paste0("Successfully uploaded file ", requestFileName, "\r\n" ))
  
  # response file
  responseFileName <- paste0(filename, ".out")
  if(grepl(x = RblRequest, pattern = 'PROGRAMNAME=gethistory', fixed = TRUE))
    responseFileName <- paste0(responseFileName, '.gz')
  
  # return
  return (list(req = requestFileName, out = responseFileName))
  
}



#' Download a file from Bloomberg
#' 
#' Download a generic file from Bloomberg. 
#' Use \code{\link{RblFiles}} to list the files available at Bloomberg.
#' If the file is not available yet (i.e. a request file has just been uploaded), the function waits until the response file is there or the timeout is reached
#' 
#' @param file character string representing the file to download
#' @param frequency the polling frequency to check if file is available at Bloomberg
#' @param timeout the timeout in seconds
#' @param verbose logical. Should R report extra information on progress?
#' 
#' @return character string. Path to the downloaded file. NULL on failure 
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' 
#' # Build a request file to download the daily closing prices of 
#' #  EURO STOXX Index from 2005-01-01 to 2015-12-31.
#' RblRequest <- 
#'      RblRequestBuilder(
#'           header = c(FIRMNAME = RblUser(), 
#'                      PROGRAMNAME = 'gethistory', 
#'                      DATERANGE = '20050101|20151231'),
#'           fields = c('PX_LAST'), 
#'           identifiers = c('SXXE Index')
#'      )
#'                                 
#' # Upload the request file
#' req <- RblUpload(RblRequest)
#' 
#' # Download the response file
#' out <- RblDownload(req$out)
#' out
#' }
#' 
#' @export
#' 
RblDownload <- function(file, frequency = 60, timeout = 3600, verbose = TRUE) {
  
  # check args
  if(frequency <= 0) stop("frequency must be > 0")
  if(timeout < 0) stop("timeout must be >= 0")
  
  # temp file to write 
  tmp <- tempfile()
  # url to query
  url <- RblUrl()

  # start downloading
  time <- 0
  while (time <= timeout) {
    
    if (verbose) cat(paste0("Checking if file ", file, " is available...\r\n"))
    
    if( class(try(suppressWarnings(utils::download.file(paste(url, file, sep='/'), destfile = tmp, method = "libcurl", quiet = !verbose)), silent = T)) == 'try-error' ) {
      
      time <- time + frequency
      if(time > timeout) break
        
      if (verbose) cat('File not yet available, waiting...\r\n')
      for (x in 1:as.integer(frequency / 2)) {
        if (verbose) cat('.')
        Sys.sleep(2)
      }
      if (verbose) cat('\r\n')
      
    } else {
      
      if (verbose) cat('File available! Downloading...\r\n')
      return(tmp)
      
    }
    
  }
  
  # timeout reached. Giving up!
  cat(paste0('Timeout! Could not download file ', file, ' from Bloomberg in ', timeout, ' sec. Giving up!\r\n'))
  return (NULL)

}




#' Parse Bloomberg response file and import data
#' 
#' Parse Bloomberg local response files and import the data in R. The PROGRAMNAME in use is auto detected: 'getdata' and 'gethistory' are supported.
#' 
#' @param file character string representing the local file to parse (see \code{\link{RblDownload}})
#' @param auto.assign logical. Should results be loaded to env when using PROGRAMNAME=gethistory? 
#' @param env where to create objects if auto.assign = TRUE
#' @param verbose logical. Should R report extra information on progress?
#' 
#' @return 
#' \describe{
#'  \item{PROGRAMNAME=getdata}{data.frame containig identifiers (rows) and fields (columns). NULL on failure.}
#'  \item{PROGRAMNAME=gethistory}{list of xts objects. If \emph{auto.assign}=TRUE the xts objects are loaded in \emph{env} and the object names are returned. NULL on failure.}
#' }
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' 
#' # Build a request file to download the daily closing prices of 
#' #  EURO STOXX Index from 2005-01-01 to 2015-12-31.
#' RblRequest <- 
#'      RblRequestBuilder(
#'           header = c(FIRMNAME = RblUser(), 
#'                      PROGRAMNAME = 'gethistory', 
#'                      DATERANGE = '20050101|20151231'),
#'           fields = c('PX_LAST'), 
#'           identifiers = c('SXXE Index')
#'      )
#'                                 
#' # Upload the request file
#' req <- RblUpload(RblRequest)
#' 
#' # Download the response file
#' out <- RblDownload(req$out)
#' 
#' # Import the data
#' data <- RblParse(out)
#' str(data)
#' }
#' 
#' @import xts
#' 
#' @export
#' 
RblParse <- function(file, auto.assign = FALSE, env = parent.frame(), verbose = TRUE){
  
  # check
  if(!is.character(file)) 
    return(NULL)
  
  # read file
  x <- readLines(file)
  # detect programname
  if(startsWith(x = x[1], prefix = '##')) programname <- ''
  else programname <- gsub(x[startsWith(x, 'PROGRAMNAME=')], pattern = 'PROGRAMNAME=', replacement = '')
  
  # parse
  db <- list()
  if(programname=='gethistory'){
    
    # security start/end indexes
    xx <- x[(which(x=='START-OF-DATA')+1):(which(x=='END-OF-DATA')-1)]
    start <- which(startsWith(x=xx, prefix="START SECURITY|"))
    end <- which(startsWith(x=xx, prefix="END SECURITY|"))
    if(length(start)!=length(end)) 
      stop('File malformed: START SECURITY not matching END SECURITY')
    
    # parse single security block
    for(i in 1:length(start)){
      s <- start[i]
      e <- end[i]
      if( e-s > 1 ){
        info <- unlist(strsplit(x = xx[s], split = '|', fixed = T))
        ticker <- info[2]
        field <- info[3]
        data <- unlist(strsplit(x = xx[(s+1):(e-1)], split = '|', fixed = T))
        data <- data[-which(data==ticker)]
        n <- length(data)
        id <- ticker 
        data <- xts(x = as.numeric(data[seq(2, n, by = 2)]), order.by = as.Date(data[seq(1, n, by = 2)], '%m/%d/%Y'))
        colnames(data) <- field
        if(is.null(db[[id]])) db[[id]] <- data
        else db[[id]] <- merge(db[[id]], data)
      }
    }
    
    # auto.assign
    if(auto.assign){
      ids <- names(db)
      for(id in ids)
        assign(x = make.names(id), value = db[[id]], envir = env)
      return(ids)
    }
    
    # return
    return(db)
    
  }
  else if(programname=='getdata'){
    
    # data
    data <- x[(which(x=='START-OF-DATA')+1):(which(x=='END-OF-DATA')-1)]
    
    # fields
    fields <- x[(which(x=='START-OF-FIELDS')+1):(which(x=='END-OF-FIELDS')-1)]
    
    # parse
    xx <- unlist(strsplit(data,  split = '|', fixed = T))
    n <- length(xx)
    by <- length(fields)+3
    db <- data.frame(row.names = xx[seq(1, n, by = by)])
    for(field in fields){
      db[,field] <- xx[seq(which(fields==field)+3, n, by = by)]
    }
  
    # return
    return(db)
    
  }
    
  # verbose
  if(verbose){
    cat(x)
    if(programname=='') warning(paste('There were errors detected within the response file'))
    else warning(paste('PROGRAMNAME', programname, 'not supported'))
  }
  
  # failure
  return(NULL)
}








