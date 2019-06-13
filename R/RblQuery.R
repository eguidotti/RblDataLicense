#' Query Bloomberg and Fetch Data
#' 
#' The function provides a high level interface to Bloomberg Datalicense 'getdata' and 'gethistory' programs.
#' 
#' The following routine to query Bloomberg is implemented:
#' \describe{
#'  \item{Build the request file}{see \code{\link{RblRequestBuilder}}}
#'  \item{Upload the request file}{see \code{\link{RblUpload}}}
#'  \item{Download the response file}{see \code{\link{RblDownload}}}
#'  \item{Parse the response file}{see \code{\link{RblParse}}}
#' }
#' 
#' @param identifiers vector of Bloomberg identifiers. E.g. c('SXXE Index', 'SX5E Index')
#' @param fields vector of Bloomberg fields. E.g. c('PX_LAST', 'PX_CLOSE', 'PX_OPEN', 'PX_HIGH', 'PX_LOW')
#' @param from date or string (format YYYY-MM-DD). Start time for the 'gethistory' request. If not provided, a 'getdata' request will be made
#' @param to date or string (format YYYY-MM-DD). End time for the 'gethistory' request. Ignored if \emph{from} is not provided 
#' @param overrides named vector of Bloomberg overrides. E.g. c('END_DT' = '20100101')
#' @param add_headers named vector of additional headers. E.g. c(PROGRAMFLAG = 'oneshot')
#' @param auto.assign logical. Should results be loaded to env? Ignored if \emph{from} is not provided 
#' @param env where to create objects if auto.assign = TRUE
#' @param category vector of Data License categories to enable. E.g. c('SECMASTER', 'PRICING', 'FUNDAMENTALS'). WARNING! Each DL category is billed separately, so check your DL license carefully!
#' @param limit prevent requesting data for more than this amout of identifiers. This is done to help you keeping your budget under control. Data License is billing based on the amout of instruments you request, so check your DL license carefully before increasing this limit. 
#' @param split maximum number of identifiers to process at once. Requests are split to avoid memory leaks.
#' @param frequency the polling frequency to check if the response file is available at Bloomberg
#' @param timeout the timeout in seconds
#' @param filename name assigned to the remote file. Only alphanumeric characters are allowed. Invalid characters are removed. 
#' @param verbose logical. Should R report extra information on progress?
#' 
#' @return 
#' A list with components
#' \describe{
#'  \item{req}{
#'   List of characters representing each of the request files uploaded to Bloomberg
#'  }
#'  \item{out}{
#'   List of characters representing each of the response file downloaded from Bloomberg
#'  }
#'  \item{data}{
#'   Return of \code{\link{RblParse}}
#'  }
#' }
#' 
#' @examples 
#' \dontrun{
#' # Run RblConnect first
#' x <- RblQuery(fields = c('PX_LAST', 'PX_OPEN', 'PX_HIGH', 'PX_LOW'), 
#'               identifiers = c('SXXE Index', "SX5E Index"), 
#'               from = '2005-01-01')
#' str(x)
#' }
#' 
#' @export
#' 
RblQuery <- function(
  identifiers, 
  fields, 
  from = NULL, 
  to = Sys.Date(), 
  auto.assign = FALSE, 
  env = parent.frame(),
  category = c(),
  add_headers = c(),
  overrides = c(),
  limit = 5,
  split = 100,
  frequency = 60, 
  timeout = 3600, 
  filename = format(Sys.time(), "%m%d%H%M%S"),
  verbose = TRUE) 
{
  
  # checks
  if(length(identifiers)>limit) stop(paste('\n Preventing Data License from requesting', length(identifiers), 'instruments. Increase the "limit" parameter to confirm and run the query. \n\n WARNING! \n Data License is billing based on the amout of instruments you request, so check your DL license carefully before increasing the limit.'))
  if(is.null(from)){
    if(to!=Sys.Date()) warning('Tha parameter "to" is ignored if "from" is not provided')
    if(auto.assign) warning('Tha parameter "auto.assign" is ignored if "from" is not provided')
  }
  
  # header
  header <- c()
  header[['FIRMNAME']] <- RblUser()
  header[['PROGRAMNAME']] <- ifelse(is.null(from), 'getdata', 'gethistory')
  
  # Enable DL categories
  for(i in category) header[[i]] <- 'yes'
  
  # header for gethistory
  if(header[['PROGRAMNAME']]=='gethistory'){
    fmt <- '%Y%m%d'
    header[['DATERANGE']] <- paste0(format(as.Date(from), fmt), '|', format(as.Date(to), fmt))
  }
  
  # add headers
  for(i in names(add_headers)) header[[i]] <- add_headers[[i]]
  
  # info 
  i <- list()
  i$req <- list()
  i$out <- list()
  
  # split identifiers
  identifiers <- suppressWarnings(split(x = identifiers, seq(1, length(identifiers), by = split)))
  
  # check filename
  maxchar <- 13 - nchar(length(identifiers))
  if(nchar(filename) > maxchar) 
    stop(sprintf("filename cannot exceed %s characters", maxchar))
  
  # upload request files
  for(n in 1:length(identifiers)){
  
    # build request file
    RblRequest <- RblRequestBuilder(header = header, fields = fields, identifiers = identifiers[[n]], overrides = overrides)
    
    # upload request file
    request <- RblUpload(RblRequest = RblRequest, filename = paste(filename, n, sep = '_'), verbose = verbose)
    
    # store info
    i$req[[request$req]] <- unlist(strsplit(RblRequest, '\n'))
    i$out[[request$out]] <- ''
    
  }
  
  # download response files
  for(file.out in names(i$out)){
    
    # download response files
    file <- RblDownload(file = file.out, frequency = frequency, timeout = timeout, verbose = verbose)
    # store info
    if(!is.null(file)) i$out[[file.out]] <- paste0('Read this file: ', file)
    
    # parse file and make data available in R
    data <- RblParse(file = file, auto.assign = auto.assign, env = env, verbose = verbose)
    # store info
    if(is.null(i$data)) i$data <- data    
    else if (class(data)=='data.frame') i$data <- rbind(i$data, data)
    else if (class(data)=='list') for(id in names(data)) i$data[[id]] <- data[[id]]
    else if (class(data)=='character') i$data <- c(i$data, data)
    
  }

  # return
  return(i)
}
