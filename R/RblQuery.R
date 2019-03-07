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
#' @param identifiers vector of Bloomberg identifiers. Ex c('SXXE Index', 'SX5E Index')
#' @param fields vector of Bloomberg fields. Ex. c('PX_LAST', 'PX_CLOSE', 'PX_OPEN', 'PX_HIGH', 'PX_LOW')
#' @param from date or string (format YYYY-MM-DD). The start time of the period of interest
#' @param to date or string (format YYYY-MM-DD). The end time of the period of interest. Ignored if \emph{from} is not provided 
#' @param overrides named list of Bloomberg overrides. Ex list('END_DT' = '20100101')
#' @param auto.assign logical. Should results be loaded to env? Ignored if \emph{from} is not provided 
#' @param env where to create objects if auto.assign = TRUE
#' @param split maximum number of identifiers to process at once. Split requests to avoid memory leaks
#' @param pollFrequency the polling frequency to check if the response file is available at Bloomberg
#' @param timeout the timeout in seconds
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
#' # Run RblConnect first
#' x <- RblQuery(fields = c('PX_LAST', 'PX_OPEN', 'PX_HIGH', 'PX_LOW'), identifiers = c('SXXE Index', "SX5E Index"), from = '2005-01-01')
#' str(x)
#' 
#' @export
#' 
RblQuery <- function(
  identifiers, 
  fields, 
  from = NULL, 
  to = Sys.Date(), 
  overrides = NULL,
  auto.assign = FALSE, 
  env = parent.frame(),
  split = 100,
  pollFrequency = 60, 
  timeout = 3600, 
  verbose = TRUE) 
{
  
  # checks
  if(is.null(from)){
    if(to!=Sys.Date()) warning('Tha parameter "to" is ignored if "from" is not provided')
    if(auto.assign) warning('Tha parameter "auto.assign" is ignored if "from" is not provided')
  }
  
  # header
  header <- list()
  header$FIRMNAME <- RblUser()
  header$PROGRAMNAME <- ifelse(is.null(from), 'getdata', 'gethistory')
  
  # header for getdata
  if(header$PROGRAMNAME=='getdata') {
    header$PRICING = 'yes' 
    header$SECMASTER = 'yes'
    header$FUNDAMENTALS = 'yes'
  }
  
  # header for gethistory
  if(header$PROGRAMNAME=='gethistory'){
    fmt <- '%Y%m%d'
    header$DATERANGE <- paste0(format(as.Date(from), fmt), '|', format(as.Date(to), fmt))
  }
  
  # info 
  i <- list()
  i$req <- list()
  i$out <- list()
  
  # split identifiers
  identifiers <- suppressWarnings(split(x = identifiers, seq(1, length(identifiers), by = split)))
  
  # upload request files
  for(n in 1:length(identifiers)){
  
    # build request file
    RblRequest <- RblRequestBuilder(header = header, fields = fields, identifiers = identifiers[[n]], overrides = overrides)
    
    # upload request file
    request <- RblUpload(RblRequest = RblRequest, verbose = verbose)
    # store info
    i$req[[request$req]] <- unlist(strsplit(RblRequest, '\n'))
    i$out[[request$out]] <- ''
    
  }
  
  # download response files
  for(file.out in names(i$out)){
    
    # download response files
    file <- RblDownload(file = file.out, pollFrequency = pollFrequency, timeout = timeout, verbose = verbose)
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
