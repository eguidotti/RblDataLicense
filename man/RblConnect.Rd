% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{RblConnect}
\alias{RblConnect}
\title{Create an SFTP connection to Bloomberg Data License}
\usage{
RblConnect(
  user,
  pw,
  host = "sftp.bloomberg.com",
  port = "22",
  protocol = "sftp",
  verbose = TRUE
)
}
\arguments{
\item{user}{The account number assigned by Bloomberg}

\item{pw}{The password assigned by Bloomberg}

\item{host}{The connection host}

\item{port}{The connection port}

\item{protocol}{The connection protocol}

\item{verbose}{logical. Should print extra information on failure?}
}
\value{
logical. Is the connection succesful?
}
\description{
An sftp connection to Bloomberg Datalicense is established. On some Linux systems, this may
not work out of the box, as libcurl does not natively support sftp. In that case, you need to 
compile curl with SFTP support. 
See here for details: http://askubuntu.com/questions/195545/how-to-enable-sftp-support-in-curl
}
\examples{
\dontrun{
# These are dummy credentials. Replace with the credentials received from Bloomberg 
RblConnect(user = 'dl000000', pw = '0000000000000000') 
}

}
