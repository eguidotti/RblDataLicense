# RblDataLicense

![](https://www.r-pkg.org/badges/version/RblDataLicense) ![](https://www.r-pkg.org/badges/last-release/RblDataLicense) ![](https://cranlogs.r-pkg.org/badges/grand-total/RblDataLicense)



The __RblDataLicense__ package aims at providing an easy R interface to access prices and market data with the __Bloomberg Data License__ service. Unlike the [Rblpapi](https://cran.r-project.org/package=Rblpapi) package for Bloomberg Terminal users, [RblDataLicense](https://cran.r-project.org/package=RblDataLicense) does not require the user to set up any working Bloomberg installation. As a prerequisite, a valid Data License from Bloomberg is needed together with the corresponding SFTP credentials and whitelisting of the IP from which accessing the service.

_This software and its author are in no way affiliated, endorsed, or approved by "Bloomberg" or any of its affiliates. "Bloomberg" is a registered trademark._



## Prerequisites

As a prerequisite, a valid Data License from Bloomberg is needed together with the corresponding SFTP credentials and whitelisting of the IP from which accessing the service. The credentials consist of the username and password needed to authenticate requests. As an additional security measure, all requests are blocked by default and are only accepted if coming from a whitelisted IP address. The user needs to inform Bloomberg of the IP addresses from which accessing the service. Only correctly authenticated requests from whitelisted IP will work. 

The Bloomberg Data License Service is billed on a pay-per-use model, based on the amount of instruments and data categories requested. The RblDataLicense package implements several checks to protect the user against unintended programming errors and to help keeping costs under control. In any case, to avoid incurring unexpected expenses, the user should carefully read the Bloomberg contract before accessing the Data License Service with this or any other software.

RblDataLicense is built on top of [RCurl](https://cran.r-project.org/package=RCurl), a wrapper for [libcurl](https://curl.haxx.se/libcurl/). The package establishes SFTP connections to Bloomberg Data License. On some Linux systems, this may not work out of the box, as libcurl does not natively support SFTP. In that case, the user needs to compile curl with SFTP support first. 



## Quickstart

As the very first step, the credentials received from Bloomberg to access the SFTP server must be provided to the `RblConnect` function. The function checks for the SFTP connection to be successful and stores the credentials in the current R session. Then, all the other functions of the package will be unlocked and ready to use.

```R
# These are dummy credentials. Replace with the credentials received from Bloomberg
RblConnect(user = 'dl000000', pw = '0000000000000000') 
```

The user should now prepare a request file according to the Bloomberg's documentation, containing the instructions for the data to retrieve. This file has to be uploaded to the SFTP server the user owning a valid License has been granted access to. Upon receipt, Bloomberg generates the corresponding response file containing the data. This process can take up to several minutes, depending on the kind of request. Once available, the user can download the response file from the SFTP server and parse it to import the data in R.

### Step by Step Example

```R
# Build a request file to download the daily closing prices of
#  EURO STOXX Index from 2005-01-01 to 2015-12-31
RblRequest <- 
	RblRequestBuilder(
        header = c(FIRMNAME = RblUser(),
                   PROGRAMNAME = 'gethistory',
                   DATERANGE = '20050101|20151231'),
        fields = c('PX_LAST'),
        identifiers = c('SXXE Index')
    )

# Upload the request file
req <- RblUpload(RblRequest)

# Download the response file
out <- RblDownload(req$out)

# Import the data
data <- RblParse(out)
```

### All in One Example

```R
data <- RblQuery(fields = c('PX_LAST', 'PX_OPEN', 'PX_HIGH', 'PX_LOW'), 
                 identifiers = c('SXXE Index', "SX5E Index"), 
                 from = '2005-01-01')
```



## Acknowledgements

The RblDataLicense was developed at Algo Finance Sagl, software house start-up developing financial algorithms for the asset management industry. The Swiss company is gratefully acknowledged for open-sourcing the software.



## Legal

All code of the RblDataLicense is released under the [GNU GPL-3](https://cran.r-project.org/web/licenses/GPL-3). This software and its author are in no way affiliated, endorsed, or approved by "Bloomberg" or any of its affiliates. "Bloomberg" is a registered trademark. All trademarks and registered trademarks are the property of their respective owners.



## Additional Resources

https://emanueleguidotti.dev/RblDataLicense

https://cran.r-project.org/package=RblDataLicense

https://www.bloomberg.com/professional/product/data-license