\name{pumptest}
\alias{pumptest}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("pumptest")}
\format{
  The format is:
List of 5
 $ ths_ds1:'data.frame':	22 obs. of  2 variables:
  ..$ V1: int [1:22] 180 300 480 720 1200 1440 1800 2280 2820 3000 ...
  ..$ V2: num [1:22] 0.0914 0.2134 0.3962 0.6401 0.9754 ...
 $ thc_ds2:'data.frame':	1406 obs. of  2 variables:
  ..$ time: num [1:1406] 10 20 30 40 50 60 70 80 90 100 ...
  ..$ F   : num [1:1406] 0.3 1.44 2.28 2.9 3.42 3.84 4.21 4.5 4.76 4.94 ...
 $ agt_ds1:'data.frame':	15 obs. of  2 variables:
  ..$ V1: num [1:15] 60 120 180 300 420 600 900 1200 1800 2400 ...
  ..$ V2: num [1:15] 0.23 0.31 0.36 0.44 0.48 0.56 0.63 0.67 0.74 0.78 ...
 $ htj_ds1:'data.frame':	43 obs. of  2 variables:
  ..$ V1: num [1:43] 6.01 18.18 29.85 41.2 59.73 ...
  ..$ V2: num [1:43] 3.44 6.68 8.32 9.44 10.63 ...
 $ tmr_ds1:'data.frame':	18 obs. of  2 variables:
  ..$ V1: num [1:18] 300 600 900 1200 1500 1800 2100 2400 2700 3000 ...
  ..$ V2: num [1:18] 1.38 1.65 1.81 1.93 2.02 2.09 2.68 2.85 2.96 3.05 ...
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pumptest)
## maybe str(pumptest) ; plot(pumptest) ...
}
\keyword{datasets}
