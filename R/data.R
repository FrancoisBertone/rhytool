#' A dataset describing examples of pump tests
#' @description  A series of 4 examples of pump test data:
#' \describe{
#'   \item{ths_ds1}{Data set of a pump test in a confined aquifer (pumping rate Q=1.3888e-2 m3/s, radial distance r=250 m) typed from: Table 5.1, page 172 in the following book: C.W. Fetter, 2001, Applied Hydrogeology, Fourth Edition. Prentice Hall, Upper Saddle River, 598 pp.}
#'   \item{ths_ds2}{Data set of a pump test in a confined aquifer aquired with automatic data logger. Author's personal data.}
#'   \item{htj_ds1}{Data set of a pump test in a confined aquifer with leakage through an aquitard. The data set for this example comes from the following reference: Hall, P. Water well and aquifer test analysis 1996, Water Resources Publications, LLC, 412 pp. Example Fig 11.14 p.267-268 (pumping rate Q=6.309E-3 m3/s, distance to the pumping well r=3.048 m, thickness of the aquitard e'=6.096 m)}
#'   \item{agt_ds1}{Data set of a precovery after constant rate pumping test. The data set for this example comes from: Todd D.K.(1980), Ground Water Hydrology, John Wiley & Sons, New York, Batu, V., Aquifer Hydraulics: A Comprehensive Guide to Hydrogeologic Data Analysis, John Wiley, New York, 1998. Example 4-12, Pages 183-186 (pumping rate Q=2500/24/60/60 m3/s, distance to the pumping well r=60 m, duration of pumping tp=240*60 s)}
#'   \item{tmr_ds1}{Data set of a variable rate pump test, from Kruseman and de Ridder (1994) pp. 185. The data are collected in a piezometer located at 5 m from the pumping well.}
#' }
#' @format Data are organised in a list of data.frame.
#' For each data.frame, V1 describes the time in second from begining of the pump test
#' and V2 describes the drawdowns mesured in meter at the corresponding time.
#' @author François Bertone \email{f.bertone@@bertone.associates}
#' @author Philippe renard \email{Philippe.Renard@@unine.ch}
#' @references
#' Renard, Philippe (2017). Hytool: an open source matlab toolbox for the interpretation of hydraulic tests using analytical solutions. _Journal of Open Source Software_, 2(19), 441, [doi:10.21105/joss.00441]
#' \url{http://joss.theoj.org/papers/10.21105/joss.00441}
#' @docType data
#' @keywords datasets
#' @name pumptest
#' @usage pumptest
data("pumptest")
"pumptest"
