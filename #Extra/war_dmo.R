#' Pumping test in a double porosity aquifer.
#' ==============================
#' This demonstrates the interpretation of a pumping test with the Warren and Root (1965) solution with the rhytool package
#'
#' The data set for this example comes from the following reference: Moench, A. 1984. Double porosity model for a fissured groundwater reservoir with fracture skin. Water Resources Research, 20(7), 831-846. Table 2, Page 838.
#'
#' Let us first create a dradown object with the data
library(rhytool)
rm(list=ls())
data("pumptest")
mydata <- drawdowns(pumptest$war_ds1)
#' And now let's visualize the diagnostic plot
plot(mydata, legend=FALSE)
