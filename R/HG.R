### HG package
### Created: July 6, 2015

#' A package to share between us
#' 
#' This is an overview of the tools in this package
#' 
#' To update the package:
#' 
#' \code{library(devtools)}
#' 
#' \code{install_github("gmonette/HG")}
#' 
#' @section Working with SPSS files:
#' 
#' Hadley Wickham's \code{haven} package makes it easy to read and write SPSS
#' 'sav' files but there are problems integrating his package with R functions
#' that manipulate packages, such as \code{\link[base]{merge}}. 
#' 
#' These functions facilitate manipulating SPSS files in R so they can be
#' rewritten back to SPSS.
#' 
#' The main \code{haven} functions are \code{\link[haven]{read_sav}} and 
#' \code{\link[haven]{write_sav}}. 
#' 
#' The data frame created by \code{read_sav} stores SPSS 
#' \emph{variable labels} in the 'label' attribute of each variable 
#' (e.g.\code{attr(dd$Sex,'label')}) and the \emph{value labels} are
#' stored in the 'labels' attribute (e.g.\code{attr(dd$Sex,'labels')}).
#' Variables with value labels belong to the \code{labelled} class. 
#' \code{\link[haven]{read_sav}} will have raw character and numeric variables
#' some of which will have class 'label'
#' 
#' Many manipulations in R will lose the variable labels. These need to be
#' restored before writing the file back to SPSS.
#' 
#' Current and planned functions to work with 'haven':
#' 
#' \code{\link{toR}}: Transform 'labelled' variables to factors.
#' 
#' This is a test to see whether math works: \deqn{y = X \beta+ \epsilon}.
HG <- function() help("HG")