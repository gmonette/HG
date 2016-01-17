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
#' If a variable in the SPSS file has a \emph{variable label} then
#' \code{read_sav} stores the label in the'label' attribute of the 
#' corresponding R variable.  The class of the variable is not 
#' affected by the presence of a variable label. Thus variable labels
#' can be read and and set with:  
#' \code{attr(data$Gender,"label")}  
#' \code{attr(data$Gender,"label") <- "Gender of respondent"}  
#' 
#' The \code{\link{varlab}} function provides an alternative way of doing this:  
#' \code{varlab(data$Gender)}  \cr
#' \code{varlab(data$Gender) <- "Gender of respondent"}  
#' 
#' You can read all the variable labels for a whole data frame with: \cr 
#' \code{vlabs <- varlabels(data)}  \cr
#' and later restore those variable labels with \cr  
#' \code{varlabels(data) <- vlabs}  
#' 
#' If the SPSS variable has \emph{value labels} then these labels
#' are stored in the 'labels' (note the plural) attribute of the 
#' R variable and its class is set to \code{\link[haven]{labelled}}.
#' 
#' (e.g.\code{attr(dd$Sex,'label')}) and the \emph{value labels} are
#' stored in the 'labels' attribute (e.g.\code{attr(dd$Sex,'labels')}).
#' Variables that have value labels in data sets created by \code{read_sav} 
#' belong to \code{haven}'s \code{\link[haven]{labelled}} class. 
#' 
#' Many manipulations in R will lose the variable labels. These need to be
#' restored before writing the file back to SPSS.
#' 
#' Current and planned functions to work with 'haven':
#' 
#' \code{\link{toR}}: Transform 'labelled' variables to factors.
#' 
#' \code{\link{varlabels}}: Read and restore \emph{variable labels} for a data frame.
#' 
#' \code{\link{varlab}}: Read and modify \emph{value label} for individual variable.
#' 
#' \code{\link{merge_check}}: Check compatibility of variable classes for merging.
#' 
HG <- function() help("HG")
