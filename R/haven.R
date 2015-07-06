### Package: HG
### Functions and methods to enhance the 'haven' package
### July 6, 2015

#' toR: Transform files read from SPSS.
#' 
#' Transforms variables in files read with \code{\link[haven]{read_sav}} into 
#' variables common in R.
#' 
#' @param data a data frame created by reading a SPSS file with 
#' \code{\link[haven]{read_sav}} in the 'haven' package.
#' 
#' @return a data frame with all 'labelled' and 'character' variables 
#' transformed to factors.
#' 
#' @examples
#' \dontrun{
#' library(haven)
#' dat <- read_sav("SPSSfile.sav")
#' dat <- toR(dat)
#' write_sav(dat, "OUT.sav")
#' }
#' @export 
toR <-
  function(data) {
    library(haven)
    #  as_factor.numeric <- function(x) x
    ret <- as.data.frame( lapply(data, as_factor))
    ret
  }
as_factor.numeric <- function(x) x 

#' varlabels: extract and restore SPSS variable labels
#' 
#' Extract SPSS variable labels from a data frame read with 
#' \code{\link[haven]{read_sav}} in the 'haven' package.
#' 
#' Many manipulations of data frames in R lose the \code{label} attribute
#' that holds SPSS variable labels. 'varlab' and 'varlab<-' allow the
#' variable labels to be extracted and later restored.
#' 
#' @param data a data frame, usually read with \code{\link[haven]{read_sav}}.
#' 
#' @return a named list of variable labels
#' 
#' @aliases varlabels<-
#' 
#' @examples
#' \dontrun{
#' library(haven)
#' dat <- read_sav("SPSSfile.sav")
#' varlabs <- varlabels(dat)
#' dat <- toR(dat)
#' # work with dat in R
#' varlabels(dat) <- varlabs
#' write_sav(dat, "OUT.sav") # should have variable labels
#' } 
#' @export
varlabels <- function(data) lapply(data, attr, 'label')
`varlabels<-` <- function(data, value) {
  # creates 'label' attribute if a variable does not have it
  namd <- names(data)
  namv <- names(value)
  reps <- intersect(namd, namv)
  
  for ( nn in reps) {
    if(is.null(attr(data[[nn]],'label'))) attr(data[[nn]], 'label') <- value[[nn]]
  }
  data
}

#' merge_check: Check compatibility of variables before merging.
#' 
#' Flags variables with the same name but different classes in two 
#' data frames to be merged.
#' 
#' @param data1, data2 two data frames to compare.
#' 
#' @return a data frame with variable names and classes in each input data frame.
#' 
#' @export
merge_check <- function(data1, data2) {
  n1 <- names(data1)
  n2 <- names(data2)
  class1 <- sapply(lapply(data1,class),paste,collapse="|")
  class2 <- sapply(lapply(data2,class),paste,collapse="|")
  d1 <- data.frame(var=n1,class1=class1)
  d2 <- data.frame(var=n2,class2=class2)
  ret <- merge(d1,d2,all=T)
  ret$Flag <- ifelse(ret$class1==ret$class2, "", "Different classes")
  ret
}
#' varlab: Read and modify variable labels
#' 
#' Read and modify variable labels for individual variables in a data frame read by \code{\link[haven]{read_sav}} in the 'haven' package.
#' 
#' @param x a variable which may be have a 'label' attribute
#' 
#' @return the label attribute
#' 
#' @export
varlab <- function(x,...) attr(x, 'label') 

`varlab<-` <- function(x, value, ...) {
  attr(x, 'label') <- value
  x
}
#' vallab: Read and modify SPSS value labels
#' 
#' Read and modify value labels for individual variables in a data frame read by \code{\link[haven]{read_sav}} in the 'haven' package.
#' 
#' @param x a variable which may be have a 'label' attribute
#' 
#' @return the labels attribute of objects of class 'labelled'
#' 
#' @examples
#' \dontrun{
#' dat <- read_sav("SPSSfile.sav")
#' vallab(dat$Gender)
#' vallab(dat$Gender) <- c('Male','Female')  # changing from, say, 'M' and 'F'
#' write_sav(dat,"OUT.sav")
#' }
#' 
#' @export
vallab <- function(x,...) UseMethod("vallab")
vallab.labelled <- function(x,...) attr(x, 'labels')
vallab.default <- function(x,...) stop('Only works on labelled objects')

`vallab<-` <- function(x, value, pos = NULL, ...) UseMethod("vallab<-")
`vallab<-.labelled` <- function(x, value, pos = NULL, ...) {
  labs <- attr(x, 'labels')
  if(is.null(pos)) pos <- seq_along(labs)
  if(is.logical(pos)) pos <- which(pos)
  if(max(pos) > length(labs)) stop('replacement position for value label out of range')
  if(length(pos) != length(value)) stop('replacement value labels wrong length')
  names(attr(x,'labels'))[pos] <- value
  x
}
`vallab<-.default` <- function(x, value, pos, ...) stop('Only works on labelled objects')
