##
##  Comma-separated answer number to matrix or list of variables
##
##

#' Convert comma-separated lists of selections to a multiple-response questionnaire to a list of variables
#' 
#' Converts a variable containing list of selections, e.g. 1,4,10 to a list of variables, e.g. V1, V2, ..., V10, containing TRUE of FALSE indicating whether the corresponding item was selected. BUG: The function may fail mysteriously if one of the 'inputs' is not a number between 1 and the maximum number.
#' 
#' @param x variable to convert
#' @param (default 'Q') name for new variable, which will be followed by numbers to identify each question.
#' @param sep (default ',') separator between numbers
#' @examples
#' df <- data.frame(id = 1:4,
#'      ans = c('1,2,3','5,2,1,4','',NA))
#' df
#' df <- cbind(df,comma2mat(df$ans,"Ques."))
#' df    
#' @export
comma2mat <- function(x, name = 'Q', sep = ',') {
  # find highest number
  nos <- strsplit(as.character(x), split = sep)
  nos <- lapply(nos,as.numeric)
  maxn <- max(do.call(c,nos),na.rm = T)  # max Q number
  mat <- matrix( 0, length(x), maxn)
  rows <- rep(1:length(x), sapply(nos,length))
  reps <- cbind(rows, unlist(nos))
  mat[reps] <- 1
  colnames(mat) <- paste0(name,1:maxn)
  as.data.frame(mat)
}