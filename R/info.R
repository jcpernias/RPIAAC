#' Coding scheme for a PIAAC variable
#'
#' @param var Name of a variable
#' @return A dataframe with the coding scheme
#' @examples
#' variable_coding("AGE_R")
#' variable_coding("AGE5LFS")
#' @export
variable_coding <- function(var) {
  codeidx <- which(values$variable_code$Name == var)
  if(!length(codeidx))
    return(NULL)

  values$codes[values$codes$ID == values$variable_code$ID[codeidx],
               c("Label", "Value")]
}

#' Return information on PIAAC variables
#'
#' @param var Name of a variable
#' @return A list with metadata information
#' @examples
#' variable_info("AGE_R")
#' variable_info("AGE5LFS")
#' @export
variable_info <- function(var) {
  type_table <- c("i" = "Integer", "c" = "Character", "d" = "Floating point")
  ret <- list()
  varidx <- which(variables$Name == var)
  if(!length(varidx))
    return(ret)
  varinfo <- as.list(variables[varidx, ])

  miss <- missing_variables[missing_variables$Name == var, ]$Country

  codes <- variable_coding(var)

  ret$Name <- varinfo$Name
  ret$Label <- varinfo$Label
  ret$Type <- unname(type_table[varinfo$Type])
  ret$Codes <- codes
  ret$Missing <- miss
  ret
}

