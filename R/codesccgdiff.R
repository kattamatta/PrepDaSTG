#' find divergent codes between child and the caregivers ignoring personal letter of code scheme
#'
#' @description filters codes that do not match between the child and the caregivers ingnoring their personal letter in the code and returns the missmatching codes of the child only. The corresponding missmatiching codes of the caregivers are NOT returned.
#'
#' @param data data frame containing both children's and caregivers' codes
#' @param code character string indicating variable name of children's code
#' @param code_cg character string indicating variable name of caregivers' code
#'
#' @return character vector with children's codes only (!) that differ from their caregivers' codes
#' @export

codesccg.diff <- function(data, code, code_cg)
{ c <- data[[code]]
  p <- data[[code_cg]]
  c_sub <- gsub("(.*)([[:alpha:]]+)(.*)", "\\1\\3", c)
  p_sub <- gsub("(.*)([[:alpha:]]+)(.*)", "\\1\\3", p)
  tmp <- setdiff(c_sub, p_sub)
  gsub("(.{3})", "\\1c\\2", tmp)
}




