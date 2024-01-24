#' renaming cognitive data
#'
#' @description renaming variable names of cognitive data sets
#'
#' @param data file with cognitive data with column names to be renamed
#' @param time character indicating assessment occasion. Must be one of "baseline", "follow-up 1", or "follow-up 2"
#' @param save logical. If TRUE returned data set will be saved as .csv
#' @param file optional. absolute path including file name and appendix .csv in "" where returned data set should be stored
#' @return data set with cognitive data with renamed column names
#' @export


cognitivevariables.rename <- function (data, time = c("baseline", "follow-up 1", "follow-up 2"), save, file) {
  iconv(colnames(data), from = "utf8", to = "ASCII//TRANSLIT")
  time <- match.arg(time)
  #data <- data
  colnames(data) <- ifelse(colnames(data) == "code", sub("^", "c_", colnames(data)), colnames(data))
  colnames(data) <- if(time == "baseline"){
    sub("^", "BA_", colnames(data))
  }
  else if(time == "follow-up 1"){
    sub("^", "FU1_",  colnames(data))
  }
  else {
    sub("^", "FU2_",  colnames(data))
  }
  assign("cognitivedata", data, value = , envir = .GlobalEnv)
  if (save == "TRUE"){
    file <- paste(file)
    utils::write.csv2(cognitivedata, file = file, row.names = FALSE)
    print("saved")
  } else {
    print("done")
  }
}



