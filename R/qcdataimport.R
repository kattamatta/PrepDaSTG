#' import data set for quality control of codes
#'
#' @description import data set with data and code to be quality controlled. Only .csv are valid to be imported.
#'
#' @param file_path name of the file with data to be quality controlled, can include absolute path
#'
#' @return data frame of imported data
#' @export

qcdata.import <- function(file_path){
  if(grepl(file_path, pattern = ".csv") == TRUE){
    assign("data_set", value = (utils::read.csv2(file_path, sep = ";", comment.char="#")), envir = .GlobalEnv)
    print("data files loaded")
  } else {
    print("file format for research data not supported; valid file format: .csv")
  }
}

