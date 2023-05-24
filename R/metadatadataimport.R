#' import data
#'
#' @description import of data set with raw unprocessed assessment data (only .csv with , as separator) and an optional meta data set (.csv with ; as separator or .xls)of the respective STG survey
#'
#' @param path_data name of the file with assessment data, can include absolute path
#' @param path_metadata optional file name with absolute path with meta data of STG export - not for data aggregation!
#'
#' @return one or two data frames of imported data
#' @export

metadata.import <- function(path_data, path_metadata){
  if(!missing(path_metadata)){
    if (grepl(path_metadata, pattern = ".xls") == TRUE){
      assign("Metadata", value = (read_excel(path_metadata, .name_repair = "universal")), envir = .GlobalEnv)
    } else if(grepl(path_metadata, pattern = ".csv") == TRUE){
      assign("Metadata_renamed", value = (utils::read.csv(path_metadata, sep = ";")), envir = .GlobalEnv)
    } else {
      print("file format for meta data not supported; valid file format: .xls or .csv")
    }
  }
  if(grepl(path_data, pattern = ".csv") == TRUE){
    assign("STG_data", value = (utils::read.csv(path_data, sep = ",")), envir = .GlobalEnv)
    print("data files loaded")
  } else {
    print("file format for research data not supported; valid file format: .csv")
  }
}
