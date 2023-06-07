#' renaming STG data
#'
#' @description renaming variable names of STG exported data sets
#'
#' @param data file with STG exported data with column names to be renamed to variable names based on renaming file
#' @param subject character indicating participant group or interview format. Must be one of "Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students", "Teachers", or "TwinCaregivers".
#' @param country character specifying the country participant codes were assessed. Must be one of "Ghana", "Tanzania", "Uganda", or "Haiti".
#' @param time character indicating assessment occasion. Must be one of "baseline", "follow-up 1", or "follow-up 2".
#' @param save logical. If TRUE returned data set will be saved as .csv
#' @param path optional. absolute path where returned data set should be stored
#'
#' @return data set with STG exported data with renamed column names based on renaming file
#' @export

STGvariables.rename <- function (data, subject = c("Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students", "Teachers", "TwinCaregivers"), country = c("Ghana", "Tanzania", "Uganda", "Haiti"), time = c("baseline", "follow-up 1", "follow-up 2"), save, path) {
  colnames(data) <- sub('A_Q', 'T_Q', colnames(data))
  time <- match.arg(time)
  subject <- match.arg(subject)
  country <- match.arg(country)
  if(subject == "Caregivers" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogCaregivers")
    rename <- ItemCatalogCaregivers
    remove(ItemCatalogCaregivers, envir = .GlobalEnv)
  }
  if(subject == "Caregivers" & time == "follow-up 1" & country != "Haiti"){
    data("ItemCatalogCaregiversFollowUp1")
    rename <- ItemCatalogCaregiversFU1
    remove(ItemCatalogCaregiversFU1, envir = .GlobalEnv)
  }
  if(subject == "Children" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogChildren")
    rename <- ItemCatalogChildren
    remove(ItemCatalogChildren, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogHeadTeachers")
    rename <- ItemCatalogHeadTeachers
    remove(ItemCatalogHeadTeachers, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "follow-up 1" & country != "Haiti"){
    data("ItemCatalogHeadTeachersFollowUp1")
    rename <- ItemCatalogHeadTeachersFU1
    remove(ItemCatalogHeadTeachersFU1, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogHeadTeachersHaiti")
    rename <- ItemCatalogHeadSchoolHaiti
    remove(ItemCatalogHeadSchoolHaiti, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogHeadNumeracyLiteracy")
    rename <- data("ItemCatalogNumeracyLiteracy")
    remove(ItemCatalogNumeracyLiteracy, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "follow-up 1" & country != "Haiti"){
    data("ItemCatalogNumeracyLiteracyFollowUp1")
    rename <- ItemCatalogNumeracyLiteracyFU1
    remove(ItemCatalogNumeracyLiteracyFU1, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogNumeracyLiteracyHaiti")
    rename <- ItemCatalogNumeracyLiteracyHaiti
    remove(ItemCatalogNumeracyLiteracyHaiti, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogStudents")
    rename <- ItemCatalogStudents
    remove(ItemCatalogStudents, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogStudentsHaiti")
    rename <- ItemCatalogStudentsHaiti
    remove(ItemCatalogStudentsHaiti, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "follow-up 1" & country != "Haiti"){
    data("ItemCatalogStudentsFollowUp1")
    rename <- ItemCatalogStudentsFU1
    remove(ItemCatalogStudentsFU1, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogTeachers")
    rename <- ItemCatalogTeachers
    remove(ItemCatalogTeachers, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogTeachersHaiti")
    rename <- ItemCatalogTeachersHaiti
    remove(ItemCatalogTeachersHaiti, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "follow-up 1" & country != "Haiti"){
    data("ItemCatalogTeachersFollowUp1")
    rename <- ItemCatalogInterviewTeacherFU1
    remove(ItemCatalogInterviewTeacherFU1, envir = .GlobalEnv)
  }
  if(subject == "TwinCaregivers" & time == "baseline" & country != "Haiti"){
    data("ItemCatalogTwinCaregivers")
    rename <- ItemCatalogTwinCaregivers
    remove(ItemCatalogTwinCaregivers, envir = .GlobalEnv)
  }
  rename$STGexportName <- sub('A_Q', 'T_Q', rename$STGexportName)
  iconv(colnames(data), from = "utf8", to = "ASCII//TRANSLIT")
  rename$NewName <- if(time == "baseline"){
                      sub("^", "BA_", rename$NewName)
                    }
                    else if(time == "follow-up 1"){
                          sub("^", "FU1_", rename$NewName)
                         }
                        else {
                        sub("^", "FU2_", rename$NewName)
                        }
  if(time == "follow-up 1"){
   names(data) <- sub("_F1", "", names(data))
  }
  names <- data.frame("var" = rename$STGexportName,
                      "var_new"= rename$NewName)
  names_valid <- names[names$var %in% colnames(data), ]
  assign("STG_data_renamed", value = (data |>
                                        rename_at(vars(names_valid$var), ~ names_valid$var_new, perserve = T)), envir = .GlobalEnv)
  #remove(rename, envir = .GlobalEnv)
   if (all(data == STG_data_renamed, na.rm = T) == TRUE){
    print("variables of STG_data successfully renamed in STG_data_renamed")
  } else{
    print("error")
  }
  if (save == "TRUE"){
    filename <- paste("STG_data_renamed", ".csv", sep = "")
    file <- paste(path, filename, sep = "/")
    utils::write.csv2(STG_data_renamed, file = file, row.names = FALSE)
    print("saved")
  } else {
    print("done")
  }
}
