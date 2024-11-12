#' convert metadata
#'
#' @description convert meta data to useful meta data set for renaming variable names in assessment data sets (not used for data aggregation!)
#'
#' @param metadata data set containing exported meta data to be renamed
#' @param data data set containing exported assessment data
#' @param subject character indicating participant group or interview format. Must be one of "Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students", "Teachers", or "TwinCaregivers".
#' @param country character specifying the country participant codes were assessed. Must be one of "Ghana", "Tanzania", "Uganda", "Haiti", or "Pakistan".
#' @param time character indicating of assessment period. Must be one of "baseline", "follow-up 1", or "follow-up 2".
#' @param save logical. If TRUE returned data set will be saved as .csv
#' @param file optional. absolute path including file name and appendix .csv in "" where returned data set should be stored
#'
#' @return data set with renamed meta data
#' @export

metadata.convert <- function(metadata, data, subject = c("Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students", "Teachers", "TwinCaregivers"), country = c("Ghana", "Tanzania", "Uganda", "Haiti", "Pakistan"), time = c("baseline", "follow-up 1", "follow-up 2"), save, file) {
  time <- match.arg(time)
  subject <- match.arg(subject)
  country <- match.arg(country)
  metadata <- metadata
  if(subject == "Caregivers" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogCaregivers")
    rename <- ItemCatalogCaregivers
    remove(ItemCatalogCaregivers, envir = .GlobalEnv)
  }
  if(subject == "Caregivers" & time == "follow-up 1" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogCaregiversFollowUp1")
    rename <- ItemCatalogCaregiversFU1
    remove(ItemCatalogCaregiversFU1, envir = .GlobalEnv)
  }
  if(subject == "Children" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogChildren")
    rename <- ItemCatalogChildren
    remove(ItemCatalogChildren, envir = .GlobalEnv)
  }
  if(subject == "Children" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogChildrenHaiti")
    rename <- ItemCatalogUSetChildrenHaiti
    remove(ItemCatalogUSetChildrenHaiti, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogHeadTeachers")
    rename <- ItemCatalogHeadTeachers
    remove(ItemCatalogHeadTeachers, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "follow-up 1" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogHeadTeachersFollowUp1")
    rename <- ItemCatalogHeadTeachersFU1
    remove(ItemCatalogHeadTeachersFU1, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogHeadTeachersHaiti")
    rename <- ItemCatalogHeadSchoolHaiti
    remove(ItemCatalogHeadSchoolHaiti, envir = .GlobalEnv)
  }
  if(subject == "HeadTeachers" & time == "baseline" & country == "Pakistan"){
    data("ItemCatalogHeadTeachersPakistan")
    rename <- ItemCatalogHeadSchoolPakistan
    remove(ItemCatalogHeadSchoolPakistan, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogNumeracyLiteracy")
    rename <- ItemCatalogNumeracyLiteracy
    remove(ItemCatalogNumeracyLiteracy, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogNumeracyLiteracyHaiti")
    rename <- ItemCatalogNumeracyLiteracyHaiti
    remove(ItemCatalogNumeracyLiteracyHaiti, envir = .GlobalEnv)
  }
  if(subject == "NumeracyLiteracy" & time == "follow-up 1" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogNumeracyLiteracyFollowUp1")
    rename <- ItemCatalogNumeracyLiteracyFU1
    remove(ItemCatalogNumeracyLiteracyFU1, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogStudents")
    rename <- ItemCatalogStudents
    remove(ItemCatalogStudents, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogStudentsHaiti")
    rename <- ItemCatalogStudentsHaiti
    remove(ItemCatalogStudentsHaiti, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "baseline" & country == "Pakistan"){
    data("ItemCatalogStudentsPakistan")
    rename <- ItemCatalogStudentsPakistan
    remove(ItemCatalogStudentsPakistan, envir = .GlobalEnv)
  }
  if(subject == "Students" & time == "follow-up 1" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogStudentsFollowUp1")
    rename <- ItemCatalogStudentsFU1
    remove(ItemCatalogStudentsFU1, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogTeachers")
    rename <- ItemCatalogTeachers
    remove(ItemCatalogTeachers, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "baseline" & country == "Haiti"){
    data("ItemCatalogTeachersHaiti")
    rename <- ItemCatalogTeachersHaiti
    remove(ItemCatalogTeachersHaiti, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "baseline" & country == "Pakistan"){
    data("ItemCatalogTeacherPakistan")
    rename <- ItemCatalogTeacherPakistan
    remove(ItemCatalogTeacherPakistan, envir = .GlobalEnv)
  }
  if(subject == "Teachers" & time == "follow-up 1" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogTeachersFollowUp1")
    rename <- ItemCatalogInterviewTeacherFU1
    remove(ItemCatalogInterviewTeacherFU1, envir = .GlobalEnv)
  }
  if(subject == "TwinCaregivers" & time == "baseline" & country != "Haiti" & country != "Pakistan"){
    data("ItemCatalogTwinCaregivers")
    rename <- ItemCatalogTwinCaregiver
    remove(ItemCatalogTwinCaregiver, envir = .GlobalEnv)
  }
  rename2 <- rename
  metadata$Text <- gsub("<.*?>", "", metadata$Text)
  metadata$Text <- ifelse(grepl(metadata$Text, pattern = "&nbsp;"), gsub('&nbsp;', ' ', metadata$Text), metadata$Text)
  if(any(grepl(colnames(data), pattern = "A_Q_"))==TRUE){
    questionindex_tmp <- sub('A_Q_(\\d+).*', '\\1', colnames(data))
    questionindex_tmp <- subset(colnames(data), grepl(colnames(data), pattern = "A_Q_[[:digit:]].*"))
    questionindex_tmp2 <- sub('A_Q_(\\d+).*', '\\1', questionindex_tmp)
    questionindex_tmp3 <- subset(questionindex_tmp2, duplicated(questionindex_tmp2) == FALSE)
    df_tmp <- data.frame(matrix(ncol = ncol(metadata), nrow = 2*length(questionindex_tmp3)))
    colnames(df_tmp) <- colnames(metadata)
    df_tmp$Question.Index <- rep(questionindex_tmp3, 2)
    df_tmp <- df_tmp[order(df_tmp$Question.Index),]
    df_tmp$Index <- rep(c(0:1))
    df_tmp$Text <- rep(c("item category unchecked", "item category checked"))
    df_tmp$Is.Topic <- rep("FALSE")
    metadata$Is.Topic <- ifelse((metadata$Question.Index %in% questionindex_tmp3) & metadata$Is.Topic == "FALSE", "TRUE", metadata$Is.Topic)
    metadata <- rbind(metadata, df_tmp)
    metadata <- metadata[order(as.numeric(metadata$Question.Index)),]
    colnames(data) <- sub('A_Q', 'T_Q', colnames(data))
    rename$STGexportName <- sub('A_Q', 'T_Q', rename$STGexportName)
  }
  else{
  }
  metadata$Entrance.Rule <- sub("Answer", "Q_", metadata$Entrance.Rule)
  metadata$Question.Index_long <- ifelse(metadata$Is.Topic == "TRUE", 
                                         paste((sub("^","T_Q_",metadata$Question.Index)), metadata$Text, sep = "_"), 
                                         ifelse(metadata$Is.Topic == "FALSE", 
                                                paste((sub("^","A_Q_",metadata$Question.Index)), metadata$Text, sep = "_"), 
                                                metadata$Question.Index))
  metadata$Question.Index_long <- ifelse(is.na(metadata$Is.Topic) == TRUE, 
                                         paste((sub("^","Q_",metadata$Question.Index)), metadata$Text, sep = "_"), 
                                         metadata$Question.Index_long)
  metadata$Question.Index_short <- ifelse(metadata$Is.Topic == "TRUE", 
                                          paste(paste(sub("^","T_Q_",metadata$Question.Index)), metadata$Index, sep = "_"), 
                                          NA)
  metadata$Question.Index_short <- ifelse(is.na(metadata$Is.Topic) == TRUE, 
                                          paste(sub("^","Q_",metadata$Question.Index)), 
                                          metadata$Question.Index_short)
  metadata$Question.Index_long <- ifelse(is.na(metadata$Index) & is.na(metadata$Is.Topic), 
                                         paste(
                                           paste(
                                             paste((sub("^","Q_",metadata$Question.Index)), "Entrance.Rule" , sep = "_"), 
                                           metadata$Entrance.Rule, sep = "_"), 
                                          metadata$Text, sep = "_"), 
                                         metadata$Question.Index_long)
  metadata <- metadata[metadata$Question.Index_long != "NA_Entrance.Rule_NA_NA", ]
  metadata$DataVariableName <- rename$NewName[match(metadata$Question.Index_short, rename$STGexportName)]
  `%nin%` <- Negate(`%in%`)
  metadata$Question.Index_long <- ifelse((metadata$Question.Index_short %nin% rename2$STGexportName & metadata$DataVariableName %in% rename2$NewName) == TRUE, sub("T_Q_", "A_Q_", metadata$Question.Index_long), metadata$Question.Index_long)
  metadata$Question.Index_short <- ifelse((metadata$Question.Index_short %nin% rename2$STGexportName & metadata$DataVariableName %in% rename2$NewName) == TRUE, sub("T_Q_", "A_Q_", metadata$Question.Index_short), metadata$Question.Index_short)
  metadata$DataVariableName <- ifelse(time == "baseline" & is.na(metadata$DataVariableName) == FALSE, 
                                      sub("^", "BA_", metadata$DataVariableName), 
                                      ifelse(time == "follow-up 1" & is.na(metadata$DataVariableName) == FALSE, 
                                             sub("^", "FU1_", metadata$DataVariableName), 
                                             ifelse(time == "follow-up 2" & is.na(metadata$DataVariableName) == FALSE, 
                                                    sub("^", "FU2_", metadata$DataVariableName), 
                                                    NA)
                                             )
                                      )
  metadata_renamed <- metadata |> relocate(c(DataVariableName, Question.Index_short, Question.Index_long), .before = Chapter.Index)
  metadata_renamed$Question.Index_long <- gsub("_NA_", "_", metadata_renamed$Question.Index_long)
  assign("Metadata_renamed", metadata_renamed, value = , envir = .GlobalEnv)
  #remove(rename, envir = .GlobalEnv)
  if (save == "TRUE"){
    #filename <- paste("Metadata_renamed", ".csv", sep = "")
    file <- paste(file)
    utils::write.csv2(Metadata_renamed, file = file, row.names = FALSE)
  } else {
  }
  print("successfully created data.frame 'Metadata_renamed'")
}
