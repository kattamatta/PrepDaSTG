#' quality control that filters baseline participant codes not matching participation code & demographic info between assessment phases (BA, FU1)
#'
#' @description this function filters cases (rows, baseline participation codes) that do not match participation code & demographic info between assessment phases 
#'
#' @param data data frame, must include variables "UUID_code"
#' @param person character specifying the person participants code belongs to. Must be one of "Teacher" or "Child"
#' @param save logical. If TRUE returned data frame will be saved as .csv with ; separation
#' @param file optional. absolute path including file name and appendix .csv in "" where returned data set should be stored
#'
#' @return data frame of participant's unique UUID not not matching participation code & demographic info between assessment phases (Baseline, Follow Up 1, Follow Up 2)
#'
#' @export

qcphasesBAFU1FU2.filtering <- function(data, person = c("Teacher", "Child"), save, file)
{
  persmatch <- match.arg(person)

  # code matching BA & FU1 (children | teacher)
  person <- if (persmatch == "Child") { 
    dfcodeBAFU1 <- data.frame(subset(data, data$BA_c_code!= data$FU1_c_code, c(UUID_code, BA_c_code, FU1_c_code)))
  } else if (persmatch == "Teacher") {
    dfcodeBAFU1 <- data.frame(subset(data, data$BA_t_code!= data$FU1_t_code, c(UUID_code, BA_t_code, FU1_t_code)))
  } else { 
  }
  if (length(dfcodeBAFU1$UUID_code) != 0){
    names(dfcodeBAFU1) <- c("UUID_code", "BA_code", "FU1_code")
    dfcodeBAFU1$pat <- paste0((paste0("BA_code=", dfcodeBAFU1$BA_code)), sep = " vs. ", (paste0("FU1_code=", dfcodeBAFU1$FU1_code)))
    dfcodeBAFU1$BA_code <- NULL
    dfcodeBAFU1$FU1_code <- NULL
  } else {
  }
  
  # demographic data machting (children & teacher)
  dfsexBAFU1 <- data.frame(subset(data, subset = data$BA_sex != data$FU1_sex, select = c(UUID_code, BA_sex, FU1_sex)))
  if (length(dfsexBAFU1$UUID_code) != 0){
    dfsexBAFU1$pat <- paste0((paste0("BA_sex=", dfsexBAFU1$BA_sex)), sep = " vs. ", (paste0("FU1_sex=", dfsexBAFU1$FU1_sex)))
    dfsexBAFU1$BA_sex <- NULL
    dfsexBAFU1$FU1_sex <- NULL
  } else {
  }
  
  dfschtypeBAFU1 <- data.frame(subset(data, data$BA_schtype != data$FU1_schtype, select = c(UUID_code, BA_schtype, FU1_schtype)))
  if (length(dfschtypeBAFU1$UUID_code) != 0){
    dfschtypeBAFU1$pat <- paste0((paste0("BA_schtype=", dfschtypeBAFU1$BA_schtype)), sep = " vs. ", (paste0("FU1_schtype=", dfschtypeBAFU1$FU1_schtype)))
    dfschtypeBAFU1$BA_schtype <- NULL
    dfschtypeBAFU1$FU1_schtype <- NULL
  } else {
  }
  
  dflocBAFU1 <- data.frame(subset(data, data$BA_loc != data$FU1_loc, select = c(UUID_code, BA_loc, FU1_loc)))
  if (length(dflocBAFU1$UUID_code) != 0){
    dflocBAFU1$pat <- paste0((paste0("BA_loc=", dflocBAFU1$BA_loc)), sep = " vs. ", (paste0("FU1_loc=", dflocBAFU1$FU1_loc)))
    dflocBAFU1$BA_loc <- NULL
    dflocBAFU1$FU1_loc <- NULL
  } else {
  }
  
  dfcountryBAFU1 <- data.frame(subset(data, data$BA_country != data$FU1_country, select = c(UUID_code, BA_country, FU1_country)))
  if (length(dfcountryBAFU1$UUID_code) != 0){
    dfcountryBAFU1$pat <- paste0((paste0("BA_country=", dfcountryBAFU1$BA_country)), sep = " vs. ", (paste0("FU1_country=", dfcountryBAFU1$FU1_country)))
    dfcountryBAFU1$BA_country <- NULL
    dfcountryBAFU1$FU1_country <- NULL
  } else {
  }
  
  dfageBAFU1 <- data.frame(subset(data, data$BA_age > data$FU1_age, select = c(UUID_code, BA_age, FU1_age)))
  if (length(dfageBAFU1$UUID_code) != 0){
    dfageBAFU1$pat <- paste0((paste0("BA_age=", dfageBAFU1$BA_age)), sep = " vs. ", (paste0("FU1_age=", dfageBAFU1$FU1_age)))
    dfageBAFU1$BA_age <- NULL
    dfageBAFU1$FU1_age <- NULL
  } else {
  }
 
  if (persmatch == "Teacher") {
  dfacBAFU1 <- data.frame(subset(data, data$BA_ac_qual != data$FU1_ac_qual, select = c(UUID_code, BA_ac_qual, FU1_ac_qual)))
  if (length(dfacBAFU1$UUID_code) != 0){
    dfacBAFU1$pat <- paste0((paste0("BA_ac_qual=", dfacBAFU1$BA_ac_qual)), sep = " vs. ", (paste0("FU1_ac_qual=", dfacBAFU1$FU1_ac_qual)))
    dfacBAFU1$BA_ac_qual <- NULL
    dfacBAFU1$FU1_ac_qual <- NULL
  } else {
  }
  
  dfyrstBAFU1 <- data.frame(subset(data, data$BA_yrs_t > data$FU1_yrs_t, select = c(UUID_code, BA_yrs_t, FU1_yrs_t)))
  if (length(dfyrstBAFU1$UUID_code) != 0){
    dfyrstBAFU1$pat <- paste0((paste0("BA_yrs_t=", dfyrstBAFU1$BA_yrs_t)), sep = " vs. ", (paste0("FU1_yrs_t=", dfyrstBAFU1$FU1_yrs_t)))
    dfyrstBAFU1$BA_yrs_t <- NULL
    dfyrstBAFU1$FU1_yrs_t <- NULL
  } else {
  }
  
  dfyrsschBAFU1 <- data.frame(subset(data, data$BA_yrs_sch > data$FU1_yrs_sch, select = c(UUID_code, BA_yrs_sch, FU1_yrs_sch)))
  if (length(dfyrsschBAFU1$UUID_code) != 0){
    dfyrsschBAFU1$pat <- paste0((paste0("BA_yrs_sch=", dfyrsschBAFU1$BA_yrs_sch)), sep = " vs. ", (paste0("FU1_yrs_sch=", dfyrsschBAFU1$FU1_yrs_sch)))
    dfyrsschBAFU1$BA_yrs_sch <- NULL
    dfyrsschBAFU1$FU1_yrs_sch <- NULL
  } else {
  }
  
  dfbiocBAFU1 <- data.frame(subset(data, data$BA_bioc > data$FU1_bioc, select = c(UUID_code, BA_bioc, FU1_bioc)))
  if (length(dfbiocBAFU1$UUID_code) != 0){
    dfbiocBAFU1$pat <- paste0((paste0("BA_bioc=", dfbiocBAFU1$BA_bioc)), sep = " vs. ", (paste0("FU1_bioc=", dfbiocBAFU1$FU1_bioc)))
    dfbiocBAFU1$BA_bioc <- NULL
    dfbiocBAFU1$FU1_bioc <- NULL
  } else {
  }
  } else { 
  }
  
  # combinde data sets
  dfcombBAFU1 <- data.frame(code = character(), pat = character())
  
  if (length(dfcodeBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfcodeBAFU1)
  }
  if (length(dfsexBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfsexBAFU1)
  } 
  if (length(dfschtypeBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfschtypeBAFU1)
  } 
  if (length(dflocBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dflocBAFU1)
  }
  if (length(dfcountryBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfcountryBAFU1)
  } else if (length(dfageBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfageBAFU1)
  } 
  if (persmatch == "Teacher") { 
  if (length(dfacBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfacBAFU1)
  }
  if (length(dfyrstBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfyrstBAFU1)
  }
  if (length(dfyrsschBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfyrsschBAFU1)
  }
  if (length(dfbiocBAFU1$UUID_code) != 0){
    dfcombBAFU1 <- rbind(dfcombBAFU1, dfbiocBAFU1)
  } else {
  }
 } else { 
    }
    
  
  if (length(dfcombBAFU1$UUID_code) != 0){
    duplcodeBAFU1 <- subset(dfcombBAFU1$UUID_code, duplicated(dfcombBAFU1$UUID_code, na.action = stats::na.omit)) # test
    if(length(duplcodeBAFU1) > 0){
      dfcomb2BAFU1 <- dfcombBAFU1[dfcombBAFU1$UUID_code %in% duplcodeBAFU1, ]
    } else {
      dfcomb2BAFU1 <- dfcombBAFU1
    }
    pattmpBAFU1 <- stats::as.formula(paste(quote(pat), "UUID_code", sep = " ~ "))
    listtmpBAFU1 <- list(x = pattmpBAFU1, data = unique(dfcomb2BAFU1), FUN = paste, collapse = ", ")
    dfcomb3BAFU1 <- do.call("aggregate", listtmpBAFU1)
    dfcomb2BAFU1$pat2 <- dfcomb3BAFU1$pat[match(dfcomb2BAFU1$UUID_code, dfcomb3BAFU1$UUID_code)]
    dfcombBAFU1$pat2 <- dfcomb2BAFU1$pat2[match(dfcombBAFU1$UUID_code, dfcomb2BAFU1$UUID_code)]
    dfcombBAFU1$pat2 <- ifelse(is.na(dfcombBAFU1$pat2) == TRUE, dfcombBAFU1$pat, dfcombBAFU1$pat2)
    dfcombBAFU1$pat <- dfcombBAFU1$pat2
    dfcombBAFU1$pat2 <- NULL
    dfcombBAFU1 <- dfcombBAFU1[!duplicated(dfcombBAFU1), ]
    #dfcombBAFU1$pat <- sub(":,", ":", dfcombBAFU1$pat)
    names(dfcombBAFU1) <- c("UUID_code", "missmatchBAFU1")
  } else {
    print("no misstatches between BA & FU1")
  }  
  
   person <- if (persmatch == "Child") { 
    dfcombfinal <- merge(x = dfcombBAFU1, y = data[ , c("UUID_code", "BA_c_code", "FU1_c_code")], by = "UUID_code", all.x=TRUE)
  } else if (persmatch == "Teacher") {
    dfcombfinal <- merge(x = dfcombBAFU1, y = data[ , c("UUID_code", "BA_t_code", "FU1_t_code")], by = "UUID_code", all.x=TRUE)
  } else { 
  }
  dfcombfinal <- dfcombfinal[, c(1,3:4,2)]  
  
  # code matching BA & FU2 (children | teacher)
  if (any(grepl("^FU2_", names(data))==TRUE)==TRUE){
  person <- if (persmatch == "Child") {
    dfcodeBAFU2 <- data.frame(subset(data, data$BA_c_code!= data$FU2_c_code, select = c(UUID_code, BA_c_code, FU2_c_code)))
  } else if (persmatch == "Teacher") {
    dfcodeBAFU2 <- data.frame(subset(data, data$BA_t_code!= data$FU2_t_code, select = c(UUID_code, BA_t_code, FU2_t_code)))
  } else { 
  }
  if (length(dfcodeBAFU2$UUID_code) != 0){
    names(dfcodeBAFU2) <- c("UUID_code", "BA_code", "FU2_code")
    dfcodeBAFU2$pat <- paste0((paste0("BA_code=", dfcodeBAFU2$BA_code)), sep = " vs. ", (paste0("FU2_code=", dfcodeBAFU2$FU2_code)))
    dfcodeBAFU2$BA_code <- NULL
    dfcodeBAFU2$FU2_code <- NULL
  } else {
  }
  
  # demographic data machting (children & teacher)
  dfsexBAFU2 <- data.frame(subset(data, subset = data$BA_sex != data$FU2_sex, select = c(UUID_code, BA_sex, FU2_sex)))
  if (length(dfsexBAFU2$UUID_code) != 0){
    dfsexBAFU2$pat <- paste0((paste0("BA_sex=", dfsexBAFU2$BA_sex)), sep = " vs. ", (paste0("FU2_sex=", dfsexBAFU2$FU2_sex)))
    dfsexBAFU2$BA_sex <- NULL
    dfsexBAFU2$FU2_sex <- NULL
  } else {
  }
  
  dfschtypeBAFU2 <- data.frame(subset(data, data$BA_schtype != data$FU2_schtype, select = c(UUID_code, BA_schtype, FU2_schtype)))
  if (length(dfschtypeBAFU2$UUID_code) != 0){
    dfschtypeBAFU2$pat <- paste0((paste0("BA_schtype=", dfschtypeBAFU2$BA_schtype)), sep = " vs. ", (paste0("FU2_schtype=", dfschtypeBAFU2$FU2_schtype)))
    dfschtypeBAFU2$BA_schtype <- NULL
    dfschtypeBAFU2$FU2_schtype <- NULL
  } else {
  }
  
  dflocBAFU2 <- data.frame(subset(data, data$BA_loc != data$FU2_loc, select = c(UUID_code, BA_loc, FU2_loc)))
  if (length(dflocBAFU2$UUID_code) != 0){
    dflocBAFU2$pat <- paste0((paste0("BA_loc=", dflocBAFU2$BA_loc)), sep = " vs. ", (paste0("FU2_loc=", dflocBAFU2$FU2_loc)))
    dflocBAFU2$BA_loc <- NULL
    dflocBAFU2$FU2_loc <- NULL
  } else {
  }
  
  dfcountryBAFU2 <- data.frame(subset(data, data$BA_country != data$FU2_country, select = c(UUID_code, BA_country, FU2_country)))
  if (length(dfcountryBAFU2$UUID_code) != 0){
    dfcountryBAFU2$pat <- paste0((paste0("BA_country=", dfcountryBAFU2$BA_country)), sep = " vs. ", (paste0("FU2_country=", dfcountryBAFU2$FU2_country)))
    dfcountryBAFU2$BA_country <- NULL
    dfcountryBAFU2$FU2_country <- NULL
  } else {
  }
  
  dfageBAFU2 <- data.frame(subset(data, data$BA_age > data$FU2_age, select = c(UUID_code, BA_age, FU2_age)))
  if (length(dfageBAFU2$UUID_code) != 0){
    dfageBAFU2$pat <- paste0((paste0("BA_age=", dfageBAFU2$BA_age)), sep = " vs. ", (paste0("FU2_age=", dfageBAFU2$FU2_age)))
    dfageBAFU2$BA_age <- NULL
    dfageBAFU2$FU2_age <- NULL
  } else {
  }

  if (persmatch == "Teacher") {
  dfacBAFU2 <- data.frame(subset(data, data$BA_ac_qual != data$FU2_ac_qual, select = c(UUID_code, BA_ac_qual, FU2_ac_qual)))
  if (length(dfacBAFU2$UUID_code) != 0){
    dfacBAFU2$pat <- paste0((paste0("BA_ac_qual=", dfacBAFU2$BA_ac_qual)), sep = " vs. ", (paste0("FU2_ac_qual=", dfacBAFU2$FU2_ac_qual)))
    dfacBAFU2$BA_ac_qual <- NULL
    dfacBAFU2$FU2_ac_qual <- NULL
  } else {
  }
  
  dfyrstBAFU2 <- data.frame(subset(data, data$BA_yrs_t > data$FU2_yrs_t, select = c(UUID_code, BA_yrs_t, FU2_yrs_t)))
  if (length(dfyrstBAFU2$UUID_code) != 0){
    dfyrstBAFU2$pat <- paste0((paste0("BA_yrs_t=", dfyrstBAFU2$BA_yrs_t)), sep = " vs. ", (paste0("FU2_yrs_t=", dfyrstBAFU2$FU2_yrs_t)))
    dfyrstBAFU2$BA_yrs_t <- NULL
    dfyrstBAFU2$FU2_yrs_t <- NULL
  } else {
  }
  
  dfyrsschBAFU2 <- data.frame(subset(data, data$BA_yrs_sch > data$FU2_yrs_sch, select = c(UUID_code, BA_yrs_sch, FU2_yrs_sch)))
  if (length(dfyrsschBAFU2$UUID_code) != 0){
    dfyrsschBAFU2$pat <- paste0((paste0("BA_yrs_sch=", dfyrsschBAFU2$BA_yrs_sch)), sep = " vs. ", (paste0("FU2_yrs_sch=", dfyrsschBAFU2$FU2_yrs_sch)))
    dfyrsschBAFU2$BA_yrs_sch <- NULL
    dfyrsschBAFU2$FU2_yrs_sch <- NULL
  } else {
  }
  
  dfbiocBAFU2 <- data.frame(subset(data, data$BA_bioc > data$FU2_bioc, select = c(UUID_code, BA_bioc, FU2_bioc)))
  if (length(dfbiocBAFU2$UUID_code) != 0){
    dfbiocBAFU2$pat <- paste0((paste0("BA_bioc=", dfbiocBAFU2$BA_bioc)), sep = " vs. ", (paste0("FU2_bioc=", dfbiocBAFU2$FU2_bioc)))
    dfbiocBAFU2$BA_bioc <- NULL
    dfbiocBAFU2$FU2_bioc <- NULL
  } else {
  }
 } else { 
  }
  

  # combinde data sets
  dfcombBAFU2 <- data.frame(code = character(), pat = character())
  
  if (length(dfcodeBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfcodeBAFU2)
  }
  if (length(dfsexBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfsexBAFU2)
  } 
  if (length(dfschtypeBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfschtypeBAFU2)
  } 
  if (length(dflocBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dflocBAFU2)
  }
  if (length(dfcountryBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfcountryBAFU2)
  } else if (length(dfageBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfageBAFU2)
  } 
  if (persmatch == "Teacher") { 
  if (length(dfacBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfacBAFU2)
  }
  if (length(dfyrstBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfyrstBAFU2)
  }
  if (length(dfyrsschBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfyrsschBAFU2)
  }
  if (length(dfbiocBAFU2$UUID_code) != 0){
    dfcombBAFU2 <- rbind(dfcombBAFU2, dfbiocBAFU2)
   } else {
   }
} else { 
    }
  
  if (length(dfcombBAFU2$UUID_code) != 0){
    duplcodeBAFU2 <- subset(dfcombBAFU2$UUID_code, duplicated(dfcombBAFU2$UUID_code, na.action = stats::na.omit)) # test
    if(length(duplcodeBAFU2) > 0){
      dfcomb2BAFU2 <- dfcombBAFU2[dfcombBAFU2$UUID_code %in% duplcodeBAFU2, ]
    } else {
      dfcomb2BAFU2 <- dfcombBAFU2
    }
    pattmpBAFU2 <- stats::as.formula(paste(quote(pat), "UUID_code", sep = " ~ "))
    listtmpBAFU2 <- list(x = pattmpBAFU2, data = unique(dfcomb2BAFU2), FUN = paste, collapse = ", ")
    dfcomb3BAFU2 <- do.call("aggregate", listtmpBAFU2)
    dfcomb2BAFU2$pat2 <- dfcomb3BAFU2$pat[match(dfcomb2BAFU2$UUID_code, dfcomb3BAFU2$UUID_code)]
    dfcombBAFU2$pat2 <- dfcomb2BAFU2$pat2[match(dfcombBAFU2$UUID_code, dfcomb2BAFU2$UUID_code)]
    dfcombBAFU2$pat2 <- ifelse(is.na(dfcombBAFU2$pat2) == TRUE, dfcombBAFU2$pat, dfcombBAFU2$pat2)
    dfcombBAFU2$pat <- dfcombBAFU2$pat2
    dfcombBAFU2$pat2 <- NULL
    dfcombBAFU2 <- dfcombBAFU2[!duplicated(dfcombBAFU2), ]
    # dfcombBAFU2$pat <- sub(":,", ":", dfcombBAFU2$pat)
    names(dfcombBAFU2) <- c("UUID_code", "missmatchBAFU2")
  } else {
    print("no misstatches between BA & FU2")
  }  
  
  # code matching FU1 & FU2 (children | teacher)
  person <- if (persmatch == "Child") {
    dfcodeFU1FU2 <- data.frame(subset(data, data$FU1_c_code!= data$FU2_c_code, select = c(UUID_code, FU1_c_code, FU2_c_code)))
  } else if (persmatch == "Teacher") {
    dfcodeFU1FU2 <- data.frame(subset(data, data$FU1_t_code!= data$FU2_t_code, select = c(UUID_code, FU1_t_code, FU2_t_code)))
  } else { 
  }
  if (length(dfcodeFU1FU2$UUID_code) != 0){
    names(dfcodeFU1FU2) <- c("UUID_code", "FU1_code", "FU2_code")
    
    dfcodeFU1FU2$pat <- paste0((paste0("FU1_code=", dfcodeFU1FU2$FU1_code)), sep = " vs. ", (paste0("FU2_code=", dfcodeFU1FU2$FU2_code)))
    dfcodeFU1FU2$FU1_code <- NULL
    dfcodeFU1FU2$FU2_code <- NULL
  } else {
  }
  
  # demographic data machting (children & teacher)
  dfsexFU1FU2 <- data.frame(subset(data, subset = data$FU1_sex != data$FU2_sex, select = c(UUID_code, FU1_sex, FU2_sex)))
  if (length(dfsexFU1FU2$UUID_code) != 0){
    dfsexFU1FU2$pat <- paste0((paste0("FU1_sex=", dfsexFU1FU2$FU1_sex)), sep = " vs. ", (paste0("FU2_sex=", dfsexFU1FU2$FU2_sex)))
    dfsexFU1FU2$FU1_sex <- NULL
    dfsexFU1FU2$FU2_sex <- NULL
  } else {
  }
  
  dfschtypeFU1FU2 <- data.frame(subset(data, data$FU1_schtype != data$FU2_schtype, select = c(UUID_code, FU1_schtype, FU2_schtype)))
  if (length(dfschtypeFU1FU2$UUID_code) != 0){
    dfschtypeFU1FU2$pat <- paste0((paste0("FU1_schtype=", dfschtypeFU1FU2$FU1_schtype)), sep = " vs. ", (paste0("FU2_schtype=", dfschtypeFU1FU2$FU2_schtype)))
    dfschtypeFU1FU2$FU1_schtype <- NULL
    dfschtypeFU1FU2$FU2_schtype <- NULL
  } else {
  }
  
  dflocFU1FU2 <- data.frame(subset(data, data$FU1_loc != data$FU2_loc, select = c(UUID_code, FU1_loc, FU2_loc)))
  if (length(dflocFU1FU2$UUID_code) != 0){
    dflocFU1FU2$pat <- paste0((paste0("FU1_loc=", dflocFU1FU2$FU1_loc)), sep = " vs. ", (paste0("FU2_loc=", dflocFU1FU2$FU2_loc)))
    dflocFU1FU2$FU1_loc <- NULL
    dflocFU1FU2$FU2_loc <- NULL
  } else {
  }
  
  dfcountryFU1FU2 <- data.frame(subset(data, data$FU1_country != data$FU2_country, select = c(UUID_code, FU1_country, FU2_country)))
  if (length(dfcountryFU1FU2$UUID_code) != 0){
    dfcountryFU1FU2$pat <- paste0((paste0("FU1_country=", dfcountryFU1FU2$FU1_country)), sep = " vs. ", (paste0("FU2_country=", dfcountryFU1FU2$FU2_country)))
    dfcountryFU1FU2$FU1_country <- NULL
    dfcountryFU1FU2$FU2_country <- NULL
  } else {
  }
  
  dfageFU1FU2 <- data.frame(subset(data, data$FU1_age > data$FU2_age, select = c(UUID_code, FU1_age, FU2_age)))
  if (length(dfageFU1FU2$UUID_code) != 0){
    dfageFU1FU2$pat <- paste0((paste0("FU1_age=", dfageFU1FU2$FU1_age)), sep = " vs. ", (paste0("FU2_age=", dfageFU1FU2$FU2_age)))
    dfageFU1FU2$FU1_age <- NULL
    dfageFU1FU2$FU2_age <- NULL
  } else {
  }
  if (persmatch == "Teacher") {
  dfacFU1FU2 <- data.frame(subset(data, data$FU1_ac_qual != data$FU2_ac_qual, select = c(UUID_code, FU1_ac_qual, FU2_ac_qual)))
  if (length(dfacFU1FU2$UUID_code) != 0){
    dfacFU1FU2$pat <- paste0((paste0("FU1_ac_qual=", dfacFU1FU2$FU1_ac_qual)), sep = " vs. ", (paste0("FU2_ac_qual=", dfacFU1FU2$FU2_ac_qual)))
    dfacFU1FU2$FU1_ac_qual <- NULL
    dfacFU1FU2$FU2_ac_qual <- NULL
  } else {
  }
  
  dfyrstFU1FU2 <- data.frame(subset(data, data$FU1_yrs_t > data$FU2_yrs_t, select = c(UUID_code, FU1_yrs_t, FU2_yrs_t)))
  if (length(dfyrstFU1FU2$UUID_code) != 0){
    dfyrstFU1FU2$pat <- paste0((paste0("FU1_yrs_t=", dfyrstFU1FU2$FU1_yrs_t)), sep = " vs. ", (paste0("FU2_yrs_t=", dfyrstFU1FU2$FU2_yrs_t)))
    dfyrstFU1FU2$FU1_yrs_t <- NULL
    dfyrstFU1FU2$FU2_yrs_t <- NULL
  } else {
  }
  
  dfyrsschFU1FU2 <- data.frame(subset(data, data$FU1_yrs_sch > data$FU2_yrs_sch, select = c(UUID_code, FU1_yrs_sch, FU2_yrs_sch)))
  if (length(dfyrsschFU1FU2$UUID_code) != 0){
    dfyrsschFU1FU2$pat <- paste0((paste0("FU1_yrs_sch=", dfyrsschFU1FU2$FU1_yrs_sch)), sep = " vs. ", (paste0("FU2_yrs_sch=", dfyrsschFU1FU2$FU2_yrs_sch)))
    dfyrsschFU1FU2$FU1_yrs_sch <- NULL
    dfyrsschFU1FU2$FU2_yrs_sch <- NULL
  } else {
  }
  
  dfbiocFU1FU2 <- data.frame(subset(data, data$FU1_bioc > data$FU2_bioc, select = c(UUID_code, FU1_bioc, FU2_bioc)))
  if (length(dfbiocFU1FU2$UUID_code) != 0){
    dfbiocFU1FU2$pat <- paste0((paste0("FU1_bioc=", dfbiocFU1FU2$FU1_bioc)), sep = " vs. ", (paste0("FU2_bioc=", dfbiocFU1FU2$FU2_bioc)))
    dfbiocFU1FU2$FU1_bioc <- NULL
    dfbiocFU1FU2$FU2_bioc <- NULL
  } else {
  }
} else { 
  }  
  # combinde data sets
  dfcombFU1FU2 <- data.frame(code = character(), pat = character())
  
  if (length(dfcodeFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfcodeFU1FU2)
  }
  if (length(dfsexFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfsexFU1FU2)
  } 
  if (length(dfschtypeFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfschtypeFU1FU2)
  } 
  if (length(dflocFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dflocFU1FU2)
  }
  if (length(dfcountryFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfcountryFU1FU2)
  } else if (length(dfageFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfageFU1FU2)
  } 
  if (persmatch == "Teacher") { 
  if (length(dfacFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfacFU1FU2)
  }
  if (length(dfyrstFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfyrstFU1FU2)
  }
  if (length(dfyrsschFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfyrsschFU1FU2)
  }
  if (length(dfbiocFU1FU2$UUID_code) != 0){
    dfcombFU1FU2 <- rbind(dfcombFU1FU2, dfbiocFU1FU2)
  } else {
  }
} else { 
    }
  
  if (length(dfcombFU1FU2$UUID_code) != 0){
    duplcodeFU1FU2 <- subset(dfcombFU1FU2$UUID_code, duplicated(dfcombFU1FU2$UUID_code, na.action = stats::na.omit)) # test
    if(length(duplcodeFU1FU2) > 0){
      dfcomb2FU1FU2 <- dfcombFU1FU2[dfcombFU1FU2$UUID_code %in% duplcodeFU1FU2, ]
    } else {
      dfcomb2FU1FU2 <- dfcombFU1FU2
    }
    pattmpFU1FU2 <- stats::as.formula(paste(quote(pat), "UUID_code", sep = " ~ "))
    listtmpFU1FU2 <- list(x = pattmpFU1FU2, data = unique(dfcomb2FU1FU2), FUN = paste, collapse = ", ")
    dfcomb3FU1FU2 <- do.call("aggregate", listtmpFU1FU2)
    dfcomb2FU1FU2$pat2 <- dfcomb3FU1FU2$pat[match(dfcomb2FU1FU2$UUID_code, dfcomb3FU1FU2$UUID_code)]
    dfcombFU1FU2$pat2 <- dfcomb2FU1FU2$pat2[match(dfcombFU1FU2$UUID_code, dfcomb2FU1FU2$UUID_code)]
    dfcombFU1FU2$pat2 <- ifelse(is.na(dfcombFU1FU2$pat2) == TRUE, dfcombFU1FU2$pat, dfcombFU1FU2$pat2)
    dfcombFU1FU2$pat <- dfcombFU1FU2$pat2
    dfcombFU1FU2$pat2 <- NULL
    dfcombFU1FU2 <- dfcombFU1FU2[!duplicated(dfcombFU1FU2), ]
    #dfcombFU1FU2$pat <- sub(":,", ":", dfcombFU1FU2$pat)
    names(dfcombFU1FU2) <- c("UUID_code", "missmatchFU1FU2")
  } else {
    print("no misstatches between FU1 & FU2")
  }  
  
  # combine dfcomb* sets 
  dfcombBAFU1FU2 <- dplyr::full_join(dfcombBAFU1, dfcombBAFU2, by = "UUID_code")
  dfcombBAFU1FU2 <- dplyr::full_join(dfcombBAFU1FU2, dfcombFU1FU2, by = "UUID_code")
  person <- if (persmatch == "Child") { 
    dfcombBAFU1FU2 <- merge(x = dfcombBAFU1FU2, y = data[ , c("UUID_code", "BA_c_code", "FU1_c_code", "FU2_c_code")], by = "UUID_code", all.x=TRUE)
  } else if (persmatch == "Teacher") {
    dfcombBAFU1FU2 <- merge(x = dfcombBAFU1FU2, y = data[ , c("UUID_code", "BA_t_code", "FU1_t_code", "FU2_t_code")], by = "UUID_code", all.x=TRUE)
  } else { 
  }
  dfcombfinal <- dfcombBAFU1FU2[, c(1,5:7,2:4)]
  } else {
  }  
  
assign("QCcodesPhase", value = dfcombfinal, envir = .GlobalEnv)
if (save == "TRUE") {
  file <- paste(file)
  utils::write.csv2(QCcodesPhase, file = file, row.names = FALSE)
} else {
}
print("successfully created data.frame 'QCcodesPhase'")
}
