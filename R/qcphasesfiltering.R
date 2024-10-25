#' quality control that filters baseline participant codes not matching participation code & demographic info between assessement phases (BA, FU1)
#'
#' @description this function filters cases (rows, baseline participation codes) that do not match participation code & demographic info between assessement phases 
#'
#' @param data data frame 
#' @param BA_code character of variable in data frame that hold participants codes of Baseline assessement 
#' @param FU1_code character of variable in data frame that hold participants codes of Follow Up 1 assessement
#' @param person character specifying the person participants code belongs to. Must be one of "Teacher" or "Child"
#' @param save logical. If TRUE returned data frame will be saved as .csv with ; separation
#' @param file optional. absolute path including file name and appendix .csv in "" where returned data set should be stored
#'
#' @return data frame of baseline participant codes not not matching participation code & demographic info between assessement phases (BA, FU1)
#'
#' @export

qcphases.filtering <- function(data, BA_code, FU1_code, person = c("Teacher", "Child"), save, file)
{
  persmatch <- match.arg(person)

# code matching BA & FU1 (children | teacher)
person <- if (persmatch == "Child") {
  dfcode <- data.frame(subset(data[[BA_code]], data[[BA_code]]!= data[[FU1_code]]))
  } else if (persmatch == "Teacher") {
    dfcode <- data.frame(subset(data[[BA_code]], data[[BA_code]]!= data[[FU1_code]]))
  } else { 
 }
    names(dfcode) <- "code"
    if (length(dfcode$code) != 0){
    dfcode$pat <- rep("code", nrow(dfcode))
    } else {
    }

# demographic data machting (children & teacher)
dfsex <- data.frame(subset(data[[BA_code]], data$BA_sex != data$FU1_sex))
names(dfsex) <- "code"
if (length(dfsex$code) != 0){
  dfsex$pat <- rep("sex", nrow(dfsex))
} else {
}

dfschtype <- data.frame(subset(data[[BA_code]], data$BA_schtype != data$FU1_schtype))
names(dfschtype) <- "code"
if (length(dfschtype$code) != 0){
  dfschtype$pat <- rep("schtype", nrow(dfschtype))
} else {
}

dfloc <- data.frame(subset(data[[BA_code]], data$BA_loc != data$FU1_loc))
names(dfloc) <- "code"
if (length(dfloc$code) != 0){
  dfloc$pat <- rep("loc", nrow(dfloc))
} else {
}

dfcountry <- data.frame(subset(data[[BA_code]], data$BA_country != data$FU1_country))
names(dfcountry) <- "code"
if (length(dfcountry$code) != 0){
  dfcountry$pat <- rep("country", nrow(dfcountry))
} else {
}

dfage <- data.frame(subset(data[[BA_code]], data$BA_age > data$FU1_age))
names(dfage) <- "code"
if (length(dfage$code) != 0){
  dfage$pat <- rep("age", nrow(dfage))
} else {
}

# demographic data matching (teacher)
dfreligion <- data.frame(subset(data[[BA_code]], data$BA_religion != data$FU1_religion))
names(dfreligion) <- "code"
if (length(dfreligion$code) != 0){
  dfreligion$pat <- rep("religion", nrow(dfreligion))
} else {
}

dfethn <- data.frame(subset(data[[BA_code]], data$BA_ethn != data$FU1_ethn))
names(dfethn) <- "code"
if (length(dfethn$code) != 0){
  dfethn$pat <- rep("ethn", nrow(dfethn))
} else {
}

dfac <- data.frame(subset(data[[BA_code]], data$BA_ac_qual != data$FU1_ac_qual))
names(dfac) <- "code"
if (length(dfac$code) != 0){
  dfac$pat <- rep("ac_qual", nrow(dfac))
} else {
}

dfyrst <- data.frame(subset(data[[BA_code]], data$BA_yrs_t > data$FU1_yrs_t))
names(dfyrst) <- "code"
if (length(dfyrst$code) != 0){
  dfyrst$pat <- rep("yrs_t", nrow(dfyrst))
} else {
}

dfyrssch <- data.frame(subset(data[[BA_code]], data$BA_yrs_sch > data$FU1_yrs_sch))
names(dfyrssch) <- "code"
if (length(dfyrssch$code) != 0){
  dfyrssch$pat <- rep("yrs_sch", nrow(dfyrssch))
} else {
}

dfbioc <- data.frame(subset(data[[BA_code]], data$BA_bioc > data$FU1_bioc))
names(dfbioc) <- "code"
if (length(dfbioc$code) != 0){
  dfbioc$pat <- rep("bioc", nrow(dfbioc))
} else {
}

data$BA_FU1_interviewBA <- data$FU1_interviewBA
BA_missingdata <- data[data$FU1_interviewBA == 0, ]
BA_missingdata <- BA_missingdata[!is.na(BA_missingdata$FU1_interviewBA), ]
BA_missingdata <- BA_missingdata[, c(length(BA_missingdata), 1:length(BA_missingdata)-1)]
BA_missingdata <- BA_missingdata[, grep("^BA_", names(BA_missingdata))]
dfmissingBA <- data.frame(subset(BA_missingdata[[BA_code]], (rowSums(!is.na(BA_missingdata[c(3:length(BA_missingdata))]) == TRUE) > 0)))
names(dfmissingBA) <- "code"
if (length(dfmissingBA$code) != 0){
  dfmissingBA$pat <- rep("BA data though FU1 interviewed only", nrow(dfmissingBA))
} else {
}


# FU1dropout <- data[data$FU1_Dropout == 1, ]
# FU1dropout <- FU1dropout[!is.na(FU1dropout$FU1_Dropout), ]
# FU1dropout <- FU1dropout[, grep("^FU1_", names(FU1dropout))]
# dfFU1dropout <- data.frame(subset(FU1dropout[[BA_code]], (rowSums(!is.na(FU1dropout[c(2:length(FU1dropout))]) == TRUE) > 0)))
# names(dfFU1dropout) <- "code"
# if (length(dfFU1dropout$code) != 0){
#   dfFU1dropout$pat <- rep("FU1 data though BA interviewed only", nrow(dfFU1dropout))
# } else {
# }

# combinde data sets
dfcomb <- data.frame(code = character(), pat = character())

if (length(dfcode$code) != 0){
  dfcomb <- rbind(dfcomb, dfcode)
  }
if (length(dfsex$code) != 0){
    dfcomb <- rbind(dfcomb, dfsex)
  } 
if (length(dfschtype$code) != 0){
    dfcomb <- rbind(dfcomb, dfschtype)
  } 
if (length(dfloc$code) != 0){
    dfcomb <- rbind(dfcomb, dfloc)
  }
if (length(dfcountry$code) != 0){
    dfcomb <- rbind(dfcomb, dfcountry)
  } else if (length(dfage$code) != 0){
    dfcomb <- rbind(dfcomb, dfage)
  } 
if (length(dfreligion$code) != 0){
    dfcomb <- rbind(dfcomb, dfreligion)
  } 
if (length(dfethn$code) != 0){
    dfcomb <- rbind(dfcomb, dfethn)
  } 
if (length(dfac$code) != 0){
    dfcomb <- rbind(dfcomb, dfac)
  }
if (length(dfyrst$code) != 0){
    dfcomb <- rbind(dfcomb, dfyrst)
  }
if (length(dfyrssch$code) != 0){
    dfcomb <- rbind(dfcomb, dfyrssch)
  }
if (length(dfbioc$code) != 0){
    dfcomb <- rbind(dfcomb, dfbioc)
  }
if (length(dfmissingBA$code) != 0){
    dfcomb <- rbind(dfcomb, dfmissingBA)
#  } 
# if (length(dfFU1dropout$code) != 0){
#     dfcomb <- rbind(dfcomb, dfFU1dropout)
  } else {
  }

if (length(dfcomb$code) != 0){
duplcode <- subset(dfcomb$code, duplicated(dfcomb$code, na.action = stats::na.omit)) # test
if(length(duplcode) > 0){
  dfcomb2 <- dfcomb[dfcomb$code %in% duplcode, ]
} else {
  dfcomb2 <- dfcomb
}
pattmp <- stats::as.formula(paste(quote(pat), "code", sep = " ~ "))
listtmp <- list(x = pattmp, data = unique(dfcomb2), FUN = paste, collapse = ", ")
dfcomb3 <- do.call("aggregate", listtmp)
dfcomb2$pat2 <- dfcomb3$pat[match(dfcomb2$code, dfcomb3$code)]
dfcomb$pat2 <- dfcomb2$pat2[match(dfcomb$code, dfcomb2$code)]
dfcomb$pat2 <- ifelse(is.na(dfcomb$pat2) == TRUE, dfcomb$pat, dfcomb$pat2)
dfcomb$pat <- dfcomb$pat2
dfcomb$pat2 <- NULL
dfcomb <- dfcomb[!duplicated(dfcomb), ]
dfcomb$pat <- sub(":,", ":", dfcomb$pat)
names(dfcomb) <- c("BA_code", "missmatchBAFU1")
} else {
  print("no misstatches between BA & FU1")
}  
assign("QCcodesPhase", value = dfcomb, envir = .GlobalEnv)
if (save == "TRUE") {
  file <- paste(file)
  utils::write.csv2(QCcodesPhase, file = file, row.names = FALSE)
} else {
}
print("successfully created data.frame 'QCcodesPhase'")
}
