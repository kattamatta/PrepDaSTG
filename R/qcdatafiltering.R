#' quality control that filters participant codes not matching required coding scheme
#'
#' @description this function filters cases (rows) that do not follow the required coding scheme set by the research team of AE17
#'
#' @param data data frame containing variables of participants' codes for quality control
#' @param level2 logical to indicate data frame holds level 2 data. If TRUE school code instead of participant code will be quality controlled. If TRUE no @param code needed.
#' @param code character of variable in data frame that hold participants codes - note for numeracy & literacy data: pnl_code & snl_code to to be converted in code as the following data$code <- ifelse(data$pnl_code == "", data$snl_code, data$pnl_code) before hand!
#' @param code_cg character of variable in data frame holding caregivers code - only for twin combinde data set. Has to be specified when @param person = Caregivers
#' @param school character of variable in data frame holding school code
#' @param person character specifying the person participants code belongs to. Must be one of "Teacher", "Child", "Mother", or "Caregivers".
#' @param country character specifying the country participant codes were assessed. Must be one of "Ghana", "Tanzania", "Uganda", "Haiti" or "Pakistan".
#' @param save logical. If TRUE returned data frame will be saved as .csv with ; separation
#' @param file optional. absolute path including file name and appendix .csv in "" where returned data set should be stored
#'
#' @return data frame of participant codes not matching required participant coding scheme with all variables plus added "pattern" column holding information on discrepancy between participant code scheme and required scheme
#' @export

qcdata.filtering <- function(data, level2, code, code_cg, school, person = c("Teacher", "Child", "Mother", "Father", "Caregivers"), country = c("Ghana", "Tanzania", "Uganda", "Haiti", "Pakistan"),  save, file){
  # duplicate codes
  if (level2 == "TRUE"){
    duplsch <- subset(data[[school]], duplicated(data[[school]]))

    if (length(duplsch) == 0){
      print("no duplicate schools")

    } else {
      dfduplsch <- data[data[[school]] %in% duplsch, ]
      dfduplsch$pat <- rep("duplicated school codes", nrow(dfduplsch))
      keepsch <- colnames(dfduplsch[, grepl(names(dfduplsch), pattern = c("pat|code|SbjNum|Date|Srvyr|Duration|Upload|RvwTime|SrvyrComment|Complete|VStart|VEnd|start|country|phase|intrw|sch|date|schtype|loc|sex|age|relstat|religion|ethn|ac_qual|yrs_t|mths_t|yrs_sch|mths_sch|hrs_sch|nr_std|nr_std_c|fam|fam_c|bioc|income|otjob"))])
      dfduplschS <- subset(dfduplsch, select = keepsch)
      dfduplschS$pattern <- dfduplschS$pat
      dfduplschS$pat <- NULL
      assign("QCsch", value = dfduplschS, envir = .GlobalEnv)
      if (save == "TRUE"){
        #filename <- paste("QC_school_STGexport", ".csv", sep = "")
        file <- paste(file)
        utils::write.csv2(QCsch, file = file, row.names = FALSE)

      } else {
      }
      print("successfully created data.frame 'QCsch'")
    }
  } else {
    persmatch <- match.arg(person)
    countrmatch <- match.arg(country)

    if (missing(code_cg)){
      duplcode <- subset(data[[code]], duplicated(data[[code]]))
      dfduplcode <- data[data[[code]] %in% duplcode, ]
      dfduplcode$pat <- rep("duplicated codes", nrow(dfduplcode))
    } else {
      duplcode <- subset(data[[code]], duplicated(data))
      dfduplcode <- data[data[[code]] %in% duplcode, ]
      dfduplcode$pat <- rep("duplicated codes (child)", nrow(dfduplcode))
      duplcode_cg <- subset(data[[code_cg]], duplicated(data))
      dfduplcode_cg <- data[data[[code_cg]] %in% duplcode_cg, ]
      dfduplcode_cg$pat <- rep("duplicated codes (caregivers)", nrow(dfduplcode_cg))
      dfduplcode <- rbind(dfduplcode, dfduplcode_cg)
    }

    # general scheme
    if (missing(code_cg)) {
      scheme <- subset(data[[code]], !grepl(data[[code]], pattern = "([[:alpha:]])([[:digit:]]{2})([[:alpha:]])([[:digit:]]{2})"))
      dfscheme <- data[data[[code]] %in% scheme, ]
      dfscheme$pat <- rep("code scheme:", nrow(dfscheme))
    } else {
      scheme <- subset(data[[code]], !grepl(data[[code]], pattern = "([[:alpha:]])([[:digit:]]{2})([[:alpha:]])([[:digit:]]{2})"))
      scheme <- subset(scheme, !is.na(scheme))
      dfscheme <- data[data[[code]] %in% scheme, ]
      dfscheme$pat <- rep("code scheme (child):", nrow(dfscheme))

      scheme_cg <- subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "([[:alpha:]])([[:digit:]]{2})([[:alpha:]])([[:digit:]]{2})"))
      dfscheme_cg <- data[data[[code_cg]] %in% scheme_cg, ]
      dfscheme_cg$pat <- rep("code scheme (caregivers):", nrow(dfscheme_cg))
      #dfscheme <- rbind(dfscheme, dfscheme_cg)
    }

    # capital letters
    if (missing(code_cg)) {
      cap <- subset(data[[code]], grepl(data[[code]], pattern = "[[:upper:]]"))
      dfcap <- data[data[[code]] %in% cap, ]
      dfcap$pat <- rep("capital letter", nrow(dfcap))
    } else {
      cap <- subset(data[[code]], grepl(data[[code]], pattern = "[[:upper:]]"))
      dfcap <- data[data[[code]] %in% cap, ]
      dfcap$pat <- rep("capital letter (child)", nrow(dfcap))

      cap_cg <- subset(data[[code_cg]], grepl(data[[code_cg]], pattern = "[[:upper:]]"))
      dfcap_cg <- data[data[[code_cg]] %in% cap_cg, ]
      dfcap_cg$pat <- rep("capital letter (caregivers)", nrow(dfcap_cg))
      #dfcap <- rbind(dfcap, dfcap_cg)
    }

    # code length > 6
    if (missing(code_cg)) {
      length <- subset(data[[code]], (nchar(data[[code]]) > 6))
      dflength <- data[data[[code]] %in% length, ]
      dflength$pat <- rep("length > 6", nrow(dflength))
    } else {
      length <- subset(data[[code]], (nchar(data[[code]]) > 6))
      dflength <- data[data[[code]] %in% length, ]
      dflength$pat <- rep("length > 6 (child)", nrow(dflength))

      length_cg <- subset(data[[code_cg]], (nchar(data[[code_cg]]) > 6))
      dflength_cg <- data[data[[code_cg]] %in% length_cg, ]
      dflength_cg$pat <- rep("length > 6 (caregivers)", nrow(dflength_cg))
      #dflength <- rbind(dflength, dflength_cg)
    }

    # specific scheme
    # o/O instead of 0
    if (missing(code_cg)){
      o <- subset(data[[code]], grepl(data[[code]], pattern = "o"))
      dfo <- data[data[[code]] %in% o, ]
      dfo$pat <- rep("o", nrow(dfo))
      O <- subset(data[[code]], grepl(data[[code]], pattern = "O"))
      dfO <- data[data[[code]] %in% O, ]
      dfO$pat <- rep("O", nrow(dfO))
      dfoO <- rbind(dfo, dfO)
    } else {
      o <- subset(data[[code]], grepl(data[[code]], pattern = "o"))
      dfo <- data[data[[code]] %in% o, ]
      dfo$pat <- rep("o (child)", nrow(dfo))
      O <- subset(data[[code]], grepl(data[[code]], pattern = "O"))
      dfO <- data[data[[code]] %in% O, ]
      dfO$pat <- rep("O (child)", nrow(dfO))
      dfoO <- rbind(dfo, dfO)

      o_cg <- subset(data[[code_cg]], grepl(data[[code_cg]], pattern = "o"))
      dfo_cg <- data[data[[code_cg]] %in% o_cg, ]
      dfo_cg$pat <- rep("o (caregivers)", nrow(dfo_cg))
      O_cg <- subset(data[[code_cg]], grepl(data[[code_cg]], pattern = "O"))
      dfO_cg <- data[data[[code_cg]] %in% O_cg, ]
      dfO_cg$pat <- rep("O (caregivers)", nrow(dfO_cg))
      dfoO_cg <- rbind(dfo_cg, dfO_cg)
      #dfoO <- rbind(dfoO, dfoO_cg)
    }

    ## country
    if (missing(code_cg)) {
      country <- if(countrmatch == "Ghana"){
        subset(data[[code]], !grepl(data[[code]], pattern = "^g."))
      } else if(countrmatch == "Tanzania"){
        subset(data[[code]], !grepl(data[[code]], pattern = "^t."))
      } else if(countrmatch == "Uganda"){
        subset(data[[code]], !grepl(data[[code]], pattern = "^u."))
      } else if(countrmatch == "Haiti"){
        subset(data[[code]], !grepl(data[[code]], pattern = "^h."))
      } else if(countrmatch == "Pakistan"){
        subset(data[[code]], !grepl(data[[code]], pattern = "^p."))
      } else {
        "NA"
      }
      dfcountry <- data[data[[code]] %in% country, ]
      dfcountry$pat <- rep("country letter", nrow(dfcountry))
      ## countryindex
      countrIndex2l <- subset(data[[code]], ((as.numeric(nchar(substr(data[[code]], 2,3)))) > 2))
      dfcountrIndex2l <- data[data[[code]] %in% countrIndex2l, ]
      dfcountrIndex2l$pat <- rep("country index too long", nrow(dfcountrIndex2l))
      countrIndex2s <- subset(data[[code]], ((as.numeric(nchar(substr(data[[code]], 2,3)))) < 2))
      dfcountrIndex2s <- data[data[[code]] %in% countrIndex2s, ]
      dfcountrIndex2s$pat <- rep("country index too short", nrow(dfcountrIndex2s))
      dfcountrIndex <- rbind(dfcountrIndex2l, dfcountrIndex2s)
    } else {
      country <- if (countrmatch == "Ghana") {
        subset(data[[code]], !grepl(data[[code]], pattern = "^g."))
      } else if (countrmatch == "Tanzania") {
        subset(data[[code]], !grepl(data[[code]], pattern = "^t."))
      } else if (countrmatch == "Uganda") {
        subset(data[[code]], !grepl(data[[code]], pattern = "^u."))
      } else if (countrmatch == "Haiti") {
        subset(data[[code]], !grepl(data[[code]], pattern = "^h."))
      } else if (countrmatch == "Pakistan") {
        subset(data[[code]], !grepl(data[[code]], pattern = "^p."))
      } else {
        "NA"
      }
      country <- subset(country, !is.na(country))
      dfcountry <- data[data[[code]] %in% country, ]
      dfcountry$pat <- rep("country letter (child)", nrow(dfcountry))
      ## countryindex
      countrIndex2l <- subset(data[[code]], ((as.numeric(nchar(substr(data[[code]], 2,3)))) > 2))
      dfcountrIndex2l <- data[data[[code]] %in% countrIndex2l, ]
      dfcountrIndex2l$pat <- rep("country index too long (child)", nrow(dfcountrIndex2l))
      countrIndex2s <- subset(data[[code]], ((as.numeric(nchar(substr(data[[code]], 2,3)))) < 2))
      dfcountrIndex2s <- data[data[[code]] %in% countrIndex2s, ]
      dfcountrIndex2s$pat <- rep("country index too short (child)", nrow(dfcountrIndex2s))
      dfcountrIndex <- rbind(dfcountrIndex2l, dfcountrIndex2s)

      country_cg <- if (countrmatch == "Ghana") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "^g."))
      } else if (countrmatch == "Tanzania") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "^t."))
      } else if (countrmatch == "Uganda") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "^u."))
      } else if (countrmatch == "Haiti") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "^h."))
      } else if (countrmatch == "Pakistan") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = "^p."))
      } else {
        "NA"
      }
      dfcountry_cg <- data[data[[code_cg]] %in% country_cg, ]
      dfcountry_cg$pat <- rep("country letter (caregivers)", nrow(dfcountry_cg))
      #dfcountry <- rbind(dfcountry, dfcountry_cg)
      ## countryindex
      countrIndex2l_cg <- subset(data[[code_cg]], ((as.numeric(nchar(substr(data[[code_cg]], 2,3)))) > 2))
      dfcountrIndex2l_cg <- data[data[[code_cg]] %in% countrIndex2l_cg, ]
      dfcountrIndex2l_cg$pat <- rep("country index too long (caregivers)", nrow(dfcountrIndex2l_cg))
      #dfcountrIndex2l <- rbind(dfcountrIndex2l, dfcountrIndex2l_cg)
      countrIndex2s_cg <- subset(data[[code_cg]], ((as.numeric(nchar(substr(data[[code_cg]], 2,3)))) < 2))
      dfcountrIndex2s_cg <- data[data[[code_cg]] %in% countrIndex2s_cg, ]
      dfcountrIndex2s_cg$pat <- rep("country index too short (caregivers)", nrow(dfcountrIndex2s_cg))
      #dfcountrIndex2s <- rbind(dfcountrIndex2s, dfcountrIndex2s_cg)
      dfcountrIndex_cg <- rbind(dfcountrIndex2l_cg, dfcountrIndex2s_cg)
    }
    ## school unmatched
    if (!missing(school)){
      data[[school]] <- ifelse(nchar(data[[school]]) == 1, paste("0", data[[school]], sep = ""), data[[school]])

      dfcountrIndexunmatch <- subset(data, (((as.character(substr(data[[code]], 2,3))) == data[[school]]) == FALSE))
      dfcountrIndexunmatch$pat <- rep("school index in code unequal index in school", nrow(dfcountrIndexunmatch)) # used to say "country index in code unequal index in school"
      dfcountrIndex <- rbind(dfcountrIndex, dfcountrIndexunmatch)
    } else {
    }
    ## person index
    if(missing(code_cg)){
      person <- if (persmatch == "Teacher") {
        subset(data[[code]], !grepl(data[[code]], pattern = ".t."))
      } else if (persmatch == "Child") {
        subset(data[[code]], !grepl(data[[code]], pattern = ".c."))
      } else if (persmatch == "Mother") {
        subset(data[[code]], !grepl(data[[code]], pattern = ".m."))
      } else if (persmatch == "Father") {
        subset(data[[code]], !grepl(data[[code]], pattern = ".f."))
      } else {
        "NA"
      }

      dfperson <- data[data[[code]] %in% person, ]
      dfperson$pat <- rep("person letter", nrow(dfperson))

    } else {
      person <- if (persmatch == "Caregivers") {
        subset(data[[code]], !grepl(data[[code]], pattern = ".c."))
      } else {
        "NA"
      }
      person <- subset(person, !is.na(person))
      dfperson <- data[data[[code]] %in% person, ]
      dfperson$pat <- rep("person letter (child)", nrow(dfperson))

      person_cg <- if(persmatch == "Caregivers") {
        subset(data[[code_cg]], !grepl(data[[code_cg]], pattern = ".m.|.f."))
      } else {
        "NA"
      }
      dfperson_cg <- data[data[[code_cg]] %in% person_cg, ]
      dfperson_cg$pat <- rep("person letter (caregivers)", nrow(dfperson_cg))
      #dfperson <- rbind(dfperson, dfperson_cg)
    }

    ## for caregivers only: match person letter with sex
    if(!missing(code_cg)){
      if(length(grep(colnames(data), pattern = ".*sex$")) > 0) {
      sex_m_cg <- subset(data[[code_cg]], c(grepl(data[[code_cg]], pattern = ".m.") & data[[grep(colnames(data), pattern = ".*sex$")]] != 1))
      sex_f_cg <- subset(data[[code_cg]], c(grepl(data[[code_cg]], pattern = ".f.") & data[[grep(colnames(data), pattern = ".*sex$")]] != 2))
      sex_cg <- c(sex_m_cg, sex_f_cg)
      dfsex_cg <- data[data[[code_cg]] %in% sex_cg, ]
      dfsex_cg_r <- subset(dfsex_cg, c((grepl(dfsex_cg[[code_cg]], pattern = ".m.") & dfsex_cg[[grep(colnames(dfsex_cg), pattern = ".*sex$")]] != 1))| (grepl(dfsex_cg[[code_cg]], pattern = ".f.") & dfsex_cg[[grep(colnames(dfsex_cg), pattern = ".*sex$")]] != 2))
      dfsex_cg_r$pat <- rep("person letter (caregivers) unequal sex variable", nrow(dfsex_cg_r))
      }
        else {
        }
      } else {
     }

    # combinde dfs
    ## combine all but duplicated codes
    if (missing(code_cg)) {
      dffilt <- rbind(dfscheme, dfcountry, dflength, dfoO, dfperson, dfcap, dfcountrIndex)
      duplcode2 <- subset(dffilt[[code]], duplicated(dffilt[[code]], na.action = stats::na.omit))
      if(length(duplcode2) > 0){
        dffilt2 <- dffilt[dffilt[[code]] %in% duplcode2, ]
      } else {
        dffilt2 <- dffilt
      }
      pattmp <- stats::as.formula(paste(quote(pat), code, sep = " ~ "))
      listtmp <- list(x = pattmp, data = unique(dffilt2), FUN = paste, collapse = ", ")
      dffilt3 <- do.call("aggregate", listtmp)
      dffilt2$pat2 <- dffilt3$pat[match(dffilt2[[code]], dffilt3[[code]])]
      dffilt$pat2 <- dffilt2$pat2[match(dffilt[[code]], dffilt2[[code]])]
      dffilt$pat2 <- ifelse(is.na(dffilt$pat2) == TRUE, dffilt$pat, dffilt$pat2)
      dffilt$pat <- dffilt$pat2
      dffilt$pat2 <- NULL
      dffilt <- dffilt[!duplicated(dffilt), ]
      ## add duplicated codes
      dffilt <- rbind(dffilt, dfduplcode)
      dffilt$pat <- sub(":,", ":", dffilt$pat)
    } else {
      dffilt_c <- rbind(dfscheme, dfcountry, dflength, dfoO, dfperson, dfcap, dfcountrIndex)
      duplcode2_c <- subset(dffilt_c[[code]], duplicated(dffilt_c[[code]]))
      if(length(duplcode2_c) != 0){
        dffilt2_c <- dffilt_c[dffilt_c[[code]] %in% duplcode2_c, ]
        pattmp_c <- stats::as.formula(paste(quote(pat), code, sep = " ~ "))
        listtmp_c <- list(x = pattmp_c, data = unique(dffilt2_c), FUN = paste, collapse = ", ")
        dffilt3_c <- do.call("aggregate", listtmp_c)
        dffilt2_c$pat2 <- dffilt3_c$pat[match(dffilt2_c[[code]], dffilt3_c[[code]])]
        dffilt_c$pat2 <- dffilt2_c$pat2[match(dffilt_c[[code]], dffilt2_c[[code]])]
        dffilt_c$pat2 <- ifelse(is.na(dffilt_c$pat2) == TRUE, dffilt_c$pat, dffilt_c$pat2)
        dffilt_c$pat <- dffilt_c$pat2
        dffilt_c$pat2 <- NULL
        dffilt_c <- dffilt_c[!duplicated(dffilt_c), ]
      } else {
      }
      if(length(grep(colnames(data), pattern = ".*sex$")) > 0) {
      dffilt_cg <- rbind(dfscheme_cg, dfcountry_cg, dflength_cg, dfoO_cg, dfperson_cg, dfcap_cg, dfcountrIndex_cg, dfsex_cg_r)
      } else {
        dffilt_cg <- rbind(dfscheme_cg, dfcountry_cg, dflength_cg, dfoO_cg, dfperson_cg, dfcap_cg, dfcountrIndex_cg)
      }
      duplcode2_cg <- subset(dffilt_cg[[code_cg]], duplicated(dffilt_cg[[code_cg]]))
      if(length(duplcode2_cg) != 0){
        dffilt2_cg <- dffilt_cg[dffilt_cg[[code_cg]] %in% duplcode2_cg, ]
        pattmp_cg <- stats::as.formula(paste(quote(pat), code_cg, sep = " ~ "))
        listtmp_cg <- list(x = pattmp_cg, data = unique(dffilt2_cg), FUN = paste, collapse = ", ")
        dffilt3_cg <- do.call("aggregate", listtmp_cg)
        dffilt2_cg$pat2 <- dffilt3_cg$pat[match(dffilt2_cg[[code_cg]], dffilt3_cg[[code_cg]])]
        dffilt_cg$pat2 <- dffilt2_cg$pat2[match(dffilt_cg[[code_cg]], dffilt2_cg[[code_cg]])]
        dffilt_cg$pat2 <- ifelse(is.na(dffilt_cg$pat2) == TRUE, dffilt_cg$pat, dffilt_cg$pat2)
        dffilt_cg$pat <- dffilt_cg$pat2
        dffilt_cg$pat2 <- NULL
        dffilt_cg <- dffilt_cg[!duplicated(dffilt_cg), ]
      } else {
      }

      dffiltcomb <- rbind(dfduplcode, dffilt_c, dffilt_cg)
      duplcode2comb <- subset(dffiltcomb[[code]], duplicated(dffiltcomb[[code]]))
      if(length(duplcode2comb) != 0){
        duplcode2comb <- subset(dffiltcomb[[code]], duplicated(duplcode2comb))
        if(length(duplcode2comb) != 0){
          dffilt2comb <- dffiltcomb[dffiltcomb[[code]] %in% duplcode2comb, ]
          dffilt2comb$BA_ccg_code <- paste(dffilt2comb[[code]], dffilt2comb[[code_cg]], sep = "|")
          pattmpcomb <- stats::as.formula(paste(quote(pat), "BA_ccg_code", sep = " ~ "))
          listtmpcomb <- list(x = pattmpcomb, data = unique(dffilt2comb), FUN = paste, collapse = ", ")
          dffilt3comb <- do.call("aggregate", c(listtmpcomb, na.action = stats::na.pass))
          dffilt2comb$pat <- NULL
          dffilt2comb <- dffilt2comb[!duplicated(dffilt2comb),]
          dffilt2comb$pat2 <- dffilt3comb$pat[match(dffilt2comb[["BA_ccg_code"]], dffilt3comb[["BA_ccg_code"]])]
          dffilt <- dffilt2comb
          dffilt$pat <- dffilt$pat2
          dffilt$pat2 <- NULL
          dffilt$BA_ccg_code <- NULL
          dffilt$pat <- sub("):,", "):", dffilt$pat)
        } else {
          dffilt2comb <- dffiltcomb
          dffilt2comb$BA_ccg_code <- paste(dffilt2comb[[code]], dffilt2comb[[code_cg]], sep = "|")
          pattmpcomb <- stats::as.formula(paste(quote(pat), "BA_ccg_code", sep = " ~ "))
          listtmpcomb <- list(x = pattmpcomb, data = unique(dffilt2comb), FUN = paste, collapse = ", ")
          dffilt3comb <- do.call("aggregate", c(listtmpcomb, na.action = stats::na.pass))
          dffilt2comb$pat <- NULL
          dffilt2comb <- dffilt2comb[!duplicated(dffilt2comb),]
          dffilt2comb$pat2 <- dffilt3comb$pat[match(dffilt2comb[["BA_ccg_code"]], dffilt3comb[["BA_ccg_code"]])]
          dffilt <- dffilt2comb
          dffilt$pat <- dffilt$pat2
          dffilt$pat2 <- NULL
          dffilt$BA_ccg_code <- NULL
          dffilt$pat <- sub("):,", "):", dffilt$pat)
        }
      } else {
        dffilt <- dffiltcomb
        dffilt$pat <- sub("):,", "):", dffilt$pat)
      }
    }

    ## shorten data set
    keepvar <- colnames(dffilt[,grepl(names(dffilt), pattern = c("pat|code|SbjNum|Date|Srvyr|Duration|Upload|RvwTime|SrvyrComment|Complete|VStart|VEnd|start|country|phase|intrw|sch|date|schtype|loc|sex|age|relstat|religion|ethn|ac_qual|yrs_t|mths_t|yrs_sch|mths_sch|hrs_sch|nr_std|nr_std_c|fam|fam_c|bioc|income|otjob"))])

    dffiltS <- subset(dffilt, select = keepvar)
    dffiltS$pattern <- dffiltS$pat
    dffiltS$pat <- NULL
    assign("QCcodes", value = dffiltS, envir = .GlobalEnv)
    if (save == "TRUE") {
      #filename <- paste("QC_codes_STGexport", ".csv", sep = "")
      file <- paste(file)
      utils::write.csv2(QCcodes, file = file, row.names = FALSE)
    } else {
    }
    print("successfully created data.frame 'QCcodes'")
  }
}
