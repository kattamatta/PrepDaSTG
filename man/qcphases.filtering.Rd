% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qcphasesfiltering.R
\name{qcphases.filtering}
\alias{qcphases.filtering}
\title{quality control that filters baseline participant codes not matching participation code & demographic info between assessement phases (BA, FU1)}
\usage{
qcphases.filtering(
  data,
  BA_code,
  FU1_code,
  person = c("Teacher", "Child"),
  save,
  file
)
}
\arguments{
\item{data}{data frame}

\item{BA_code}{character of variable in data frame that hold participants codes of Baseline assessement}

\item{FU1_code}{character of variable in data frame that hold participants codes of Follow Up 1 assessement}

\item{person}{character specifying the person participants code belongs to. Must be one of "Teacher" or "Child"}

\item{save}{logical. If TRUE returned data frame will be saved as .csv with ; separation}

\item{file}{optional. absolute path including file name and appendix .csv in "" where returned data set should be stored}
}
\value{
data frame of baseline participant codes not not matching participation code & demographic info between assessement phases (BA, FU1)
}
\description{
this function filters cases (rows, baseline participation codes) that do not match participation code & demographic info between assessement phases
}
