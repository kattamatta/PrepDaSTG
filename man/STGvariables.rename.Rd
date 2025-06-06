% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stgvariablesrename.R
\name{STGvariables.rename}
\alias{STGvariables.rename}
\title{renaming STG data}
\usage{
STGvariables.rename(
  data,
  subject = c("Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students",
    "Teachers", "TwinCaregivers"),
  country = c("Ghana", "Tanzania", "Uganda", "Haiti", "Pakistan"),
  time = c("baseline", "follow-up 1", "follow-up 2"),
  save,
  file
)
}
\arguments{
\item{data}{file with STG exported data with column names to be renamed to variable names based on renaming file}

\item{subject}{character indicating participant group or interview format. Must be one of "Caregivers", "Children", "HeadTeachers", "NumeracyLiteracy", "Students", "Teachers", or "TwinCaregivers".}

\item{country}{character specifying the country participant codes were assessed. Must be one of "Ghana", "Tanzania", "Uganda", "Haiti", or "Pakistan".}

\item{time}{character indicating assessment occasion. Must be one of "baseline", "follow-up 1", or "follow-up 2"}

\item{save}{logical. If TRUE returned data set will be saved as .csv}

\item{file}{optional. absolute path including file name and appendix .csv in "" where returned data set should be stored}
}
\value{
data set with STG exported data with renamed column names based on renaming file
}
\description{
renaming variable names of STG exported data sets
}
