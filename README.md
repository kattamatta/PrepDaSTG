**PrepDaSTG** is an R package that facilitates pre-processing of research data assessed by the AE17 team.
The package provides:
- renaming column names with semantically meaningful variable names of research data. Schemes of recoding are provided within the package for each participant group (children, students, caregivers, twins, teachers, head teachers). These schemes are valid for the specific participant group between countries (Ghana, Tanzania, Uganda, Haiti).
- conducting quality control of participants codes that follow a specific scheme specified a-priori by the AE17 research group. It returns information which code differs how from the predefined coding scheme. The scheme is defined in the function. 
- functions for the data import of required data as well as data export of data with renamed column names as well as quality control results.

## Installation
**PrepDaSTG** can be installed from GitHub using the following code. Note: On Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) have to be installed.

```r
if(!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("kattamatta/PrepDaSTG")
```

