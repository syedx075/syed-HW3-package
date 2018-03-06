#############################
### Jasmine Syed
### STAT 3701
### HW 3
### March 7, 2018
#############################

library(devtools) # install.packages("devtools")
library(roxygen2) # install.packages("roxygen2")
library(readr) # install.packages("readr")
library(tidyverse)
library(testthat)

## Check my current working directory path
getwd()
setwd("C:/Users/jasmi/Documents/STAT 3701/package template - HW 3")
## Check what is in my current working directory
dir()

##
syedtools<-"syedTools"
devtools::create(pckname)
dir(pckname)

loc<-paste(getwd(),pckname,sep="/")
descLoc<-paste(loc,"DESCRIPTION",sep="/")
readLines(descLoc)

## Clean up Description
desc<-readLines(descLoc)
desc[grep("Title",desc)]<-"Title: Tools for STAT 3701 HW 3"
desc[grep("Version:",desc)]<-"Version: 0.0.1"
desc[grep("Authors",desc)]<-"Authors@R: person(\"Jasmine\", \"Syed\", email = \"syedx075@umn.edu\", role = c(\"aut\", \"cre\"))"
desc[grep("Description",desc)]<-"Description: This is a set of functions for HW 3."
desc[grep("License",desc)]<-"License: MIT + file LICENSE"
writeLines(desc,con=descLoc)

## Write LICENSE FILE
write_file(x="
YEAR: 2018
COPYRIGHT HOLDER: Jasmine M. Syed
", path=paste(loc,"LICENSE",sep="/"))

# copy R functions to file in package
file.copy(paste(getwd(),"RFunctionsForPackage.R",sep="/"), paste(loc,"R",sep="/"))
file.rename(paste(loc,"R","RFunctionsForPackage.R",sep="/"),paste(loc,"R",paste(pckname,"Rfunctions.R",sep=""),sep="/"))


# Build comments for the package
devtools::document(loc)

########## dataset 1 ############
# Add some example data
d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
devtools::use_data(d,pkg=loc, overwrite=TRUE)

## Document data
write_file(x=
             "
           #' Example data from HW 1, problem 4
           #'
           #' @author Charlie Geyer \\email{geyer@umn.edu}
           #' @references \\url{http://www.stat.umn.edu/geyer/3701/data/q1p4.txt}
           \"d\" ",path=paste(loc,"R","data.R",sep="/"))

# Build comments for the data
devtools::document(loc)

########## dataset 2 ############
g <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p1.txt"))
devtools::use_data(g,pkg=loc, overwrite=TRUE)
 
## Document data   
write_file(x=
              "
          #' Example data from HW 1, problem 1 
          #'
          #' @author Charlie Geyer \\email{geyer@umn.edu}
          #' @references \\url{http://www.stat.umn.edu/geyer/3701/data/q1p1.txt}
         \"g\" ",path=paste(loc,"R","data.R",sep="/"))
 
# Build comments for the data
devtools::document(loc)

########## dataset 3 ############
b <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
devtools::use_data(b,pkg=loc, overwrite=TRUE)

## Document data   
write_file(x=
             "
           #' Example data from HW 1, problem 3
           #'
           #' @author Charlie Geyer \\email{geyer@umn.edu}
           #' @references \\url{http://www.stat.umn.edu/geyer/3701/data/q1p3.txt}
           \"b\" ",path=paste(loc,"R","data.R",sep="/"))

# Build comments for the data
devtools::document(loc) 

####CHANGE WD BEFORE AND AFTER TEST_THAT()
# Build Test Framework (for unit tests of functions)

use_testthat(loc)

# copy R functions to file in package
file.copy(paste(getwd(),"UnitTestForRfunctions.R",sep="/"), paste(loc,"tests","testthat",sep="/"))
file.rename(paste(loc,"tests","testthat","UnitTestForRfunctions.R",sep="/"),paste(loc,"tests","testthat",paste(pckname,"func1_func2.R",sep=""),sep="/"))


# At this point let's do an R Check
# And an R build
devtools::check(loc)
devtools::build(loc)

# Add function that requires ggplot2

# copy R functions to file in package
file.copy(paste(getwd(),"RFunctionsWdependencies.R",sep="/"), paste(loc,"R",sep="/"))
file.rename(paste(loc,"R","RFunctionsWdependencies.R",sep="/"),paste(loc,"R",paste(pckname,"myplot.R",sep=""),sep="/"))

## Add dependancies to R package
# Dependancies
use_package("ggplot2",pkg=loc)
use_package("dplyr",pkg=loc)
use_package("magrittr", package=loc)
# Add Manuals
devtools::document(loc)


# At this point let's do an R Check, again
# And an R build
devtools::check(loc)
devtools::build(loc)



# Let's now document the whole package
use_package_doc(loc)

# Add Manuals
devtools::document(loc)

## Let's build a vignette
devtools::use_vignette("introduction",pkg=loc)

# At this point let's do an R Check, again
# And an R build
devtools::check(loc)
devtools::build(loc)


# Last let's install it on our machine
devtools::install()

############
## After out template, all future development will be
## through RStudio project, e.g. almquistTools.Rpro
############

############
## Wednesday we will walk through how 
## to add everything to git
############
