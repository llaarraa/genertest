g\name{dbStudents}
\alias{dbStudents}
\docType{data}
\title{Data set with students data used to generate homeworks}
\description{
This file includes the information about students and groups needed to generate the personalized homework assignments using the \code{generateHomeworks} function
}
\usage{data(dbStudents)}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{Name}}{A string vector, indicating the name of students beloinging to each group to which the homework was assigned.}
    \item{\code{Group}}{A string vector, indicating the name of the group to which the homework was assigned. It is matched with the Group variable of the \code{dbHomeworks} data set}

  }
}
\details{The dataset is used together with the dbHomework data set to generate personalized assignments. The information contained in the two files is matched based on the variable Group, which appears in both data sets.}
\source{
\url{http://sites.google.com/site/lara3107/Home/software/genertest}
}
\references{
\url{http://sites.google.com/site/lara3107/Home/software/genertest},  \url{https://github.com/llaarraa/genertest}
}
\examples{
data(dbStudents)
str(dbStudents) 
head(dbStudents) 
}
\keyword{datasets}
