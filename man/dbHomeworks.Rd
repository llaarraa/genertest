\name{dbHomeworks}
\alias{dbHomeworks}
\docType{data}
\title{Tab delimited data set describing the homeworks}
\description{
This file includes the information needed to generate the personalized homework assignments using the \code{generateHomeworks} function

}
\usage{data(dbHomeworks)}
\format{
  A data frame with 12 observations on the following 9 variables. Each assignment can be specified using a header (first line where the question appears in the dataset) and sub-questions (lines after the header and before a new question begins). 
  \describe{
    \item{\code{Group}}{A string vector, indicating the name of the group to which the homework was assigned. It is matched with the Group variable of the \code{dbStudents} data set}
    \item{\code{Type}}{A string vector, indicating the type of assignment.}
    \item{\code{Sources}}{A string vector, indicating the sources that can be used to complete the assignment.}
    \item{\code{Question}}{A string vector, containing the text describing the assignment.}
    \item{\code{Answer}}{A string vector, containing the answer to the assignment.}
    \item{\code{DueDate}}{A string vector, containing the date when the assignment is due.}
    \item{\code{Order}}{A number indicating the order of the assignments.}
    \item{\code{Template}}{A string vector, indicating the which template should be used to prepare the assignment.}
    \item{\code{Notes}}{A string vector, containing additional notes for the teacher.}
      
  }
}
\details{
Only the variables Question and Answer can have non-empty cells in the non-header rows. For these variables, the text appearing in the header line will be displayed as the header of the assignment, the successive lines will be printed out as items of a list.
}
\source{
 \url{http://sites.google.com/site/lara3107/Home/software/genertest}
}
\references{
 \url{http://sites.google.com/site/lara3107/Home/software/genertest}, \url{https://github.com/llaarraa/genertest}
}
\examples{
data(dbHomeworks)
str(dbHomeworks) 
head(dbHomeworks)
}
\keyword{datasets}
