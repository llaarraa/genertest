\name{my.data}
\alias{my.data}
\docType{data}
\title{Example Database with Questions}
\description{
  Tab-delimited database that can be used with functions to generate tests included in the genertest package.
}
\usage{data(my.data)}
\format{
  A dataset containing questions, answers and other variables necessary for test generation. Each question can be specified using a header (first line where the question appears in the dataset) and sub-questions (lines after the header and before a new question begins).
  \describe{
    \item{\code{Topic}}{Topic of the question; can be specified only for the header of a question, optional}
    \item{\code{Points}}{Number of points; mandatory for the header of all questions}
    \item{\code{Question.ID}}{Question ID; mandatory for the header of all questions}
    \item{\code{IncludeMed}}{a logical vector; specified for the header of the questions, it indicates if the question should be included (TRUE) or not (FALSE); optional, see genertest() documentation }
    \item{\code{IncludeDM}}{a logical vector; specified for the header of the questions, it indicates if the question should be included (TRUE) or not (FALSE); optional, see genertest() documentation}
    \item{\code{Question}}{Text of the question (in the header) and sub-questions (for lines where Question.ID is empty); see details for specification of questions using Sweave syntax, mandatory.}
    \item{\code{Answer}}{Answer to the questions and sub-questions, mandatory if the user decides to generate the solutions of the tests - see genertest() documentation}
    \item{\code{ExtraLines}}{Number of empty lines to be left after the question (or sub-questions), if left missing no empty lines will be added; can be specified for header and sub-questions, optional}
    \item{\code{ExcludeIf}}{Questions that should be excluded if the current question is included in the test (using Question.ID to identify the questions). If more than a question should be excluded question IDs to be excluded should be separated by a comma;  if omitted or missing from the header of some questions it will be assumed that no question should be excluded when the current question is included in the text; optional}
    \item{\code{PermuteOK}}{a logical vector: indicates if the sub-questions can be permuted (TRUE) or not (FALSE); optional, if omitted or missing for the header of some questions it will be assumed that sub-questions should not be permuted}
  }
}
\details{
 Each question can be specified using a header (first line where the question appears in the dataset) and sub-questions (lines after the header and before a new question begins). The beginning of a new question is indicated specifiying a question ID, i.e., filling the value of Question.ID, this should be done exclusively for the lines where a question begins. Lines that have an empty Question.ID will be considered as sub-questions. 
 Number of points must be specified for each header. Topic, ExcludeIf, PermuteOK and vector indicating which questions should be excluded (in this dataset: IncludeMed and IncludeDM) can be specified in the header of the questions.
Sweave code can be used to specify the text of questions and answers; it is important that an empty line is left before the beginning of Sweave code-chunks (indicated with <<>>= ) and to go to a new line after the end of the Sweave code-chunk (indicated with @). 
The last line of the dataset is a fake question, indicated with topic "end", its own question ID and 0 points. In the current version of the implementation of genertest() function it is needed - if omitted, the(real) question that appears at the end of the file will never be included in the tests.
}
\source{
  \url{http://sites.google.com/site/lara3107/Home/software/genertest}
}
\references{
  \url{http://sites.google.com/site/lara3107/Home/software/genertest}
}
\examples{
data(my.data)
str(data)
}
\seealso{\code{\link{write.all}}, \code{\link{genertest}}}
\keyword{datasets}
