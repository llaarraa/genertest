\name{write.all}
\alias{write.all}
\title{Writes All the Questions from a Database}
\description{
 Writes all the questions from the database of questions (my.data) in the same format used to prepare the tests. The function is useful to check the obtainable output for the questions included in the database. The topic, number of points and exclusion criteria of each question is also printed out. 
}
\usage{
write.all(my.db.name, my.outdir, my.seed = 1999, 
generate.solutions = FALSE, 
my.title = "", my.date = "", my.prefix = "exam", 
head.name = "Name", head.id = "ID number", 
head.points = "Number of points", head.prefix = "MED", 
my.language = "english", use.Sweave = FALSE, compile.pdf = FALSE)
}
\arguments{
  \item{my.data}{Name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory, should be a string, written between quotes}
  \item{my.outdir}{Name of the directory where the exams will be written, should be a string, written between quotes; full path must be given}
   \item{my.seed}{Seed used to inizialize the random number generator, useful to get reproducibile results}
  \item{my.include}{The number of the column in the database of questions in which a TRUE/FALSE indicator in included, to indicate which questions should be included (T) (or excluded (F)), if left to defalut NA all the questions should be considere for inclusion. The column number rather than a fixed name is used since in principle the same database can be used to generate test for different courses or of different difficulties}
  \item{generate.solutions}{Indicator (TRUE or FALSE) that indicates if the solutions of the tests must be generated; if set to TRUE it generated a LaTeX (or PDF) file with solutions corresponding to each exam}
  \item{my.title}{Name of the exam - to be displayed in the header}
  \item{my.date}{Date of the exam - to be displayed in the header}
  \item{my.prefix}{String with which the names of the LaTeX files of the tests begins}
  \item{head.name}{String indicating the name of the student - to be displayed in the header (the default is "Name" but it can be changed if a language different than English is used)}
  \item{head.id}{String indicating the identification number of the student - to be displayed in the header (the default is "ID number" but it can be changed if a language different than English is used)}
  \item{head.points}{String indicating how to indicate the number of points - to be displayed in the header (the default is "Number of points" but it can be changed if a language different than English is used)}
  \item{head.prefix}{An alpha-numerical ID is generated for each test. The ID is generated pasting \code{ head.prefix}, a random number and a progressive number that indicates the test}
  \item{my.language}{String indicating the language in which the test is written - it is used to call a LaTeX Babel package that contains the appropriate language settings (a call to the Babel package will be included in the header of the *.tex files)}
  \item{use.Sweave}{Indicator of whether in the database there are some exercises written using Sweave code; default is FALSE, if set to TRUE *.rnw files will be generated and compiled to *.tex (or *.pdf) files. See details }
  \item{compile.pdf}{Indicator of whether the *.tex files should be compiled to *.pdf files (compiled if set to TRUE. See details}
}
\details{
See function genertest() for more details.
}
\value{
Writes LaTeX (od PDF) files containing the texts of the exams and returns a list containing the IDs of the questions selected for each test and the list of the files *.tex files that were generated. 
   \item{Questions}{Each element of the list contains a vector that can be used to identify the questions selected for an exam. The index for a question represents the row in which the question appears (or begins) in the database of questions. If permuted versions of the same test were produced (repeat.each.test>1) then only one record for each set of permuted tests is reported}
  \item{files}{List of the generated files (*.tex or *.pdf, depending on the selected options)}
  \item{errors}{A string with the errors encountered during the execution of the function, if any.)}

}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}, \url{https://github.com/llaarraa/genertest}}
\author{Lara Lusa}
\note{}

\seealso{\code{\link{genertest}}, \code{\link{my.data}}}
\examples{
##data(my.data)
my.title<-"Medical Faculty - Undergraduates  - Exam of Biostatistics"
my.date<-"31.7.2013"
my.file.name=paste(path.package("genertest"),"/extdata/questionsExamples.txt",sep="")

#my.outdir<-"C:\\Documents and Settings\\genertest" 

TMP1<-write.all(my.file.name, my.outdir=NULL, my.seed=2, 
generate.solutions=TRUE, my.title=my.title, my.date=my.date, 
my.prefix="Exam", head.name="Name", head.id="ID Number", 
head.points="Points", head.prefix="Exam", my.language="english", 
use.Sweave=TRUE, compile.pdf=TRUE)




##
}  
\keyword{misc}
