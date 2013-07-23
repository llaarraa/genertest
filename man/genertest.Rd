\name{genertest}
\alias{genertest}
\title{Function that Generates Tests}
\description{
  This function generates test with questions drawn from a tab-delimited database. It can be used to randomly draw questions, to permute questions and answers, to select questions to include in the exams according to their topic or difficulty, to generate questions containing Sweave code. The output is a LaTeX or PDF file for each of the tests.
}
\usage{
genertest(my.data, my.outdir, num.tests = 1, 
repeat.each.test = 1, my.seed = 1999, topics = NULL, 
topics.points = NULL, tot.points = NULL, 
topics.points.difficulty = NULL, min.distance = 3, 
my.include = NA, generate.solutions = FALSE, 
my.title = "", my.date = "", my.prefix = "exam", 
head.name = "Name", head.id = "ID number", 
head.points = "Number of points", head.prefix = "MED", 
my.language = "english", use.Sweave = FALSE, compile.pdf = FALSE)
}
\arguments{
  \item{my.data}{Name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory, should be a string, written between quotes. See details.}
  \item{my.outdir}{Name of the directory where the exams will be written, should be a string, written between quotes; full path must be given}
  \item{num.tests}{Number of different tests to be generated}
  \item{repeat.each.test}{Number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation and just one test of each kind will be generated}
  \item{my.seed}{Seed used to inizialize the random number generator, useful to get reproducibile results}
  \item{topics}{Vector listing which topics should be included in the test, these strings have to match exactly the topics listed in the Topic variable in the database of questions}
  \item{topics.points}{Vector listing the number of points for each topic to include in the exam}
  \item{tot.points}{Total number of points to be included in the exams. It will be used only if \code{topics} was not specified, otherwise it will be ignored and the total number of points will be calculated using the sum of points specified in \code{topics.points} or \code{topics.points.difficulty}}
  \item{topics.points.difficulty}{Matrix with number of rows equal to the number of topics and number of columns equal to the number of different difficulties that one wishes to include. For example, if the difficulties in the question database were coded 1 to 4, the number in the first column and first row will define how many points from the first topic with difficulty 1 should be included, the number of second column and first row the points from topic 1 difficulty 2, etc... At most four different columns can be specified in this case. If you do not wish to select any questions from a specific combination of topic and difficulty write 0 in the corresponding cell}
  \item{min.distance}{Minimum distance between two consecutive tests that contain the same questions, 0=no constrain, 1=at least 1 test in between before the same question appears, etc...}
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
my.data must be prepared using a similar structure as the example data from this library. The inclusion of some variables is mandatory (Question, Question.ID) other variables can be included (Answer, Topic, Points, etc). See \code{\link{my.data}} for more details. 
If use.Sweave=TRUE,   a call to the Sweave package will be included in the header of the *.rnw and *.tex files. For this reason Sweave package must be available to LaTeX compiler. File Sweave.sty can be included in the directory my.outdir in case of problems related to package unavailability. 
If compile.pdf=TRUE, pdflatex.exe program from MikTeX will be invoked to convert *.tex to *.pdf files. MikTeX must be installed locally and its executables (as pdflatex.exe) must be available from my.outdir directory. See MikTeX documentation. If the user wishes to use a different program than pdflatex.exe to compile LaTeX files, set compile.pdf=FALSE and manually compile the *.tex files outside R
}
\value{
Writes LaTeX (od PDF) files containing the texts of the exams and returns a list containing the IDs of the questions selected for each test and the list of the files *.tex files that were generated. 
   \item{Questions}{Each element of the list contains a vector that can be used to identify the questions selected for an exam. The index for a question represents the row in which the question appears (or begins) in the database of questions. If permuted versions of the same test were produced (repeat.each.test>1) then only one record for each set of permuted tests is reported}
  \item{files}{List of the generated files (*.tex or *.pdf, depending on the selected options)}
}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}}
\author{Lara Lusa}
\note{}
\examples{
#data(my.data)
#my.title<-"Medical Faculty - Undergraduates  - Exam of Biostatistics"
#my.date<-"09.12.2008"
#my.outdir<-"C:\\Documents and Settings\\"  
#Sweave.sty and pdflatex.exe must be available from this directory
#	   TMP<-genertest(my.data, my.outdir, num.tests=2, repeat.each.test=1, 
#my.seed=2, topics=c("Descr", "Probability"), topics.points=c(20,30), 
#tot.points=NULL, topics.points.difficulty=NULL, min.distance=0, 
#my.include=3, generate.solutions=TRUE, my.title=my.title, 
#my.date=my.date, my.prefix="Exam", head.name="Name", head.id="ID Number", 
#head.points="Points", head.prefix="Exam", my.language="english", 
#use.Sweave=TRUE, compile.pdf=TRUE)
}
\seealso{\code{\link{write.all}}, \code{\link{my.data}}}
\keyword{misc}

