\name{genertestPreSelect}
\alias{genertestPreSelect}
\title{Function that Generates Tests using a list of prespecified questions}
\description{
  This function generates tests with questions drawn from a tab-delimited database. The questions to be included in each test are specified by the user. The tests can include permuted questions and answers, or be generated using Sweave code. The output is a LaTeX or PDF file for each of the tests.}
\usage{    
genertestPreSelect(my.db.name, my.outdir, list.QID, num.tests=NULL, 
                            repeat.each.test=1, my.seed=1999, 
                            generate.solutions=FALSE, my.title="Exam", my.date="Today", my.prefix="exam",     head.name="Name", head.id="ID number", 
                            head.points="Number of points", head.prefix="MED", my.language="english", 
                            use.Sweave=TRUE, compile.pdf=TRUE, merge.pdf=TRUE, my.final.sentence=NULL, files.to.move=NULL, names.files.to.move=NULL)
 
}
\arguments{
  \item{my.db.name}{Name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory, should be a string, written between quotes. Alternatively, an R data.frame can be passed as an argument. See details.}
  \item{my.outdir}{Name of the directory where the exams will be written, should be a string, written between quotes; full path must be given. If not specified, a directory named Exams+DateTime  (ExamsMonthDayYearHourMin using the date and time when the function is executed) will be created in the working directory and the files will be stored in that directory.}
  \item{list.QID}{List with the questions ID (as they appear in the questions data base) to be used for each test. Each element of the list contains a vector with the IDs of the questions to include in each test.}
  \item{num.tests}{Number of different tests to be generated, optional, as the number can be retrieved from the lenght of the list.QID argument.}
  \item{repeat.each.test}{Number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation and just one test of each kind will be generated}
  \item{my.seed}{Seed used to inizialize the random number generator, useful to get reproducibile results}
  \item{generate.solutions}{Indicator (TRUE or FALSE) that indicates if the solutions of the tests must be generated; if set to TRUE it generated a LaTeX (or PDF) file with solutions corresponding to each exam}
  \item{my.title}{Name of the exam - to be displayed in the header}
  \item{my.date}{Date of the exam - to be displayed in the header}
  \item{my.prefix}{String with which the names of the LaTeX files of the tests begins}
  \item{head.name}{String indicating the name of the student - to be displayed in the header (the default is "Name" but it can be changed if a language different than English is used)}
  \item{head.id}{String indicating the identification number of the student - to be displayed in the header (the default is "ID number" but it can be changed if a language different than English is used)}
  \item{head.points}{String indicating how to indicate the number of points - to be displayed in the header (the default is "Number of points" but it can be changed if a language different than English is used)}
  \item{head.prefix}{An alpha-numerical ID is generated for each test. The ID is generated pasting \code{ head.prefix}, a random number and a progressive number that indicates the test}
  \item{my.language}{String indicating the language in which the test is written - it is used to call a LaTeX Babel package that contains the appropriate language settings (a call to the Babel package will be included in the header of the *.tex files)}
  \item{use.Sweave}{Indicator of whether in the database there are some exercises written using Sweave code; default is TRUE, if set to TRUE *.rnw files will be generated and compiled to *.tex (or *.pdf) files. See details.}
  \item{compile.pdf}{Indicator of whether the *.tex files should be compiled to *.pdf files (compiled if set to TRUE). See details.}
   \item{merge.pdf}{Indicator of whether the *.pdf files should be merged in a single PDF file, valid only if compile.pdf=TRUE}
  \item{my.final.sentence}{A string with a sentence that will be written in bold at the end of each exam. If set to NULL (default) no sentence will be displayed.}
     \item{files.to.move}{Vector of strings indicating the (full path) name of the files that should be moved in the same directory as the exams, if not specified, all the files in dir.files.to.move are moved.}
\item{names.files.to.move}{Vector of strings indicating the name of the files.to.move files, as it should appear in the final version. This parameter is useful only if the files passed through the files.to.move argument should be renamed (as in the shinyApp). Ignore otherwise.}
 
  
}
\details{
The data set with questions must be prepared using a similar structure as the example data from this library. The inclusion of some variables is mandatory (Question, Question.ID) other variables can be included (Answer, Topic, Points, etc). See \code{\link{dbQuestions}} for more details. 
\code{my.db.name} can be either the path to the tab delimted data base containing the questions or an R data.frame.
If use.Sweave=TRUE,   a call to the Sweave package will be included in the header of the *.rnw and *.tex files. For this reason Sweave package must be available to LaTeX compiler. File Sweave.sty can be included in the directory my.outdir in case of problems related to package unavailability. 
If compile.pdf=TRUE, the *.tex  files will be compiled into *.pdf files using \code{texi2dvi} function. 
%MikTeX must be installed locally and its executables (as pdflatex.exe) must be available from my.outdir directory. See MikTeX documentation. 
If the user wishes to use a different program to compile the LaTeX files, set compile.pdf=FALSE and manually compile the *.tex files outside R.}
\value{
Writes LaTeX (od PDF) files containing the texts of the exams and returns a list containing the IDs of the questions selected for each test and the list of the files *.tex files that were generated. 
   \item{Questions}{Each element of the list contains a vector that can be used to identify the questions selected for an exam. The index for a question represents the row in which the question appears (or begins) in the database of questions. If permuted versions of the same test were produced (repeat.each.test>1) then only one record for each set of permuted tests is reported}
  \item{files}{Names of the generated files, using full path (*.tex or *.pdf, depending on the selected options)}
   \item{names.files}{Names the generated files (*.tex or *.pdf, depending on the selected options)}
   \item{dir.files}{Path where the generated files are stored}
\item{merged.file}{Path to the files containing the merged PDFs, if produced.}
     \item{errors}{A string with the errors encountered during the execution of the function, if any.)}
}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}, \url{https://github.com/llaarraa/genertest}}
\author{Lara Lusa}
\note{}
\examples{
#data(my.data)
my.title<-"Medical Faculty - Undergraduates  - Exam of Biostatistics"
my.date<-"31.7.2013"
#my.outdir<-"C:\\Users\\lara\\Documents\\proveGenertest"  
#Sweave.sty and pdflatex.exe must be available from this directory
#name and path of the database with the questions
my.file.name=paste(path.package("genertest"),"/extdata/questionsExamples.txt",sep="")
gPS.output<-genertestPreSelect(my.file.name, my.outdir=NULL, list.QID=list(c(1,2,3), c(2,3,4)), num.tests=2, repeat.each.test=3, 
my.seed=2, generate.solutions=TRUE, 
my.title=my.title, my.date=my.date, my.prefix="Exam", head.name="Name", head.id="ID Number", 
head.points="Points", head.prefix="Exam", my.language="english", 
use.Sweave=TRUE, compile.pdf=TRUE, my.final.sentence="Good luck!")

gPS.output

#merge the files in a single file
merge.out=Merge.pdf(gPS.output)
}
\seealso{\code{\link{write.all}}, \code{\link{dbQuestions}}}
\keyword{misc}

