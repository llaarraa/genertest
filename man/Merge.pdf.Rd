\name{Merge.pdf}
\alias{Merge.pdf}
\title{Merges PDF files}
\description{
 Utility function that merges PDF files within R. It can be used to produce a single file that includes all the tests and that can be printed with a single command. 
}
\usage{
Merge.pdf(my.files, my.dir, outfile = "merge")
}
\arguments{
  \item{genertest.output}{Object obtained using genertest function; the function will merge all the exams generated  with the call used to produce the output. If this argument is specified (not NULL) the my.files and my.dir arguments will be ignored.}
  \item{my.files}{Vector containing the names of the PDF files that will be merged, full path of the files should not be used; for example: c("Exam1.pdf", "Exam2.pdf")}
  \item{my.dir}{Directory where the PDF files to be merged are stored; for example: "c:\\Documents and Settings\\Exams"}
  \item{outfile}{Name of the file that will be generated containing the merged PDFs; for example: using "merged" a file called merged.pdf will be generated}
 }
\details{
 Uses LaTeX package pdfpages, which must be available when pdflatex.exe is invoked. MikTeX must be istalled locally and pdflatex.exe must be available from my.dir directory.
}
\value{
 Generates in my.dir directory a PDF file that contains all the PDF files. 
 \item{file}{Full path of the merged PDF file.}
 \item{errors}{String listing the errors encountered during merging, if any.}
}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}, \url{https://github.com/llaarraa/genertest}}
\author{Lara Lusa}

\seealso{See Also as \code{\link{genertest}}}
\examples{

#generate the tests using the genertest function

my.title<-"Medical Faculty - Undergraduates  - Exam of Biostatistics"
my.date<-"31.7.2013"
my.file.name=paste(path.package("genertest"),"/extdata/questionsExamples.txt",sep="")
results.genertest<-genertest(my.file.name, my.outdir=NULL, num.tests=2, repeat.each.test=1, 
my.seed=2, topics=c("Descr", "Probability"), topics.points=c(20,30), 
min.distance=0, generate.solutions=TRUE, 
my.title=my.title, my.date=my.date, my.prefix="Exam", head.name="Name", head.id="ID Number", 
head.points="Points", head.prefix="Exam", my.language="english", 
use.Sweave=TRUE, compile.pdf=TRUE, my.final.sentence="Good luck!")

##results.genetest: summary of the tasks performed by the genertest function

# for example the result in this case would be my.files<-c("Exam1.pdf", "Exam2.pdf", "Exam1sol.pdf","Exam2sol.pdf")

#merging the files obtained using genertest, using the output 
Merge.pdf(results.genertest, my.files=NULL, my.dir=NULL, "mergeExamsSol")

#merging the files specifying the file names and directory where they are stored 
my.files<-unlist(lapply(strsplit(unlist(lapply(strsplit(TMP$files, "\\."), 
function(x) paste(x[1], ".pdf", sep=""))), "\\\\"), function(x) x[length(x)]))

my.dir=unlist(lapply(strsplit(unlist(lapply(strsplit(TMP$files, "\\."), 
function(x) paste(x[1], ".pdf", sep=""))), "\\\\"), function(x) x[-length(x)]))[1]

Merge.pdf(genertest.output=NULL, my.files=my.files, my.dir=my.dir, "mergeExamsSol")


#to repeat the same exam 3 times
my.files<-rep(my.files[1:2], 3)
Merge.pdf(genertest.output=NULL, my.files=my.files, my.dir=my.dir, "mergeExams")

#to merge the exams only (no solutions)
my.files=my.files[1:(length(my.files)/2)]
Merge.pdf(genertest.output=NULL, my.files=my.files, my.dir=my.dir, "mergeExamsOnly")

}
\keyword{misc}
