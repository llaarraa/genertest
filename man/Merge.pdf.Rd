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
  \item{my.files}{Vector containing the names of the PDF files that will be merged, full path of the files should not be used; for example: c("Exam1.pdf", "Exam2.pdf")}
  \item{my.dir}{Directory where the PDF files to be merged are stored; for example: "c:\\Documents and Settings\\Exams"}
  \item{outfile}{Name of the file with the merged pdfs; for example: "merged" }
 }
\details{
 Uses LaTeX package pdfpages, which must be available when pdflatex.exe is invoked. MikTeX must be istalled locally and pdflatex.exe must be available from my.dir directory.
}
\value{
 Generates in my.dir directory a PDF file that contains all the PDF files.  
}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}}
\author{Lara Lusa}

\seealso{See Also as \code{\link{genertest}}}
\examples{
##TMP: result from genertest
#my.files<-unlist(lapply(strsplit(unlist(lapply(strsplit(TMP$files, "\\."), 
#function(x) paste(x[1], ".pdf", sep=""))), "\\\\"), function(x) x[length(x)]))
# for example the result in this case would be my.files<-c("Exam1.pdf", "Exam2.pdf", "Exam1sol.pdf","Exam2sol.pdf")
#my.dir<-"C:\\Documents and Settings\\genertest" 
#Merge.pdf(my.files, my.dir, "mergeExamsSol")

#to repeat the same exam 3 times
#my.files<-rep(my.files[1:2], 3)
#Merge.pdf(my.files, my.dir, "mergeExams")
}
\keyword{misc}
