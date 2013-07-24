\name{generateHomeworks}
\alias{generateHomeworks}
\title{Function that generates personalized (group) homeworks}
\description{ This function generates homeworks with assignments drawn from a tab-delimited database. The assignments can be generated using Sweave code. The output is a LaTeX or PDF file for each of the tests.}
\usage{generateHomeworks(my.db.homeworks.name, my.db.students.name,  my.outdir=NULL,  my.seed=1999, generate.solutions="FALSE", 
        my.title="Homework - Biostatistics", my.date="Academic Year 2012/2013", 
  			list.id.homeworks=list(id.group=NULL, id.type=NULL, id.source=NULL, id.text=NULL, id.date=NULL, id.template=NULL, id.order.fileAll=NULL),
				list.id.students=list(id.group.names=NULL, id.students.names=NULL),
				source.text="Sources",
				template.text="Template to use",
				date.text="Due date",
				group.members.text="Group members",
        text.groupdata="Data about the group",
        text.assignment="Assignment",
  	
				my.language="english", use.Sweave=FALSE, 
        compile.pdf=TRUE,
        my.final.sentence=NULL)


}



\arguments{
  \item{my.db.homeworks.name}{Name of the tab delimited file including the assignments for each group, either the full path should be given or the file should be placed in the R working directory, should be a string, written between quotes. See details.}
    \item{my.db.students.name}{Name of the tab delimited file including the names of the members of each group, either the full path should be given or the file should be placed in the R working directory, should be a string, written between quotes. See details.}
  \item{my.outdir}{Name of the directory where the exams will be written, should be a string, written between quotes; full path must be given. If not specified, a
  directory named Homeworks+DateTime  (HomeworksMonthDayYearHourMin using the date and time when the function is executed) will be created in the working directory and the files will be stored in that directory.}
  \item{my.seed}{Seed used to inizialize the random number generator, useful to get reproducibile results}
  \item{generate.solutions}{Indicator (TRUE or FALSE) that indicates if the solutions of the tests must be generated; if set to TRUE it generated a LaTeX (or PDF) file with solutions corresponding to each exam}
  \item{my.title}{Name of the exam - to be displayed in the header}
  \item{my.date}{Date - to be displayed in the header}
\item{list.id.homeworks}{List indicating the column numbers in which the variables needed from the database of homeworks are reported. The argument can be omitted if the database with homeworks includes the variables named: Group, Type, HomeworkText, DueDate, Template, Order. See details.}
\item{list.id.students}{List indicating the column numbers in which the variables needed from the database of students are reported. The argument can be omitted if the database with students includes the variables named: Group, Name. See details.}
\item{source.text}{String with which the souces to be used are indicated in the header, default is "Sources"}
\item{template.text}{How the template to be used is indicated in the header, default is "Template"}
\item{date.text}{How the due date of the assignment is indicated in the header, default is "Due date"}
\item{group.members.text}{How the due group members are indicated in the header, default is "Group members"}
\item{text.groupdata}{Header displayed for the group information, default is "Data about the group"}
\item{text.assignment}{Header diplayed for the assignment, default is "Assignment"}
\item{my.language}{String indicating the language in which the test is written - it is used to call a LaTeX Babel package that contains the appropriate language settings (a call to the Babel package will be included in the header of the *.tex files)}
  \item{use.Sweave}{Indicator of whether in the database there are some exercises written using Sweave code; default is FALSE, if set to TRUE *.rnw files will be generated and compiled to *.tex (or *.pdf) files. See details }
  \item{compile.pdf}{Indicator of whether the *.tex files should be compiled to *.pdf files (compiled if set to TRUE.) See details}
  \item{my.final.sentence}{A string with a sentence that will be written in bold at the end of each homeworks. If set to NULL (default) no sentence will be displayed.}
  
}
\details{
The homework data base must a tab delimited file, prepared using a similar structure as the example data from this library. The inclusion of some variables is mandatory (Group, Type, HomeworkText, DueDate, Template, Order) other variables can be included. See \code{\link{my.data.homeworks}} for more details. 
If the mandatory variables have different names, the order of the columns in which they appear can be specified using the list.id.homeworks argument. 

The students data base must a tab delimited file, prepared using a similar structure as the example data from this library. The inclusion of some variables is mandatory (Group, Name) other variables can be included. See \code{\link{my.data.students}} for more details. 
If the mandatory variables have different names, the order of the columns in which they appear can be specified using the list.id.students argument. 

If use.Sweave=TRUE,   a call to the Sweave package will be included in the header of the *.rnw and *.tex files. For this reason Sweave package must be available to LaTeX compiler. File Sweave.sty can be included in the directory my.outdir in case of problems related to package unavailability. 
If compile.pdf=TRUE, the *.tex  files will be compiled into *.pdf files using \code{texi2dvi} function. 
%MikTeX must be installed locally and its executables (as pdflatex.exe) must be available from my.outdir directory. See MikTeX documentation. 
If the user wishes to use a different program to compile the LaTeX files, set compile.pdf=FALSE and manually compile the *.tex files outside R.}
\value{
Writes LaTeX (od PDF) files containing the texts of the exams and returns a list containing the IDs of the questions selected for each test and the list of the files *.tex files that were generated. It also merges the homeworks in a single PDF file.

  \item{files}{List of the generated files (*.tex or *.pdf, depending on the selected options)}
  \item{errors}{A string with the errors encountered during the execution of the function, if any.)}
}
\references{\url{http://sites.google.com/site/lara3107/Home/software/genertest}, \url{https://github.com/llaarraa/genertest}}
\author{Lara Lusa}
\note{}
\examples{

#read the name of the example database with homeworks
my.db.homeworks.name=paste(path.package("genertest"),"/extdata/HomeworkList.txt",sep="")

#read the name of the example database with the list of students
my.db.students.name=paste(path.package("genertest"),"/extdata/StudentList.txt",sep="")



TMP=generateHomeworks(my.db.homeworks.name, my.db.students.name,  my.outdir=NULL,  my.seed=1999, 
                    generate.solutions="FALSE", 
                    my.title="Homework - Biostatistics", my.date="Academic Year 2012/2013", 
  				list.id.homeworks=list(id.group=NULL, id.type=NULL, id.source=NULL, id.text=NULL, id.date=NULL, id.template=NULL, id.order.fileAll=NULL),
					list.id.students=list(id.group.names=NULL, id.students.names=NULL),
					source.text="Sources",
					template.text="Template to use",
					date.text="Due date",
					group.members.text="Group members",
					my.language="english", use.Sweave=TRUE, 
                    compile.pdf=TRUE,
                    my.final.sentence=NULL) 
					
	




}
\seealso{\code{\link{write.all}}, \code{\link{my.data}}}
\keyword{misc}
