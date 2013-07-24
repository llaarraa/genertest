 ############################# seminarske naloge #######################
 
 ################ 2012/2013, tbs #####################################################
 

#laptop
my.datadir="C:\\Users\\lara\\Dropbox\\didatticaStat\\tbs1213\\seminars"
my.outdir="C:\\Users\\lara\\Dropbox\\didatticaStat\\tbs1213\\seminars\\navodila"

my.filename.seminars="SeminarList_2013.txt"
my.filename.groups="StudentList_2013.txt"


setwd(my.datadir)
#source("C:\\Documents and Settings\\lara\\My Documents\\My Dropbox\\didatticaStat\\tbs1112\\seminarji\\scripts/functionsGenerateInstructions.r")
source(paste(my.datadir, "/scripts/functionsGenerateInstructions.r", sep=""))

my.language="slovene"
my.data=read.delim(my.filename.seminars, sep="\t", na.strings = c("NA", "") )
my.data.groups=read.delim(my.filename.groups, sep="\t", na.strings = c("NA", ""))
#remove lines where all the entries are missing
my.data<-my.data[!(apply(my.data, 1, function(x) all(is.na(x)))),]
#columns of the database that will be used
#column with the names of the group 
id.group=2
#column with the type of homework
id.type=5
#column with the links
id.source=6
#column with the text
id.text=7
#column with the due date
#id.date=10
id.date=17
#column with the template
id.template=11
#column of the database with group name in the database with the students names and group membership
id.group.names=3
#column with the names of the students in the database with the students names and group membership
id.names.names=1
my.date="\\v{S}olsko leto 2012/2013"

#column with the order of the 
id.order.fileAll=16


group.members.text="\\v{C}lani skupine"
date.text="Datum seminarja"
template.text="Predloga"
source.text="Viri ali podatki"
my.title="Seminarska naloga iz biostatistike: skupina "
#my.final.sentence="Good luck"
my.final.sentence="Uspešno reševanje!"


#whether to compile tex files with pdflatex
compile.pdf=TRUE





#checking if outdir exists
if(is.na(file.info(my.outdir)$isdir)) {my.error<<-"The directory that you choose to store the results does not exist"
stop("The directory that you choose to store the results does not exist")} 
#checking if there is a single howework for each group
 #indicator of which lines in the database contain the beginning of a question - and the info about the
    #row-ID of the database of questions (and not QUESTION.ID!), moved up 18/2/2010
    which.questions<-which(!is.na(my.data[,id.group]))
#number of groups
num.groups=length(which.questions)
#modified 18/2/2010: checking if all Questions.IDs are different
#temporary removed 26/2/2012
#  if(sum(duplicated(my.data[,id.group][which.questions]))>0) {
#~      which.duplicated.QID<<-my.data[,id.group][which.questions][which(duplicated(my.data[,id.group][which.questions]))]
#      my.error<<-as.character(c("Question.ID must be unique for each question. Questions that have repeated IDs are:", as.character(which.duplicated.QID))) 
#stop("Question.ID must be unique for each question")}






my.filenames<-vector("character", num.groups)
    #how many questions are contained in the database
    how.many.questions<-length(which.questions)   
    #end of questions row-ID
    end.questions<-c(which.questions[-1]-1, dim(my.data)[1])
      #NB: the ID in index.questions.vector refers to the row of the questions database where the question appears
       index.questions.vector<-which(!is.na(my.data[,id.group]))
   end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) {
        #cat(i, "\n")
        tmp<-0;
        my.done<-FALSE;
       # for(k in index.questions.vector[i]:dim(my.data)[1]){ modified 20/7/2009 when the "end question" was removed
  #  for(k in index.questions.vector[i]:(   max(index.questions.vector[i], dim(my.data)[1]-1)    )){ 
  if(index.questions.vector[i]==dim(my.data)[1])    return(index.questions.vector[i]) else {
    for(k in index.questions.vector[i]:(   dim(my.data)[1]-1    )){ 
           # if(is.na(my.data$Question.ID[k+1]) | my.data$Question.ID[k+1]!="") {tmp<-tmp+1} else break;
           if(is.na(my.data[,id.group][k+1]) ) {tmp<-tmp+1} else break;
        }#end for
       return(index.questions.vector[i]+tmp)}
    }))

	
#defining the database with only the headers
my.data.h=my.data[which.questions,]

my.data.h[,id.group]=as.character(my.data.h[,id.group])

###### 26/2/2012, added to generate the seminars also when the groups has not be assigned yet
if(any(my.data.h[,id.group]=="Nobody"))
my.data.h[my.data.h[,id.group]=="Nobody",id.group]=paste("Nobody", 1:sum(my.data.h[,id.group]=="Nobody", na.rm=T), sep="")

if(any(my.data.h[,id.group]=="#N/A"))
my.data.h[my.data.h[,id.group]=="#N/A", id.group]=paste("NotAssigned", 1:sum(my.data.h[,id.group]=="#N/A", na.rm=T), sep="")


my.data.h[,id.group]=factor(my.data.h[,id.group])



for(i in 1:num.groups){
cat(i, "\n\n\n")
#generating the name of the file
zz<-paste(my.outdir, "\\", my.data.h[i,id.group], ".tex", sep="")
#saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
#tests 
my.filenames[i]<-zz

  my.title.group<-paste(my.title, "\\bf{", my.data.h[i, id.group], "}", sep=" ")
    sink(zz)
  cat("\\documentclass{article}", paste("\\title{", my.title.group, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), 
  "\\usepackage[cp1250]{inputenc}",
  "\\usepackage{hyperref}",
  "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage[pdftex]{graphicx,color}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n") 
names.group.members=my.data.groups[as.character(my.data.groups[,id.group.names])==as.character(my.data.h[i,id.group]),id.names.names]


cat("\\section{Podatki o skupini}")
cat("{\\bf", group.members.text, ":",  "}")
# cat(as.character(names.group.members), sep="\\\\")
 cat(as.character(names.group.members), sep=", ")
cat("\\\\{\\bf", date.text, ":", as.character(my.data.h[i,id.date]),   "}\\\\")
cat("{\\bf", source.text, ":",  "}")
cat(as.character(my.data.h[i,id.source]), sep="\\\\")
cat("\\\\{\\bf", template.text, ":",  as.character(my.data.h[i,id.template]),  "}\\\\")
###########  text of the homework #################
cat("\\section{Vsebina naloge}")
cat(as.character(my.data.h[i, id.text]), "\\\\")
#if there are any subquestions
if(index.questions.vector[i]-end.index.questions.vector[i]!=0){
cat("\\begin{itemize}\n")
#print the subquestions
for(k in (index.questions.vector[i]+1):end.index.questions.vector[i]){
cat("\\item",  as.character(my.data[k, id.text]), "\n")
}#end for k
cat("\\end{itemize}\n")
}#end if there are subquestions
cat("\\vspace{\\baselineskip} {\\bf", as.character(my.final.sentence), "}")
    cat("\\newpage\n")
    cat("\\end{document}")
   sink()
}#end for i
my.files=my.filenames
if(compile.pdf){
my.oldwd<-getwd()
setwd(my.outdir)
#use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
#if(generate.solutions) my.files<-c(my.filenames, my.filenames.sol) else my.files<-my.filenames
for(i in 1:length(my.files)){
my.file<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
my.file<-unlist(strsplit(my.file,  "\\\\"))
my.file<-my.file[length(my.file)]
my.command<-paste("pdflatex", my.file)
##system(my.command) modified 15/2/2010 to take check for possible errors in pdflatex function, and stop the function in case of errors
      out.pdflatex<-try(system(my.command) )
 if(out.pdflatex!=0)         {
       #return to the original directory
       setwd(my.oldwd)
my.error<<-"There was an error in compiling LaTeX in PDF files with pdflatex - more details are displayed in the R console"
stop("There was an error compiling the LaTeX file(s)")
}# end out.pdflatex, error in pdf compilation
} #end for i
     setwd(my.oldwd)
 }#end if compile.pdf
 
 
 	 
for(i in 1:length(my.files)){
		if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
		}	
	 	 
 	 
 	 
 	 
 	 #my.dir="C:\\Documents and Settings\\lara\\My Documents\\My Dropbox\\didatticaStat\\tbs1112\\seminarji\\navodila"
	 #my.dir="C:\\Documents and Settings\\lara\\My Documents\\My Dropbox\\didatticaStat\\tbs1112\\seminarji\\navodila"
 	 #Merge.pdf<-function(my.files, my.dir, outfile="merge"){

my.filenames.nopath=paste(my.data.h[,id.group], "pdf", sep=".")


 	 Merge.pdf(my.filenames.nopath[order(my.data.h[,id.order.fileAll])], my.outdir, outfile="AllSortedWeek")

	 
 	 Merge.pdf(my.filenames.nopath, my.outdir, outfile="AllNotSorted")

	 