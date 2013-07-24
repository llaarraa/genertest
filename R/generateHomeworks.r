 ############################# seminarske naloge #######################
 

generateHomeworks=function(my.db.homeworks.name, my.db.students.name,  my.outdir=NULL,  my.seed=1999, 
                    generate.solutions="FALSE", 
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
                    my.final.sentence=NULL) {
					
					
				 my.error<<-"No errors were found" 	
 
			    #modified to read data from a character string
			    #my.data=read.delim(my.db.homeworks.name, sep="\t", na.strings = c("NA", "") )
				#my.data.groups=read.delim(my.filename.groups, sep="\t", na.strings = c("NA", ""))

 
				#read the assignment database	
			    my.data<-try(read.delim(as.character(my.db.homeworks.name), sep="\t", blank.lines.skip=TRUE, na.strings=c("NA", "")))
			    if(class(my.data)=="try-error") {my.error<<-"I cannot open the file containing the homeworks"
											   stop("I cannot open the file containing the questions")} 


			 #treat the empty cells as missing!
			  my.data[my.data==""]<-NA
			  #remove lines where all the entries are missing
			  my.data<-my.data[!(apply(my.data, 1, function(x) all(is.na(x)))),]
				 
											   
								   
				#read the students database
				my.data.groups<-try(read.delim(as.character(my.db.students.name), sep="\t", blank.lines.skip=TRUE, na.strings=c("NA", "")))
					if(class(my.data.groups)=="try-error") {my.error<<-"I cannot open the file containing the homeworks"
											   stop("I cannot open the file containing the questions")} 

 
			 #treat the empty cells as missing!
			  my.data.groups[my.data.groups==""]<-NA
			  #remove lines where all the entries are missing
			  my.data.groups<-my.data.groups[!(apply(my.data.groups, 1, function(x) all(is.na(x)))),]
 
 
			#checking if all the information are provided in the databases
			
			#the user did not provide the ids of the columns, check if the naming in the file is correct
			if(is.null(unlist(list.id.homeworks))){
									id.group=which(toupper(names(my.data))=="GROUP")
									if(length(id.group)!=1){my.error<<-"In the data base with the homeworks there is no variable called Group, which should indicate the ID of the group to which the homeworks was assigned. Please name Group one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Group, which should indicate the ID of the group to which the homeworks was assigned. Please name Group one of the variables or provide the column number using the list.id.homeworks argument.")
										
									}
									
									id.type=which(toupper(names(my.data))=="TYPE")
									if(length(id.type)!=1){my.error<<-"In the data base with the homeworks there is no variable called Type, which should indicate the type of assignment given to the the group. Please name Type one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Type, which should indicate the type of assignment given to the the group. Please name Type one of the variables or provide the column number using the list.id.homeworks argument.")
										
									}
			
									id.source=which(toupper(names(my.data))=="SOURCES")
									if(length(id.source)!=1){my.error<<-"In the data base with the homeworks there is no variable called Sources, which should indicate the sources that should be used for the given assignment to the the group. Please name Type one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Sources, which should indicate the sources that should be used for the given assignment to the the group. Please name Type one of the variables or provide the column number using the list.id.homeworks argument.")
										
									}
			
									id.text=which(toupper(names(my.data))=="HOMEWORKTEXT")
									if(length(id.text)!=1){my.error<<-"In the data base with the homeworks there is no variable called HomeworkText, which should indicate the text of the given assignments. Please name HomeworkText one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Sources, which should indicate the sources that should be used for the given assignment to the the group. Please name Type one of the variables or provide the column number using the list.id.homeworks argument.")
										
									}
								
									id.date=which(toupper(names(my.data))=="DUEDATE")
									if(length(id.date)!=1){my.error<<-"In the data base with the homeworks there is no variable called Date, which should indicate the due date of the assignments. Please name Date one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Date, which should indicate the due date of the assignments. Please name Date one of the variables or provide the column number using the list.id.homeworks argument.")
										
									}
			
									id.template=which(toupper(names(my.data))=="TEMPLATE")
									if(length(id.template)!=1){my.error<<-"In the data base with the homeworks there is no variable called Template, which should indicate the template to be used to prepare the assignment. Please name Template one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Template, which should indicate the template to be used to prepare the assignment. Please name Template one of the variables or provide the column number using the list.id.homeworks argument.")
									}
									
								id.order.fileAll=which(toupper(names(my.data))=="ORDER")
									if(length(id.template)!=1){my.error<<-"In the data base with the homeworks there is no variable called Order, which should indicate the order of the assignment. Please name Order one of the variables or provide the column number using the list.id.homeworks argument."
															stop("In the data base with the homeworks there is no variable called Order, which should indicate the order of the assignment. Please name Order one of the variables or provide the column number using the list.id.homeworks argument.")
									
									
									}
			
		
			}#if(is.null(unlist(list.id.homeworks))
			
			
				#the user did not provide the ids of the columns for the students database, check if the naming in the file is correct
				
				
				
			if(is.null(unlist(list.id.students))){
									id.group.names=which(toupper(names(my.data.groups))=="GROUP")
									if(length(id.group.names)!=1){my.error<<-"In the data base with the homeworks there is no variable called Group, which should indicate the ID of the group to which the homeworks was assigned. Please name Group one of the variables or provide the column number using the list.id.students argument."
															stop("In the data base with the homeworks there is no variable called Group, which should indicate the ID of the group to which the homeworks was assigned. Please name Group one of the variables or provide the column number using the list.id.students argument.")
										
									}
									
									id.students.names=which(toupper(names(my.data.groups))=="NAME")
									if(length(id.students.names)!=1){my.error<<-"In the data base with the homeworks there is no variable called Name, which should indicate the names of the group members. Please name Name one of the variables or provide the column number using the list.id.students argument."
															stop("In the data base with the homeworks there is no variable called Names, which should indicate the names of the group members. Please name Name one of the variables or provide the column number using the list.id.students argument.")
										
									}
					
			}#if(is.null(unlist(list.id.students))
			
			
			
 
				 my.oldwd<-getwd()
   
			  #checking if the directory specified to store the results exists
			  if(is.null(my.outdir)) {#my.outdir=paste("Exams", date())
									  #define the names of the directory where the files will be stored, creates a subdirectory of the working directoty 
									  
									
									  my.outdir=paste(my.oldwd, paste("Homeworks",format(Sys.Date(), "%b%d%Y"), format(Sys.time(), "%H%M"), sep=""), sep="/")
									  
									  my.command=paste("mkdir", my.outdir)
									  system(my.command)
									} else {
									  if(is.na(file.info(my.outdir)$isdir)) {my.error<<-"The directory that you choose to store the results does not exist, please specify an existing directory or leave the my.outdir argument empty."
									  stop("The directory that you choose to store the results does not exist. Please specify an existing directory of leave the my.outdir argument empty. A directory named Homeworks + the current date and time will be created in your working directory.")}
									}
			  
			  #set my.outdir as a character
			  my.outdir<-as.character(my.outdir)
			  
 
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
					names.group.members=my.data.groups[as.character(my.data.groups[,id.group.names])==as.character(my.data.h[i,id.group]),id.students.names]


					#cat("\\section{Data on the group}")
					cat("\\section{", as.character(text.groupdata), "}")
          
					cat("{\\bf", group.members.text, ":",  "}")
					# cat(as.character(names.group.members), sep="\\\\")
					 cat(as.character(names.group.members), sep=", ")
					cat("\\\\{\\bf", date.text, ":", as.character(my.data.h[i,id.date]),   "}\\\\")
					cat("{\\bf", source.text, ":",  "}")
					cat(as.character(my.data.h[i,id.source]), sep="\\\\")
					cat("\\\\{\\bf", template.text, ":",  as.character(my.data.h[i,id.template]),  "}\\\\")
					###########  text of the homework #################
					#cat("\\section{Assignment}")
					cat("\\section{", as.character(text.assignment), "}")
					
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


					#my.command<-paste("pdflatex", my.file)
					##system(my.command) modified 15/2/2010 to take check for possible errors in pdflatex function, and stop the function in case of errors
					#      out.pdflatex<-try(system(my.command) )

					#July 2013: use texi2dvi instead of pdflatex  
					  out.pdflatex=try(texi2dvi(my.file, pdf=TRUE, clean=TRUE, quiet=TRUE))
						  
						  #if there was an error - returns a code different than 0
						  if(!is.null(out.pdflatex))         {
					 
					 #if(out.pdflatex!=0)         {
						   #return to the original directory
						   setwd(my.oldwd)
					my.error<<-"There was an error in compiling LaTeX in PDF files with texi2dvi - more details are displayed in the R console"
					stop("There was an error compiling the LaTeX file(s)")
					}# end out.pdflatex, error in pdf compilation
					} #end for i
						 setwd(my.oldwd)
					 }#end if compile.pdf
					 
					 
						 
					for(i in 1:length(my.files)){
							if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
							}	
							 
						 
						 
		 
		
					my.filenames.nopath=paste(my.data.h[,id.group], "pdf", sep=".")


						 Merge.pdf(my.files=my.filenames.nopath[order(my.data.h[,id.order.fileAll])], my.dir=my.outdir, outfile="AllSortedWeek")

						 
						 Merge.pdf(my.files=my.filenames.nopath, my.dir=my.outdir, outfile="AllNotSorted")

						 
	 
	  
  return(list(files=my.files, errors=my.error))   
	 
	 }#end generateHomeworks
	 