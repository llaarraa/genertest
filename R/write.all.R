`write.all` <-
function(my.data, my.outdir, my.seed=1999, my.include=NA, generate.solutions=FALSE, my.title="", my.date="", my.prefix="exam",     head.name="Name", head.id="ID number", head.points="Number of points", head.prefix="MED", my.language="english", use.Sweave=FALSE, compile.pdf=FALSE){
   
    ##################################################
    ##############FUNCTION ARGUMENTS##################
    ##################################################
   
    #my.data: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be given, should be a string, written between quotes
    #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be created by the user before the function is called
    #my.seed: seed used to inizialize the random number generator, useful to get reproducibile results
    #my.include: the number of the column in which a TRUE/FALSE indicator in included, to indicate which questions should be included (or excluded), if left to defalut NA all the questions should be considere for inclusion, it uses the column number rather than a fixed name since in principle the same database can be used to generate test for different courses or of different difficulties
    #generate.solutions: if set to TRUE also the solutions of the tests is generated
    #my.title: name of the exam - to be displayed in the header
    #my.date: date of the exam  - to be displayed in the header
    #my.prefix: string with which the names of the latex files of the tests begins
    #informations that will be required in the header, can be changed into different languages, or requiring other informations
    #head.name<-"Name": string indicating the name of the student - to be displayed in the header (can be changed if a language different than English is used)
    #head.id<-"ID number": string indicating the identification number of the student - to be displayed in the header (can be changed if a language different than English is used)
    #head.points<-"Number of points": string indicating how to indicate the number of points - to be displayed in the header (can be changed if a language different than English is used)
    #head.prefix<-"MED" -> will generated the test id, pasting this prefix, a random number and the number of the test
    #my.language: language in which the test is writted - to be used to calls a LaTeX library that contains the appropriate language settings
    #use.Sweave: are some exercises written using Sweave code (default is FALSE)
    #compile.pdf: logical, if set to true, pdf files will be generated, otherwise only tex files 		this part of the program will work only if the user has pdflatex.exe (MikTeX) on its computer and the program is accessible from any directory (in Windows, included in the path)
	
    ######################
    # supporting functions
    #######################

    resample <- function(x, size, ...)
    if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
      
    ######################
    # end of supporting functions
    #######################

   
    #read data
    my.data<-read.delim(my.data, sep="\t", blank.lines.skip=TRUE)

   
    ######################
    #initialize objects needed later
    ######################
   
    #set random seed - for the reproducibility of the results
    set.seed(my.seed)
    #list where the question id
    #Remember.questions.index<-Remember.questions.index.end<-vector("list", 1)
    #list in which the questions to exclude due to minimum distance to keep between tests
   
   
    ###############################################
    ######### and check of the database############
	###############################################
    
	#for(tt in 1:num.tests){
   
	#check if there is a variable called Question.ID in the database of questions, if not stop
	if(is.null(my.data$Question.ID)) stop("You need to include a column called Question.ID is the database of questions")
	
		
    #indicator of which lines in the database contain the beginning of a question - and the info about the
    #row-ID
    which.questions<-which(!is.na(my.data$Question.ID))
	
	
	#check if there is a column called Points in the database of questions, if not stop 
	if(is.null(my.data$Points)) 
		stop("You need to include a column called Points is the database of questions")

	#check if there is a column called Points in the database of questions, if not stop 
	if(is.null(my.data$Question)) 
		stop("You need to include a column called Question is the database of questions")

		
	#if there is no column called PermuteOK in the database of questions, add one, with indicator FALSE for all the questions (sub-questions will not be permuted)
	if(is.null(my.data$PermuteOK)) 
		{my.data$PermuteOK<-NA 
		my.data$PermuteOK[which.questions]<-FALSE
		}
		
	#check if Topics were specified	
	if(is.null(my.data$Topic)) 
		{my.data$Topic<-rep("", dim(my.data)[1])}	
  
	#number of blank lines to be left after each question and sub-question was not specified, no extra-space will be left
	if(is.null(my.data$ExtraLines)) my.data$ExtraLines<-rep(NA, dim(my.data)[1])
  
  
	#check if there the Points were specified for each question in the database of questions, if not stop 
	if(any(is.na(my.data$Points[which.questions]))) 
		stop("You need to specify the number of points for each of the question included in the database")

  
   #ExcludeIf, the list of questions  was not included in the database of questions
   if(is.null(my.data$ExcludeIf)) 
	my.data$ExcludeIf<-rep("", dim(my.data)[1])
   
   
  #############selects all the questions that are not filtered out by my.include##################
   
    #indicator of which lines in the data base contain the beginning of a question - and the info about the
    #row-ID
	if(!is.na(my.include)) 
		index.questions.vector<-which(!is.na(my.data$Question.ID) & my.data[,my.include]) else 
       index.questions.vector<-which(!is.na(my.data$Question.ID))
	   
    #how many questions are contained in the data base
    how.many.questions<-length(index.questions.vector)   

       
    #permute the order of the questions, given as the row in which the header of the question appears in the database
    Remember.questions.index<-index.questions.vector
    end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) {
        #cat(i, "\n")
        tmp<-0;
        my.done<-FALSE;
        for(k in index.questions.vector[i]:dim(my.data)[1]){
            if(is.na(my.data[k+1,2])) {tmp<-tmp+1} else break;
           
        }
        index.questions.vector[i]+tmp}
        ))
   
    Remember.questions.index.end<-end.index.questions.vector
       
    #number of questions selected
    num.questions<-length(index.questions.vector)
   
   
   
   

    ##############################################
    # beginning of writing of the file
    #############################################
   
	#generating the name of the file with the exams
   if(use.Sweave)     zz<-paste(my.outdir, "\\", my.prefix, "All", ".rnw", sep="") else     zz<-paste(my.outdir, "\\", my.prefix, "All", ".tex", sep="")
     #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
	 #tests 
	

	#if also the solutions need to be generated
	if(generate.solutions)  {

		#check if there was no Answer column in the database of questions
		if(is.null(my.data$Answer)) {my.data$Answer<-rep("", dim(my.data)[1])
								warning("You did not include a column with the answers in the database of questions - it must be called: Answer")	
									}
	
		#generate the name of the file with the answers
		if(use.Sweave) zz.sol<-paste(my.outdir, "\\",  my.prefix,  "All", "sol",  ".rnw", sep="") else zz.sol<-paste(my.outdir, "\\",  my.prefix,  "All", "sol",  ".tex", sep="")
	
		#saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
		#solutions 
	}#end if generate.solutions
	
	
    my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}", head.prefix,     round(runif(1)*100),  " }", sep="")


	 
    sink(zz)
#    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 "\\usepackage[slovene]{babel}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
   
   
    #preamble of the latex file

    #different header if Sweave is used
    if(use.Sweave){
            cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n") 
		if(generate.solutions)             cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) 
					}			else     {cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")

	if(generate.solutions) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)
				
   }#end else if use.Sweave
   
    cat(my.header)
	if(generate.solutions)    {cat(my.header, file=zz.sol, append=TRUE)}

    #beginning of the questions
	cat("\\begin{itemize}\n")
	if(generate.solutions) cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)

	#save the random seed at the beginning of writing of the file
	my.old.seed<-.Random.seed

	
	#excluding the "end" question
	for(i in 1:(length(index.questions.vector)-1)){


    cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
	if(generate.solutions) {cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE)
	if(my.data$Answer[index.questions.vector[i]]!="" |  !is.na(my.data$Answer[index.questions.vector[i]])) 
		cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
	}
   
    if(!is.na(my.data$ExtraLines[index.questions.vector[i]])) {
		cat((rep("\\newline", my.data$ExtraLines[index.questions.vector[i]], sep="")))
		if(generate.solutions) cat((rep("\\newline", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE)}
    if(end.index.questions.vector[i]!=index.questions.vector[i]){
        cat("\\begin{enumerate}[(a)]\n") 
		if(generate.solutions) cat("\\begin{enumerate}[(a)]\n", file=zz.sol, append=TRUE) 
        if(my.data[index.questions.vector[i],]$PermuteOK==TRUE)
        perm.order<-resample(c((index.questions.vector[i]+1): end.index.questions.vector[i])) else perm.order<-c((index.questions.vector[i]+1): end.index.questions.vector[i])

        for(j in perm.order){
            cat("\\item ") 
            cat(as.character(my.data$Question[j]), "\n")
			if(!is.na(my.data$ExtraLines[j])) cat((rep("\\newline", my.data$ExtraLines[j], sep="")))
			if(generate.solutions) {
				cat("\\item ", file=zz.sol, append=TRUE) 
				cat(as.character(my.data$Question[j]), "\n", file=zz.sol, append=TRUE)
				if(my.data$Answer[j]!="" |  !is.na(my.data$Answer[j])) 
		cat("{\\bf", as.character(my.data$Answer[j]), "}",  file=zz.sol, append=TRUE)
				if(!is.na(my.data$ExtraLines[j])) cat((rep("\\newline", (my.data$ExtraLines[j]-1), sep="")), file=zz.sol, append=TRUE)
			}#end if generate.solutions
		}#end for j
    cat("\\end{enumerate}\n")
	cat("{\\bf Topic: ", as.character(my.data$Topic[index.questions.vector[i]]), ". Excluded when including question(s): ", my.data$ExcludeIf[index.questions.vector[i]], "}", "\n") 

	if(generate.solutions) {cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)
	cat("{\\bf Topic: ", as.character(my.data$Topic[index.questions.vector[i]]), ". Excluded when including question(s): ", my.data$ExcludeIf[index.questions.vector[i]], "}", "\n", file=zz.sol, append=TRUE)} 
	}#end if beginning and end of question differ
	
	
}#end for i

    cat("\\end{itemize}\n")
    cat("\\newpage\n")
    cat("\\end{document}")
	if(generate.solutions) {
	    cat("\\end{itemize}\n", file=zz.sol, append=TRUE)
		cat("\\newpage\n", file=zz.sol, append=TRUE)
		cat("\\end{document}", file=zz.sol, append=TRUE)
	}
    sink()

   
      
       
 
   


################################
#compiling Sweave files 
################################


 if(use.Sweave){
		#obtaing tex files from rnw files, Sweave must be accessible from the directory my.outdir
		#my.files<-dir(path=my.outdir, pattern=".rnw", full.names=TRUE)   
		my.oldwd<-getwd()
		setwd(my.outdir)
		
		#for reproducibility of the solutions, save the seed
		#my.old.seed<<-.Random.seed
		
		#use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
		
			set.seed(i+my.seed)
			shell(Sweave(zz))
			if(generate.solutions){
				set.seed(i+my.seed)
				shell(Sweave(zz.sol))}
		
					
		
		setwd(my.oldwd)	

	
	}

    if(generate.solutions) my.files<-c(zz, zz.sol) else my.files<-zz

################################
#compiling tex files to pdf
################################

	#if compile.pdf=TRUE it generates the pdf files from the tex files
	if(compile.pdf){
		my.oldwd<-getwd()
		setwd(my.outdir)
		#use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
		#if(generate.solutions) my.files<-c(zz, zz.sol) else my.files<-zz

		for(i in 1:length(my.files)){
			my.file<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")	
			my.file<-unlist(strsplit(my.file,  "\\\\"))
			my.file<-my.file[length(my.file)]
			my.command<-paste("pdflatex", my.file)
			system(my.command)
			
		}
     setwd(my.oldwd)	
	}
	
    #names(Remember.questions.index)<-paste("Test", 1:num.tests)   
	#close the open connections, if any - can be problematic if errors occur and the function is stopped
	
	#saving the names of the final output files, tex or pdf, depending on the selected options
	for(i in 1:length(my.files)){
		if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
		}	
			
	
	
	
	on.exit(if(sink.number()!=0) for(i in 1:sink.number()) sink())
return(list(Questions=Remember.questions.index, files=my.files))       
}#end function write.all()   

