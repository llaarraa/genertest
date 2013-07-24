
##############################################genertestPreSelect#################################################################
#10/2/2010
#function that can be used to generate the tests if the user wishes to pre-specify which questions should be included in each test
#no topics/number of points or distance are specified, just the question IDs to include in each test
#basically the same function as genertest, but it does not select the questions, instead it reads it from a list specified by the user
#updated: July 2013. texi2dvi is used instead of pdflatex.
      
	  
genertestPreSelect<-function(my.db.name, my.outdir, list.QID, num.tests=NULL, 
                            repeat.each.test=1, my.seed=1999, 
                            generate.solutions=FALSE, my.title="Exam", my.date="Today", my.prefix="exam",     head.name="Name", head.id="ID number", 
                            head.points="Number of points", head.prefix="MED", my.language="english", 
                            use.Sweave=FALSE, compile.pdf=FALSE, my.final.sentence=NULL){
   
    #on.exit: close open connections, if any
   	on.exit(if(sink.number()!=0) for(i in 1:sink.number()) sink())

   
    ##################################################
    ##############FUNCTION ARGUMENTS##################
    ##################################################
   
    #my.data: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be given, should be a string, written between quotes
    #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be created by the user before the function is called
    #list.QID: list containing the question IDs (as the appear in the database of questions) for each test, the length of the list corresponds to the number of different tests that have to generated
	  #num.tests: number of different tests to be generated
    #repeat.each.test: number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation
    #my.seed: seed used to inizialize the rand
   
    ##################################################
    ##############FUNCTION ARGUMENTS##################
    ##################################################
   
    #my.data: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be given, should be a string, written between quotes
    #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be created by the user before the function is called
	  #list.QID: list with the IDs of the questions to be used in each test. Each element of the list defines a test.
    #num.tests: number of different tests to be generated, equal to the number of elements of the list.QID list
    #repeat.each.test: number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation
    #my.seed: seed used to inizialize the random number generator, useful to get reproducibile results
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
	  #my.final.sentence: string with a sentence to be written (in bold) at the end of each test
    ######################
    # supporting functions
    #######################

    resample <- function(x, size, ...)
    if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
    
	my.error<<-"No errors were found" 	
	

	
	
    ######################
    # end of supporting functions
    #######################

   
   #read data
	 #modified to read data from a character srintg
   my.data<-try(read.delim(as.character(my.db.name), sep="\t", blank.lines.skip=TRUE))
	 if(class(my.data)=="try-error") {my.error<<-"I cannot open the file with questions"
									stop("I cannot open the file with questions")} 
									
	#treat the empty cells as missing!
	my.data[my.data==""]<-NA
	#remove lines where all the entries are missing
	my.data<-my.data[!(apply(my.data, 1, function(x) all(is.na(x)))),]
	
	

   	#checking if the directory specified to store the results exists
   	if(is.null(my.outdir)) {#my.outdir=paste("Exams", date())
   	  #define the names of the directory where the files will be stored, creates a subdirectory of the working directoty 
   	  
   	  my.oldwd<-getwd()
   	  my.outdir=paste(my.oldwd, paste("Exams",format(Sys.Date(), "%b%d%Y"), format(Sys.time(), "%H%M"), sep=""), sep="/")
   	  
   	  my.command=paste("mkdir", my.outdir)
   	  system(my.command)
   	} else {
   	  if(is.na(file.info(my.outdir)$isdir)) {my.error<<-"The directory that you choose to store the results does not exist, please specify an existing directory or leave the my.outdir argument empty."
   	                                         stop("The directory that you choose to store the results does not exist. Please specify an existing directory of leave the my.outdir argument empty. A directory named Exams + the current date and time will be created in your working directory.")}
   	}
   	
   	#set my.outdir as a character
   	my.outdir<-as.character(my.outdir)   
     
	
	
    ######################
    #initialize objects needed later
    ######################
   
    #set random seed - for the reproducibility of the results
    set.seed(my.seed)
    num.test=length(list.QID) 
    #list where the question id
    Remember.questions.index<-Remember.questions.index.end<-vector("list", num.tests)

	#check if there is a variable called Question.ID in the database of questions, if not stop
	if(is.null(my.data$Question.ID)) {my.error<<-"You need to include a column called Question.ID or Question ID in the database of questions" 
			stop("You need to include a column called Question.ID is the database of questions")}

    #indicator of which lines in the database contain the beginning of a question - and the info about the
    #row-ID, moved up 12/2/2010
    which.questions<-which(!is.na(my.data$Question.ID))
			
	
    #derive the number of the record of the questions selected by their Question ID  (included in  list.QID)
  	#added 12/2/2010: check if all the questions IDs are contained in the questions' database
	
	#istop if not not all the questionsIds are included in the questions database
	match.id<-is.element(unlist(list.QID), my.data$Question.ID[which.questions])
	
	
	if(!all(match.id)){
	
	###### correggere! 12/2/2010
	unique.not.matched<-unique(unlist(list.QID)[!match.id])
	my.error<<-as.character(c("Some of the questions that you selected are not present in the database of questions (Question ID does not match)\n Missing questions are: ", as.character(unique.not.matched)))
			stop("Please make sure that all the questions that you selected appear in the database of questions (Question.ID column)")
			}

	#matching	
	Remember.questions.index<-lapply(list.QID, function(x) which(is.element(my.data$Question.ID, x )) )



  	#### at the end it is converted again to QIDs: Remember.questions.index<-lapply(Remember.questions.index, function(x) my.data$Question.ID[x])

    
    
    #list in which the questions to exclude due to minimum distance to keep between tests
    
    
  
	   
	
	
	#check if there is a column called Points in the database of questions, if not stop 
	if(is.null(my.data$Points)) { my.error<<-"You need to include a column called Points is the database of questions"
		stop("You need to include a column called Points is the database of questions")} 

	#check if there is a column called Points in the database of questions, if not stop 
	if(is.null(my.data$Question)) {my.error<<-"You need to include a column called Question is the database of questions"
		stop("You need to include a column called Question is the database of questions")}

		
	#if there is no column called PermuteOK in the database of questions, add one, with indicator FALSE for all the questions (sub-questions will not be permuted)
	if(is.null(my.data$PermuteOK)) 
		{my.data$PermuteOK<-NA 
		my.data$PermuteOK[which.questions]<-FALSE
		}
  
	#if permuteOK is missing for some of the questions set it to FALSE
	my.data$PermuteOK[which.questions][is.na(my.data$PermuteOK[which.questions])]<-FALSE
  
  
	#number of blank lines to be left after each question and sub-question was not specified, no extra-space will be left
	if(is.null(my.data$ExtraLines)) my.data$ExtraLines<-rep(NA, dim(my.data)[1])
  
  
  	#if there is no column called Include in the database of questions, add one, with indicator TRUE for all the questions (all the questions will not included)
	if(is.null(my.data$Include)) 
		{my.data$Include<-NA 
		my.data$Include[which.questions]<-TRUE
		}


   
	#includes the questions for which the Include indicator is missing
	my.data$Include[which.questions][is.na(my.data$Include[which.questions])]<-TRUE
  
  
	#check if the Points were specified for each question in the database of questions, if not stop 
	if(any(is.na(my.data$Points[which.questions]))) {my.error<<-"You need to specify the number of points for each of the question included in the database" 
		stop("You need to specify the number of points for each of the question included in the database")}

  
   #ExcludeIf, the list of questions  was not included in the database of questions
   if(is.null(my.data$ExcludeIf)) 
	#my.data$ExcludeIf<-rep("", dim(my.data)[1])
   my.data$ExcludeIf<-NA
   
########modified 10/2/2010 to add notes to the file containing all the questions
   #Notes, notes added to the questions
   if(is.null(my.data$Notes)) 
	my.data$Notes<-rep("", dim(my.data)[1])
 #  my.data$Notes<-NA
 #############################################
   
   
   
    #how many questions are contained in the database
    how.many.questions<-length(which.questions)   

    #end of questions row-ID
    end.questions<-c(which.questions[-1]-1, dim(my.data)[1])

   
   
   
     
    #how many records are included in the database - different than number of questions, because every question can include sub-questions
    how.many.records<-dim(my.data)[1]

   
   
   
    #initializing a vector for saving the names of the files, exams and solutions
	if(repeat.each.test>1) my.filenames<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames<-vector("character", num.tests)
   
   
   
   
   
   if(generate.solutions) {
		if(repeat.each.test>1) my.filenames.sol<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames.sol<-vector("character", num.tests)
    }
   

	
	#beginning of method
   for(tt in 1:num.tests){
   
    cat("Generating exam number ", tt, "\n")
   
   		
			index.questions.vector<-Remember.questions.index[[tt]]		
                                                             
   
       
    #get the lines where the questions end, dealing with the exception for the last question
    #end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) #which(my.data$Question.ID==my.data[index.questions.vector[i],]$Question.ID+1)-1)
   
    #permute the order of the questions, given as the row in which the header of the question appears in the database
    #modified 08/04/2009
 
   end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) {
        #cat(i, "\n")
        tmp<-0;
        my.done<-FALSE;
       # for(k in index.questions.vector[i]:dim(my.data)[1]){ modified 20/7/2009 when the "end question" was removed
	  #  for(k in index.questions.vector[i]:(   max(index.questions.vector[i], dim(my.data)[1]-1)    )){ 

  if(index.questions.vector[i]==dim(my.data)[1])    return(index.questions.vector[i]) else {
  
	    for(k in index.questions.vector[i]:(   dim(my.data)[1]-1    )){ 
           # if(is.na(my.data$Question.ID[k+1]) | my.data$Question.ID[k+1]!="") {tmp<-tmp+1} else break;
           if(is.na(my.data$Question.ID[k+1]) ) {tmp<-tmp+1} else break;
        }#end for
        
       return(index.questions.vector[i]+tmp)}
        
    })) #end of definition of end.index.questions.vector
 
 
 
  

   
        Remember.questions.index.end[[tt]]<-end.index.questions.vector
       
        #number of questions selected
        num.questions<-length(index.questions.vector)
   
   
   

    ##############################################
    # beginning of writing of the file
    #############################################
   
		#generating the name of the file with the exams
		if(use.Sweave)     zz<-paste(my.outdir, "\\", my.prefix, tt, ".rnw", sep="") else     zz<-paste(my.outdir, "\\", my.prefix, tt, ".tex", sep="")
		#saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
		#tests 
		my.filenames[tt]<-zz
		

		#if also the solutions need to be generated
		if(generate.solutions)  {

			#check if there was no Answer column in the database of questions
			if(is.null(my.data$Answer)) {my.data$Answer<-NA
									warning("You did not include a column with the answers in the database of questions - it must be called: Answer")	
										}
			#generate the name of the file with the answers
			if(use.Sweave) zz.sol<-paste(my.outdir, "\\",  my.prefix,  tt, "sol",  ".rnw", sep="") else zz.sol<-paste(my.outdir, "\\",  my.prefix,  tt, "sol",  ".tex", sep="")
		
			#saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
			#solutions 
			my.filenames.sol[tt]<-zz.sol
		
		}#end if generate.solutions
		
		
	    #my.test.id<-tt

	    my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}", head.prefix,     round(runif(1)*100), tt, " }", sep="")


	 
		    sink(zz)
			#    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 "\\usepackage[slovene]{babel}", 		"\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
		   
   
    #preamble of the latex file

    #different header if Sweave is used
    if(use.Sweave){
            cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n") 
		if(generate.solutions)             cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) 
					}			else     {cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")

	if(generate.solutions) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)
				
   }#end else if use.Sweave
   
    cat(my.header)
	if(generate.solutions)    {cat(my.header, file=zz.sol, append=TRUE)}

    #beginning of the questions
cat("\\begin{itemize}\n")
	if(generate.solutions) cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)

#save the random seed at the beginning of writing of the file
my.old.seed<-.Random.seed

for(i in 1:length(index.questions.vector)){
    cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
	if(generate.solutions) {cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE)
	#if(my.data$Answer[index.questions.vector[i]]!="" |  !is.na(my.data$Answer[index.questions.vector[i]])) 
	if(!is.na(my.data$Answer[index.questions.vector[i]])) 
		cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
	}
   
    if(!is.na(my.data$ExtraLines[index.questions.vector[i]])) {
		cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")))
		if(generate.solutions) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE)}
    if(end.index.questions.vector[i]!=index.questions.vector[i]){
        cat("\\begin{enumerate}[(a)]\n") 
		if(generate.solutions) cat("\\begin{enumerate}[(a)]\n", file=zz.sol, append=TRUE) 
        if(my.data[index.questions.vector[i],]$PermuteOK==TRUE)
        perm.order<-resample(c((index.questions.vector[i]+1): end.index.questions.vector[i])) else perm.order<-c((index.questions.vector[i]+1): end.index.questions.vector[i])

        for(j in perm.order){
            cat("\\item ") 
            cat(as.character(my.data$Question[j]), "\n")
			if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[j], sep="")))
			if(generate.solutions) {
				cat("\\item ", file=zz.sol, append=TRUE) 
				cat(as.character(my.data$Question[j]), "\n", file=zz.sol, append=TRUE)
				if(!is.na(my.data$Answer[j])) 
		cat("{\\bf", as.character(my.data$Answer[j]), "}",  file=zz.sol, append=TRUE)
				if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", (my.data$ExtraLines[j]-1), sep="")), file=zz.sol, append=TRUE)
			}#end if generate.solutions
		}#end for j
    cat("\\end{enumerate}\n")
	if(generate.solutions) cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)}#end if beginning and end of question differ
}#end for i

    cat("\\end{itemize}\n")
	#added Feb2012, writes a final sentence in each test
	cat("{\\bf", as.character(my.final.sentence), "}")

    cat("\\newpage\n")
    cat("\\end{document}")
	if(generate.solutions) {
	    cat("\\end{itemize}\n", file=zz.sol, append=TRUE)
		cat("\\newpage\n", file=zz.sol, append=TRUE)
		cat("\\end{document}", file=zz.sol, append=TRUE)
	}
    sink()

   
    }#end tt
       
       
    ###########################################################
    #if permuted version of the same test need to be generated extra tests will be written
    #they will be indicated with permutation index(pp)+test index(tt)
    ############################################################
   
   
    if(repeat.each.test>1){
        for(tt in 1:num.tests){
            #retrieving the question IDs for the exam tt
            index.questions.vector<-Remember.questions.index[[tt]]
            end.index.questions.vector<-Remember.questions.index.end[[tt]]
            #number of questions selected
            num.questions<-length(index.questions.vector)
               
            for(pp in 1:repeat.each.test){
                my.sample<-resample(1:num.questions)
                #permuting the questions from exam tt, index for beginning and end of the questions   
                index.questions.vector<-index.questions.vector[my.sample]
                end.index.questions.vector<-end.index.questions.vector[my.sample]   
               
                #############################################################
                # beginning of writing of the file with the questions #######
                ##############################################################
                if(use.Sweave) zz<-paste(my.outdir, "\\", my.prefix, pp, tt, ".rnw", sep="") else zz<-paste(my.outdir, "\\", my.prefix, pp, tt, ".tex", sep="")

				#saving the name of the file
				my.filenames[num.tests+repeat.each.test*(tt-1)+pp]<-zz
				
				#if also the solutions need to be generated
				if(generate.solutions) {
	
					#generate the name of the file with the answers
					if(use.Sweave) zz.sol<-paste(my.outdir, "\\",  my.prefix,  pp, tt, "sol",  ".rnw", sep="") else zz.sol<-paste(my.outdir, "\\",  my.prefix,  pp, tt, "sol",  ".tex", sep="")
	
					#saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
					#solutions 
					my.filenames.sol[num.tests+repeat.each.test*(tt-1)+pp]<-zz.sol
		
	
				}#end if generate.solutions

				
				
				
                #my.test.id<-paste(pp, tt, sep="")

                my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}",
                head.prefix, round(runif(1)*100), pp, tt, " }", sep="")

                sink(zz)
#    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 "\\usepackage[slovene]{babel}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
   
   
                #preamble of the latex file
                if(use.Sweave) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n") else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}",   "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")

				if(generate.solutions) {if(use.Sweave)  cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}",  "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}",  "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)}

				
                cat(my.header)
				 #beginning of the questions
                cat("\\begin{itemize}\n")
				if(generate.solutions) {cat(my.header, file=zz.sol, append=TRUE)
										cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)}
										
                for(i in 1:num.questions){
                    cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }",
                    as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
					if(generate.solutions){                    
							cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }",
                    as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE)
					if(!is.na(my.data$Answer[index.questions.vector[i]])) 
							cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
					}
 	
                if(!is.na(my.data$ExtraLines[index.questions.vector[i]])){
                    cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")))
					if(generate.solutions) 
						cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE)
					}
					
                if(end.index.questions.vector[i]!=index.questions.vector[i]){
                    cat("\\begin{enumerate}[(a)]\n")
                    if(generate.solutions) cat("\\begin{enumerate}[(a)]\n", file=zz.sol, append=TRUE)
					if(my.data[index.questions.vector[i],]$PermuteOK==TRUE)
                    perm.order<-resample(c((index.questions.vector[i]+1): end.index.questions.vector[i])) else perm.order<-c((index.questions.vector[i]+1): end.index.questions.vector[i])

					
					for(j in perm.order){
			            cat("\\item ") 
			            cat(as.character(my.data$Question[j]), "\n")
						if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[j], sep="")))
						if(generate.solutions) {
							cat("\\item ", file=zz.sol, append=TRUE) 
							cat(as.character(my.data$Question[j]), "\n", file=zz.sol, append=TRUE)
							if(!is.na(my.data$Answer[j])) 
								cat("{\\bf", as.character(my.data$Answer[j]), "}",  file=zz.sol, append=TRUE)
							if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", (my.data$ExtraLines[j]-1), sep="")), file=zz.sol, append=TRUE)
						}#end if generate.solutions
					}#end for j
					cat("\\end{enumerate}\n")
					if(generate.solutions) cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)}
	}#end for i

    cat("\\end{itemize}\n")
	#added Feb2012, writes a final sentence in each test
	cat("{\\bf", as.character(my.final.sentence), "}")

    cat("\\newpage\n")
    cat("\\end{document}")
	if(generate.solutions) {
	    cat("\\end{itemize}\n", file=zz.sol, append=TRUE)
		cat("\\newpage\n", file=zz.sol, append=TRUE)
		cat("\\end{document}", file=zz.sol, append=TRUE)
	}
    sink()

	}#end pp

	}#end tt
}#end if repetitions
   
   

		#cat(my.filenames)
		#cat(my.filenames.sol)

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
		for(i in 1:length(my.filenames)){
			set.seed(i+my.seed)
			#shell(Sweave(my.filenames[i]))
			Sweave(my.filenames[i])
			if(generate.solutions){
				set.seed(i+my.seed)
				#shell(Sweave(my.filenames.sol[i]))
				Sweave(my.filenames.sol[i])
				}			
		}
		
		setwd(my.oldwd)	
    	}# end if use.Sweave
	    
	################################
	#compiling tex files to pdf
	################################

	if(generate.solutions) my.files<-c(my.filenames, my.filenames.sol) else my.files<-my.filenames

	
	#if compile.pdf=TRUE it generates the pdf files from the tex files
	if(compile.pdf){
		my.oldwd<-getwd()
		setwd(my.outdir)
		#use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
		#if(generate.solutions) my.files<-c(my.filenames, my.filenames.sol) else my.files<-my.filenames

		for(i in 1:length(my.files)){
			my.file<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")	
			my.file<-unlist(strsplit(my.file,  "\\\\"))
			my.file<-my.file[length(my.file)]
		
      #changed July 2013, uses texi2dvi instead of pdflatex
      #my.command<-paste("pdflatex", my.file)
			##	system(my.command) modified 15/2/2010 to take check for possible errors in pdflatex function, and stop the function in case of errors
      #out.pdflatex<-try(system(my.command) )
			#if(out.pdflatex!=0)         {
      
      
			out.pdflatex=try(texi2dvi(my.file, pdf=TRUE, clean=TRUE, quiet=TRUE))
			
			#if there was an error - returns a code different than 0
			if(!is.null(out.pdflatex))         {
			  
      
      
      
       #return to the original directory
       setwd(my.oldwd)
			my.error<<-"There was an error in compiling LaTeX in PDF files with texi2dvi - more details are displayed in the R console"
									stop("There was an error compiling the LaTeX file(s)")
			
			}# end out.pdflatex, error in pdf compilation
			
			
		} #end for i
     setwd(my.oldwd)	
	 }#end if compile.pdf


	#saving the names of the final output files, tex or pdf, depending on the selected options
	for(i in 1:length(my.files)){
		if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
	}	

	#outputting questions ID instead of row number
	Remember.questions.index<-lapply(Remember.questions.index, function(x) my.data$Question.ID[x])
	
    names(Remember.questions.index)<-paste("Test", 1:num.tests)   

	
	return(list(Questions=Remember.questions.index, files=my.files))       
}#end function genertestPreSelect()   
       
