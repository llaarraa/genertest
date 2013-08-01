

#########################################################################


##########################write.all####################################


write.all<-function(my.db.name, my.outdir=NULL, my.seed=1999, #my.include=NA,
                    generate.solutions=FALSE, my.title="", my.date="", my.prefix="exam",     head.name="Name", head.id="ID number", 
                    head.points="Number of points", head.prefix="MED", my.language="english", use.Sweave=TRUE, compile.pdf=TRUE, files.to.move=NULL
                    ){
  
  ##################################################
  ##############FUNCTION ARGUMENTS##################
  ##################################################
  
  #my.data: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be given, should be a string, written between quotes
  #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be created by the user before the function is called
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
  #compile.pdf: logical, if set to true, pdf files will be generated, otherwise only tex files. Uses texi2dvi to compile the files. 
  #my.final.sentence: a string with a sentence to be displayed in bold at the end of the file
  
  ######################
  # supporting functions
  #######################
  
  
  my.error<-"No errors were found" 	
  
  resample <- function(x, size, ...)
    if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
  
  ######################
  # end of supporting functions
  #######################
  
  

  #save the initial working directory
  my.oldwd<-getwd()
  #add on.exit: RETURNS TO original dirctory if an error occurs  		
  on.exit(setwd(my.oldwd)	, add=T)
  
  
  
  
  
  
  #checking if the directory specified to store the results exists
  if(is.null(my.outdir)) {#my.outdir=paste("Exams", date())
    #define the names of the directory where the files will be stored, creates a subdirectory of the working directoty 
    
    my.oldwd<-getwd()
    my.outdir=file.path(my.oldwd, paste("AllQuestions",format(Sys.Date(), "%b%d%y"), format(Sys.time(), "%H%M%S"), sep=""))
    
    my.command=paste("mkdir", my.outdir)
    system(my.command)
  } else {
    if(is.na(file.info(my.outdir)$isdir)) {my.error<-"The directory that you choose to store the results does not exist, please specify an existing directory or leave the my.outdir argument empty."
                                           stop("The directory that you choose to store the results does not exist. Please specify an existing directory of leave the my.outdir argument empty. A directory named Exams + the current date and time will be created in your working directory.")}
  }
  
  
  #set my.outdir as a character
  my.outdir<-as.character(my.outdir)
  
  

  #read data
  #July 2013: can be read also from a data.frame
  
  if(class(my.db.name)!="character" & class(my.db.name)!="data.frame") {
    my.error<-"my.db.name should be a string with the path to a tab delimited file or an R data.frame"
    stop("my.db.name should be a string with the path to a tab delimited file or an R data.frame")
    
  }
  
  
  
  if(class(my.db.name)=="character"){
              my.data<-try(read.delim(as.character(my.db.name), sep="\t", blank.lines.skip=TRUE))
              if(class(my.data)=="try-error") {my.error<-"I cannot open the file with questions"
                                   stop("I cannot open the file with questions")}} else  my.data=my.db.name 
  
  
  #my.data<-read.delim(as.character(my.data), sep="\t", blank.lines.skip=TRUE)
  #treat the empty cells as missing!
  my.data[my.data==""]<-NA
  
  #remove lines where all the entries are missing
  my.data<-my.data[!(apply(my.data, 1, function(x) all(is.na(x)))),]
  
  
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
  if(is.null(my.data$Question.ID)) {my.error<-"You need to include a column called Question.ID is the database of questions"
                                    stop("You need to include a column called Question.ID is the database of questions")}
  
  
  #indicator of which lines in the database contain the beginning of a question - and the info about the
  #row-ID
  which.questions<-which(!is.na(my.data$Question.ID))
  
  
  #check if there is a column called Points in the database of questions, if not stop 
  if(is.null(my.data$Points)) {my.error<-"You need to include a column called Points is the database of questions"
                               stop("You need to include a column called Points is the database of questions")}
  
  #check if there is a column called Points in the database of questions, if not stop 
  if(is.null(my.data$Question)) {my.error<-"You need to include a column called Question is the database of questions"
                                 stop("You need to include a column called Question is the database of questions")}
  
  
  #if there is no column called PermuteOK in the database of questions, add one, with indicator FALSE for all the questions (sub-questions will not be permuted)
  if(is.null(my.data$PermuteOK))  {my.data$PermuteOK<-NA 
                                  my.data$PermuteOK[which.questions]<-FALSE
                                  }
  
  #if permuteOK is missing for some of the questions set it to FALSE
  my.data$PermuteOK[which.questions][is.na(my.data$PermuteOK[which.questions])]<-FALSE
  
  
  
  #check if Topics were specified	
  if(is.null(my.data$Topic)) 
  {my.data$Topic<-rep("", dim(my.data)[1])}	
  #{my.data$Topic<-NA}
  
  
  #number of blank lines to be left after each question and sub-question was not specified, no extra-space will be left
  if(is.null(my.data$ExtraLines)) my.data$ExtraLines<-rep(NA, dim(my.data)[1])
  
  
  #check if there the Points were specified for each question in the database of questions, if not stop 
  if(any(is.na(my.data$Points[which.questions]))) {my.error<-"You need to specify the number of points for each of the question included in the database"
                                                   stop("You need to specify the number of points for each of the question included in the database")}
  
  
  #ExcludeIf, the list of questions  was not included in the database of questions
  if(is.null(my.data$ExcludeIf)) 
    #my.data$ExcludeIf<-rep("", dim(my.data)[1])
    my.data$ExcludeIf<-NA
  
  
  
  #if there is no column called Include in the database of questions, add one, with indicator TRUE for all the questions (all the questions will not included)
  if(is.null(my.data$Include)) 
  {my.data$Include<-NA 
   my.data$Include[which.questions]<-TRUE
  }
  
  
  #includes the questions for which the Include indicator is missing
  my.data$Include[which.questions][is.na(my.data$Include[which.questions])]<-TRUE
  
  
  #############selects all the questions that are not filtered out by my.include##################
  if(any(my.data$Include[which.questions]==FALSE)) 
    index.questions.vector<-which(!is.na(my.data$Question.ID) & my.data$Include) else 
      #NB: the ID in index.questions.vector refers to the row of the questions database where the question appears
      index.questions.vector<-which(!is.na(my.data$Question.ID))
  
  #how many questions are contained in the data base
  how.many.questions<-length(index.questions.vector)      
  
  
  #how many questions are contained in the data base
  how.many.questions<-length(index.questions.vector)   
  
  
  #close connections, if any is open
  on.exit(if(sink.number()!=0) for(i in 1:sink.number()) sink())
  
  
  
  
  
  #permute the order of the questions, given as the row in which the header of the question appears in the database
  #modified 08/04/2009
  Remember.questions.index<-index.questions.vector
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
    
  }))
  
  
  
  
  
  ##   end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) {
  ##       #cat(i, "\n")
  ##      tmp<-0;
  ##      my.done<-FALSE;
  ##     # for(k in index.questions.vector[i]:dim(my.data)[1]){ modified 20/7/2009 when the "end question" was removed
  ##   for(k in index.questions.vector[i]:(dim(my.data)[1]-1)){ 
  ##       # if(is.na(my.data$Question.ID[k+1]) | my.data$Question.ID[k+1]!="") {tmp<-tmp+1} else break;
  ##      if(is.na(my.data$Question.ID[k+1]) ) {tmp<-tmp+1} else break;
  ##  }
  ## index.questions.vector[i]+tmp}
  ##))
  
  Remember.questions.index.end<-end.index.questions.vector
  
  #number of questions selected
  num.questions<-length(index.questions.vector)
  
  
  
  
  
  ##############################################
  # beginning of writing of the file
  #############################################
  
  #generating the name of the file with the exams
  if(use.Sweave)     zz<-file.path(my.outdir,  paste(my.prefix, "All", ".rnw", sep="")) else     zz<-file.path(my.outdir, paste(my.prefix, "All", ".tex", sep=""))
  #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
  #tests 
  
  
  #if also the solutions need to be generated
  if(generate.solutions)  {
    
    #check if there was no Answer column in the database of questions
    if(is.null(my.data$Answer)) {my.data$Answer<-NA
                                 warning("You did not include a column with the answers in the database of questions - it must be called: Answer")	
    }
    
    #generate the name of the file with the answers
    if(use.Sweave) zz.sol<-file.path(my.outdir, paste(my.prefix,  "All", "sol",  ".rnw", sep="")) else zz.sol<-file.path(my.outdir, paste(my.prefix,  "All", "sol",  ".tex", sep=""))
    
    #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
    #solutions 
  }#end if generate.solutions
  
  
  my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}", head.prefix,     round(runif(1)*100),  " }", sep="")
  
  
  
  sink(zz)
  #    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 "\\usepackage[slovene]{babel}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
  
  
  #preamble of the latex file
  
  #different header if Sweave is used
  if(use.Sweave){
    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n") 
    if(generate.solutions)             cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) 
  }			else     {cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
                
                if(generate.solutions) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)
                
  }#end else if use.Sweave
  
  cat(my.header)
  if(generate.solutions)    {cat(my.header, file=zz.sol, append=TRUE)}
  
  #beginning of the questions
  cat("\\begin{itemize}\n")
  if(generate.solutions) cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)
  
  #save the random seed at the beginning of writing of the file
  my.old.seed<-.Random.seed
  
  
  #excluding the "end" question
  #for(i in 1:(length(index.questions.vector)-1)){
  
  for(i in 1:(length(index.questions.vector))){
    
    
    cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
    if(generate.solutions) {cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE)
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
      #modified 15/2/2010 , corrected a bug, \end{enumerate} was missing in the write.all solutions
      if(generate.solutions) cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)}#end if beginning and end of question differ
    
    
    
    #	cat("{\\bf Question ID: ", as.character(my.data$Question.ID[index.questions.vector[i]]), "\\bf Topic: ", as.character(my.data$Topic[index.questions.vector[i]]), ". Excluded when including question(s): ", my.data$ExcludeIf[index.questions.vector[i]], "}", "\n") 
    
    ########modified 10/2/2010, moved up, so does not include the "cat" below; corrected the bug that caused the question id and other info not to be written for questions with no sub-questions
    ###}#end if beginning and end of question differ, moved up
    
    #modified 8/4/2010, not written if there were sub-questions!  modified 10/2/2010 to add notes
    cat("{\\bf Question ID: ", as.character(my.data$Question.ID[index.questions.vector[i]]), "\\bf Topic: ", as.character(my.data$Topic[index.questions.vector[i]]), ". Excluded when including question(s): ", as.character(my.data$ExcludeIf[index.questions.vector[i]]), "Notes: ", ifelse(my.data$Notes[index.questions.vector[i]]!="", as.character(my.data$Notes[index.questions.vector[i]]), ""), "}", "\n") 
    
    
    #modified 08/04   , modified 10/2/2010 to add notes
    if(generate.solutions) {cat("{\\bf Question ID: ", as.character(my.data$Question.ID[index.questions.vector[i]]), "\\bf Topic: ", as.character(my.data$Topic[index.questions.vector[i]]), ". Excluded when including question(s): ", as.character(my.data$ExcludeIf[index.questions.vector[i]]), "Notes: ", ifelse(my.data$Notes[index.questions.vector[i]]!="", as.character(my.data$Notes[index.questions.vector[i]]), ""), "}", "\n", file=zz.sol, append=TRUE)} 	
    
    
    
  }#end for i
  
  cat("\\end{itemize}\n")
  #added Feb2012, writes a final sentence in each test
  ###cat("{\\bf", as.character(my.final.sentence), "}")
  
  cat("\\newpage\n")
  cat("\\end{document}")
  if(generate.solutions) {
    cat("\\end{itemize}\n", file=zz.sol, append=TRUE)
    cat("\\newpage\n", file=zz.sol, append=TRUE)
    cat("\\end{document}", file=zz.sol, append=TRUE)
  }
  sink()
  
  
  
  
  
  ################ moving the extra files needed to generate the exams ################
  
  if(!is.null(files.to.move)){
    #moves the files
    file.copy(files.to.move, my.outdir)
  }#end move the files
  
  
  
  
  
  
  
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
    ####shell(Sweave(zz))
    Sweave(zz)  #modified 24012011
    if(generate.solutions){
      set.seed(i+my.seed)
      #shell(Sweave(zz.sol))}
      Sweave(zz.sol)}
    
    
    
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
      #my.file<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")	
      #my.file<-unlist(strsplit(my.file,  "\\\\"))
      #my.file<-my.file[length(my.file)]
      #modified July 2013: using texi2dvi
      #my.command<-paste("pdflatex", my.file)
      ##system(my.command) modified 15/02/2010, checking for errors, 
      #out.pdflatex<-try(system(my.command))
      #if there was an error - returns a code different than 0
      #if(out.pdflatex!=0)         {
      
      #extract just the name of the file, without using the path and use the tex files only
      my.file=paste(unlist(strsplit(basename(my.files[i]), "\\."))[[1]], ".tex", sep="")
      
      
      
      
      #July 2013: using texi2dvi
      
      #updated July 2013, uses texi2dvi instead of pdflatex
      out.pdflatex=try(texi2dvi(my.file, pdf=TRUE, clean=TRUE, quiet=TRUE))
      if(!is.null(out.pdflatex))         {
      
        #return to the original directory
        setwd(my.oldwd)
        my.error<-"There was an error in compiling LaTeX in PDF files with pdflatex - more details are displayed in the R console"
        stop("There was an error compiling the LaTeX file(s)")
        
      }# end out.pdflatex, error in pdf compilation
      
    } #end for i
    setwd(my.oldwd)	
  } #end if compile.pdf
  
  #names(Remember.questions.index)<-paste("Test", 1:num.tests)   
  #close the open connections, if any - can be problematic if errors occur and the function is stopped
  
  #saving the names of the final output files, tex or pdf, depending on the selected options
  for(i in 1:length(my.files)){
    if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
  }	
  
  
  
  
  #outputting questions ID instead of row number
  Remember.questions.index<-unlist(lapply(Remember.questions.index, function(x) my.data$Question.ID[x]))
  
  return(results<-list(Questions=unlist(Remember.questions.index), files=my.files, errors=my.error))       
}#end function write.all()   



