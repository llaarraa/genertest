`genertest` <-
function(my.data, my.outdir, num.tests=1, repeat.each.test=1, my.seed=1999, topics=NULL, topics.points=NULL, tot.points=NULL, topics.points.difficulty=NULL, min.distance=3, my.include=NA, generate.solutions=FALSE, my.title="", my.date="", my.prefix="exam",     head.name="Name", head.id="ID number", head.points="Number of points", head.prefix="MED", my.language="english", use.Sweave=FALSE, compile.pdf=FALSE  
    ){
   
    ##################################################
    ##############FUNCTION ARGUMENTS##################
    ##################################################
   
    #my.data: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be given, should be a string, written between quotes
    #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be created by the user before the function is called
	#num.tests: number of different tests to be generated
    #repeat.each.test: number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation
    #my.seed: seed used to inizialize the random number generator, useful to get reproducibile results
    #topics: vector listing which topics should be included, these strings have to match exactly
    #topics.points: vector listing the number of points for each topic
    #tot.points: total number of points to be included in the exams. will be used only if topics was not specified, otherwise it will be ignored and the total number of points will be calculated using the sum of points specified in topics.points or topics.points.difficulty
    #topics.points.difficulties: matrix with number of rows equal to the number of topics and number of columns equal to the number of different difficulties that one wishes to include. If the difficulties in the question database were coded 1 to 4, the number in the first column and first row will define how many points from the first topic with difficulty 1 should be included, the number of second column and first row the points from topic 1 difficulty 2, etc... At most four different columns can be specified in this case. If you do not wish to select any questions from a specific combination of topic and difficulty write 0 in the corresponding cell.
    #min.distance: minimum distance between two consecutive tests that contain the same questions,     #0= no constrain, 1=at least 1 test in between, etc...
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
    Remember.questions.index<-Remember.questions.index.end<-vector("list", num.tests)
    #list in which the questions to exclude due to minimum distance to keep between tests
    Index.questions.4exclude<-vector("list", min.distance)
   
   
    ####################################
    #######selection of the questions###
    ######### and check of the database#
	####################################
    
	   
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
  
	#number of blank lines to be left after each question and sub-question was not specified, no extra-space will be left
	if(is.null(my.data$ExtraLines)) my.data$ExtraLines<-rep(NA, dim(my.data)[1])
  
  
	#check if there the Points were specified for each question in the database of questions, if not stop 
	if(any(is.na(my.data$Points[which.questions]))) 
		stop("You need to specify the number of points for each of the question included in the database")

  
   #ExcludeIf, the list of questions  was not included in the database of questions
   if(is.null(my.data$ExcludeIf)) 
	my.data$ExcludeIf<-rep("", dim(my.data)[1])
   
   
   
    #how many questions are contained in the database
    how.many.questions<-length(which.questions)   

    #end of questions row-ID
    end.questions<-c(which.questions[-1]-1, dim(my.data)[1])

    #If it was indicated that a column in the database indicates the questions to be excluded
    if(!is.na(my.include)){   
       
        #exclude the questions that are indicated to be excluded by my.include variable
        #also the rows that are related to those questions (subquestions)
        if(any(my.data[,my.include]==FALSE, na.rm=TRUE)){
            which.exclude<-!my.data[which.questions,my.include]
            which.rows.exclude<-numeric(0)
            for(i in c(1:how.many.questions)[which.exclude]){
                #cat(i, "\n")
                which.rows.exclude<-c(which.rows.exclude, c(which.questions[i]:end.questions[i]))
            }
       
            #exclude the questions that were indicated as FALSE in the column my.include
            my.data<-my.data[-which.rows.exclude,]
        }
       
        #recalculate the quantities of interest for the filtered data set
        #indicator of which lines in the database contain the beginning of a question - and the info about the
        #row-ID for the beginning of questions
        which.questions<-which(!is.na(my.data$Question.ID))
       
        #how many questions are contained in the database
        how.many.questions<-length(which.questions)   

        #end of questions row-ID
        end.questions<-c(which.questions[-1]-1, dim(my.data)[1])
    }   
   
    #how many records are included in the database - different than number of questions, because every question can include sub-questions
    how.many.records<-dim(my.data)[1]

    #dealing with the questions to be excluded in case a particular question in included in a test
    #getting the question IDs to exclude for each of the questions
    #taking care of the fact that ExcludeIF might be a factor (if at least one question has more than an exclusion criteria) or a numeric
	exclude.list<-lapply(1:dim(my.data)[1], function(i) {
                if(my.data$ExcludeIf[i]!="" & !is.na(my.data$ExcludeIf[i]) ) as.numeric(unlist(strsplit(as.character(my.data$ExcludeIf[i]), ",")))
                })
    #converting to row-IDs
    #a list for each row of the question-matrix is given, the rows to exclude (beginning of questions) are contained in the list
    exclude.list<- lapply(exclude.list, function(l)
            if(!is.null(l)) which(is.element(my.data$Question.ID, unlist(l))))

   
    #sample which questions will be included in the test
    #tot.points=100, min.dist=0,
   
   
	#checking if the column Topic was included in the database of questions
	if(is.null(my.data$Topic)) my.data$Topic<-rep("", dim(my.data)[1])
	
    #defining data.Topic, which replaces my.data$Topic, so that different difficulties can be dealt with
    data.Topic<-rep(NA, how.many.records)
	
	data.Topic[my.data$Topic!=""]<-as.character(my.data$Topic[my.data$Topic!=""])
   
    #check if the different difficulties were speficied
    #in this case a new set of topics will be defined, in which a suffix indicates the difficulty
    if(!is.null(topics.points.difficulty)){
           
            #if(tt=1){
            #checking if the difficulties were not specified in the original question database
			#completely missing
			if(is.null(my.data$Difficulty))     stop("You need to specify the difficulty for each question in the question database (column Difficulty) if you wish to select questions depending on their difficulty (please leave topics.points.difficult blank if you do not wish to use this option)")
			
		   #not specified for each question
		   if(any(is.na(my.data$Difficulty[which.questions])))
                    #checking if the difficulties were specified in the question database
                    stop("You need to specify the difficulty for each question in the question database (column Difficulty) if you wish to select questions depending on their difficulty (please leave topics.points.difficult blank if you do not wish to use this option)")
                   
            #checking if the object topics.points.difficulty was correctly specified, should be a matrix with number of rows equal to the numver of topics and number of columns equal at most to the number of different difficulties specified in the database
                if(dim(topics.points.difficulty)[1]!=length(topics) | dim(topics.points.difficulty)[2]>unique(length(my.data$Difficulty[which.questions])))
                    stop("You did not specify correctly the object that defines the number of points from each difficulty to be included in the exam (topics.points.difficulty) or you did not specify the topics that you whish to include (topics). topics.points.difficulty should be a matrix with the same number of rows as the number of topics that you wish to include in the exam and number of columns smaller or equal to the number of different difficulties included in the question database.")   
   
                #}
           
            new.topics<-paste(rep(topics, each=ncol(topics.points.difficulty)), 1:ncol(topics.points.difficulty), sep="")
            new.points<-as.numeric(t(topics.points.difficulty))
            #eliminate
            which.keep<-which(new.points!=0)
            #resetting topics and points
            topics<-new.topics[which.keep]
            topics.points<-new.points[which.keep]
           
            data.Topic[which.questions]<-as.character(paste(my.data$Topic, my.data$Difficulty, sep=""))[which.questions]
    }#end if difficulties
   
   
   
    #final vector containing the topics -indexed by difficulty if specified so
    data.Topic<-factor(data.Topic)
   
    #vector containing the questions selected in the min.distance previous tests
    Index.questions<-vector("list", min.distance)
   
    #initializing a vector for saving the names of the files, exams and solutions
	if(repeat.each.test>1) my.filenames<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames<-vector("character", num.tests)
   
   
   
   
   
   if(generate.solutions) {
		if(repeat.each.test>1) my.filenames.sol<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames.sol<-vector("character", num.tests)
    }
   

	
	#beginning of method
   for(tt in 1:num.tests){
   
    cat("Generating exam number ", tt, "\n")
   
    #check if the topics to be included were specified and select the questions
    if(!is.null(topics)) {
            #checks if the specified topics correspond to those included in the database
            #could be improved for partial matching, here only exact matching works fine
            if(any(!is.element(topics, data.Topic[which.questions])))
                    stop("Some or all the topics that you specified are not included in your database")   
   
            #checks if the percentages for each topic were (correctly) specified
            #the last condition could be removed, if it is not necessary that the sum is 100
            #if(is.null(topics.points) | length(topics.points)!=length(topics) | sum(topics.points)!=100), removed the requirement that the sum is 100
            if(is.null(topics.points) | length(topics.points)!=length(topics))
                    stop("Specify the number of points that you want to include for each of the selected topics")
                       
        #list that will contain the row-ID of the selected questions for each topic
        index.questions<-vector("list", length(topics))
           
        for(i in 1:length(topics)){
               
                my.done<-FALSE
                sum.points<-0
                num.errors<-0
               
                #select only the questions that have the selected topics and a number of points that is less than the selected limit
                which.questions.per.topic<-which(!is.na(my.data$Question.ID) & data.Topic==topics[i] & my.data$Points<=topics.points[i]
                #exclude also the questions listed in the excludeIf list for the questions that were already selected
                & (!is.element(c(1:how.many.records), unlist(exclude.list[unlist(index.questions[1:i])])))
                & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))
                )   
                #check if there are any questions that can be used for this topic
                if(length(which.questions.per.topic)==0 | sum(my.data$Points[which.questions.per.topic])<topics.points[i] | min(my.data$Points[which.questions.per.topic])>topics.points[i]){
                            cat("A problem was encountered for topic: ", topics[i], "\n")
                            stop("There are not enough questions for this topic to meet the requirements on the number of points that were         specified")
                                }
               
                while(my.done==FALSE){
                    #if an error is found reset - up to 100 times -> to give a second chance to situations in which an incompatible value is selected early and this selection cannot be corrected, an algorithm that finds the optimal combination of questions was not implemented
                    if(length(which.questions.per.topic)==0 & num.errors>100) {
                            cat("A problem was encountered for topic: ", topics[i], "\n")
                            stop("There are not enough questions for this topic to meet the requirements on the number of points that were         specified.")
                            }
					
					
					if(length(which.questions.per.topic)==0 & num.errors<=100) {
                        sum.points<-0
                        num.errors<-num.errors+1
                        index.questions[i]<-vector("list", 1)
                        #break
						#reset the list of possible questions
						which.questions.per.topic<-which(!is.na(my.data$Question.ID) & data.Topic==topics[i] & my.data$Points<=topics.points[i]
						#exclude also the questions listed in the excludeIf list for the questions that were already selected
						& (!is.element(c(1:how.many.records), unlist(exclude.list[unlist(index.questions[1:i])])))
						& (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))
						)
                        
						}#end if
                               
                    
                   
                    my.index<-resample(which.questions.per.topic, 1)
                    #cat(i, my.index, sum.points, "\n")
                    #if(sum.points+my.data$Points[which.questions.per.topic]<=topics.points[i]){
                    sum.points<-sum.points+my.data$Points[my.index]
                    index.questions[[i]]<-c(index.questions[[i]], my.index)
                                   
                    which.questions.per.topic <-which(!is.na(my.data$Question.ID)& data.Topic==topics[i]   
                    & my.data$Points<=(topics.points[i]-sum.points) 
                & (!is.element(c(1:how.many.records), unlist(exclude.list[unlist(index.questions[1:i])])))
                &    !is.element(1:dim(my.data)[1], unlist(index.questions[1:i]))
                & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))
                )   
                           
               
               
                                   
                    #}#end if
                    if(sum.points==topics.points[i]) my.done<-TRUE
                }#end while
               
        }
    }#if topics not null       


    #no topics were specified, proceed using all the questions
    else if(is.null(topics)) {
            if(is.null(tot.points)) stop("You need to specify the total number of points to include in the exam (using tot.points variable)")
                                   
        #list that will contain the row-ID of the selected questions
        index.questions<-numeric(0)
           
        #for(i in 1:length(topics)){
               
                my.done<-FALSE
                sum.points<-0
                num.errors<-0
               
                #select only the questions that have the selected topics and a number of points that is less than the selected limit
                which.questions.per.topic<-which(!is.na(my.data$Question.ID) & my.data$Points<=tot.points
                #exclude also the questions listed in the excludeIf list for the questions that were already selected
                & (!is.element(c(1:how.many.records), unlist(exclude.list[index.questions])))
                & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))
                )   
                #check if there are any questions that can be used for this topic
                if(length(which.questions.per.topic)==0 | sum(my.data$Points[which.questions.per.topic])<tot.points | min(my.data$Points[which.questions.per.topic])>tot.points){
                            stop("There are not enough questions in your questions database to select enough questions using the criteria that were specified. Check the consistency of the required total number of points with the points for each question included in the database.")
                                }
               
                while(my.done==FALSE){
                    #if an error is found reset - up to 100 times -> to give a second chance to situations in which an incompatible value is selected early and this selection cannot be corrected, an algorithm that finds the optimal combination of questions was not implemented
                    if(length(which.questions.per.topic)==0 & num.errors<=100) {
                        num.errors<-num.errors+1;
                        sum.points<-0
                        num.errors<-num.errors+1
                        index.questions<-numeric(0)
                        break
                        }
                               
                    if(length(which.questions.per.topic)==0 & num.errors>100) {
                            stop("There are not enough questions in your questions database to select enough questions using the criteria that were specified.")
                            }
                   
                    my.index<-resample(which.questions.per.topic, 1)
                    #cat(i, my.index, sum.points, "\n")
                    #if(sum.points+my.data$Points[which.questions.per.topic]<=topics.points[i]){
                    sum.points<-sum.points+my.data$Points[my.index]
                    index.questions<-c(index.questions, my.index)
                                   
                    which.questions.per.topic <-which(!is.na(my.data$Question.ID)& my.data$Points<=(tot.points-sum.points) 
                        & (!is.element(c(1:how.many.records), unlist(exclude.list[index.questions])))
                        & !is.element(1:dim(my.data)[1], index.questions)
                        & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))
                    )   
                           
                    if(sum.points==tot.points) my.done<-TRUE
                }#end while
               
        }#if topics were null   


    ##########end of selection of the questions for a given exam##################
       
    #permute the order of the questions, given as the row in which the header of the question appears in the database
    index.questions.vector<-resample(unlist(index.questions))
    Remember.questions.index[[tt]]<-index.questions.vector
   
    if(min.distance!=0){
                        Index.questions.4exclude[-1]<-Index.questions.4exclude[-min.distance]
                        Index.questions.4exclude[[1]]<-index.questions.vector}
       
   
       
    #get the lines where the questions end, dealing with the exception for the last question
    #end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) #which(my.data$Question.ID==my.data[index.questions.vector[i],]$Question.ID+1)-1)
   
   
   
    end.index.questions.vector <- unlist(lapply(1:length(index.questions.vector), function(i) {
        #cat(i, "\n")
        tmp<-0;
        my.done<-FALSE;
        for(k in index.questions.vector[i]:dim(my.data)[1]){
            if(is.na(my.data[k+1,2])) {tmp<-tmp+1} else break;
           
        }
        index.questions.vector[i]+tmp}
        ))
   
   
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
			if(is.null(my.data$Answer)) {my.data$Answer<-rep("", dim(my.data)[1])
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

for(i in 1:length(index.questions.vector)){
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
	if(generate.solutions) cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)}#end if beginning and end of question differ
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
                if(use.Sweave) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n") else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}",   "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")

				if(generate.solutions) {if(use.Sweave)  cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}",  "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)}

				
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
					if(my.data$Answer[index.questions.vector[i]]!="" |  !is.na(my.data$Answer[index.questions.vector[i]])) 
							cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
					}
 	
                if(!is.na(my.data$ExtraLines[index.questions.vector[i]])){
                    cat((rep("\\newline", my.data$ExtraLines[index.questions.vector[i]], sep="")))
					if(generate.solutions) 
						cat((rep("\\newline", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE)
					}
					
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
					if(generate.solutions) cat("\\end{enumerate}\n", file=zz.sol, append=TRUE)}
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
			shell(Sweave(my.filenames[i]))
			if(generate.solutions){
				set.seed(i+my.seed)
				shell(Sweave(my.filenames.sol[i]))
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
			my.command<-paste("pdflatex", my.file)
			system(my.command)
			
		}
     setwd(my.oldwd)	
	 }#end if compile.pdf


	#saving the names of the final output files, tex or pdf, depending on the selected options
	for(i in 1:length(my.files)){
		if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
	}	

	
    names(Remember.questions.index)<-paste("Test", 1:num.tests)   
	#close the open connections, if any - can be problematic if errors occur and the function is stopped
	on.exit(if(sink.number()!=0) for(i in 1:sink.number()) sink())
	return(list(Questions=Remember.questions.index, files=my.files))       
}#end function genertest()   

