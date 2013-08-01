#functions for genertest library: last updated: 08 Jul 2013
#removed the difficulty options, can be dealt directly with the topics: 08/07/2009
#removed the possibility of deciding what is the column with the include criteria
#modified 10/2/2010: fixed bug in write.all about questions with no-subquestions 
#added genertestPreSelect function, to be used when the user pre-specifies the questions to be included in each test
#modified Feb 2012: \\newline became \\\\vspace{\\baselineskip} to avoid the latex error "there is no line to end"
#  				added the possibility to write a final sentence in each test, argument my.final.sentece of genertest and genertest.PreSel
#modified Jul 2013: replaced  the compilation of the LaTeX files with pdflatex with texi2dvi 
            #my.command<-paste("pdflatex", my.file)
            #system(my.command)
            #updated: July 2013, replaces the system call with
            #texi2dvi(my.file, pdf=TRUE, clean=TRUE)
#           : the outdir can be left empty. If set to NULL a subdirectory named with the date/time of creation is created in the working directory and the results are stored there. 
#           : possibility to pass the database as an R object - not read from a tab delimited text


genertest<-function(my.db.name, my.outdir=NULL,  num.tests=1, repeat.each.test=1, my.seed=1999, 
                    topics=NULL, topics.points=NULL, tot.points=NULL, min.distance=0, generate.solutions="FALSE", 
                    my.title="Exam", my.date="Today", my.prefix="exam",     head.name="Name", head.id="ID number", 
                    head.points="Number of points", head.prefix="MED", my.language="english", use.Sweave=TRUE, 
                    compile.pdf=TRUE, merge.pdf=TRUE, my.final.sentence=NULL, files.to.move=NULL)
{
  
  ##################################################
  ##############FUNCTION ARGUMENTS##################
  ##################################################
  
  #my.db.name: name of the tab delimited file including the questions, either the full path should be given or the file should be placed in the R working directory or full file path should be    given, should be a string, written between quotes
  #my.outdir: name of the directory where the test will be written, should be a string, written between quotes. Full path of the directory must be given.  This directory has to exist or be    created by the user before the function is called
  #Remember.questions.index.QID: list containing the question IDs (as the appear in the database of questions) for each test, the length of the list corresponds to the number of different     tests that have to generated
  #num.tests: number of different tests to be generated
  #repeat.each.test: number of times that each test needs to be permuted, to generate permuted version of the same test, if set to 1 there will be no permutation
  #my.seed: seed used to inizialize the random number generator, useful to get reproducibile results
  #topics: vector listing which topics should be included, these strings have to match exactly
  #topics.points: vector listing the number of points for each topic
  #tot.points: total number of points to be included in the exams. will be used only if topics was not specified, otherwise it will be ignored and the total number of points will be       calculated using the sum of points specified in topics.points
  #min.distance: minimum distance between two consecutive tests that contain the same questions,     #0= no constrain, 1=at least 1 test in between, etc...
  
  #modified 15/2/2010: generate solutions. passed arguments can be: TRUE/FALSE/SHORT, 
  #generate.solutions: if set to TRUE also the solutions of the tests is generated, if set to SHORT the solutions are generated without repeating the questions (just the answers appear)
  
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
  #merge.pdf: logical, if set to true, the pdf files will be merged, valid only if compile.pdf=TRUE
  #my.final.sentence: a sentence, written in BOLD at the end of EACH test      
  #files.to.move: vector of strings indicating the (full path) name of the files that should be moved in the same directory as the exams, if not specified, all the files in dir.files.to.move are moved
  
  
  ######################
  # supporting functions
  #######################
  
  resample <- function(x, size, ...)
    if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
  
  my.error<-"No errors were found" 	
  
  
  ###################### ######  ####
  # end of supporting functions ####
  ####################### ###### ####
  
  
  ######################
  #read data   ########
  #####################
  
  #my.data=my.db.name
  
  
  #added July 2013, can use R data.frames as input
  
  if(class(my.db.name)!="character" & class(my.db.name)!="data.frame") {
    my.error<-"my.db.name should be a string with the path to a tab delimited file or an R data.frame"
    stop("my.db.name should be a string with the path to a tab delimited file or an R data.frame")
    
  }
  
  
  if(class(my.db.name)=="character"){
  
          #modified to read data from a character string
          my.data<-try(read.delim(as.character(my.db.name), sep="\t", blank.lines.skip=TRUE))
          if(class(my.data)=="try-error") {my.error<-"I cannot open the file containing the questions"
                                           stop("I cannot open the file containing the questions")} 
  
     } else my.data=my.db.name
  
  
  
  
  ###################################################
  # check and modify the database with questions where needed, prepare variables needed later 
  ###################################################
  
  #treat the empty cells as missing!
  my.data[my.data==""]<-NA
  #remove lines where all the entries are missing
  my.data<-my.data[!(apply(my.data, 1, function(x) all(is.na(x)))),]
  
   
  #checking if the directory specified to store the results exists
  if(is.null(my.outdir)) {#my.outdir=paste("Exams", date())
                          #define the names of the directory where the files will be stored, creates a subdirectory of the working directoty 
                          
                          my.oldwd<-getwd()
                          my.outdir=file.path(my.oldwd, paste("Exams",format(Sys.Date(), "%b%d%y"), format(Sys.time(), "%H%M%S"), sep=""))
                          
                          my.command=paste("mkdir", my.outdir)
                          system(my.command)
                        } else {
                          if(is.na(file.info(my.outdir)$isdir)) {my.error<-"The directory that you choose to store the results does not exist, please specify an existing directory or leave the my.outdir argument empty."
                          stop("The directory that you choose to store the results does not exist. Please specify an existing directory of leave the my.outdir argument empty. A directory named Exams + the current date and time will be created in your working directory.")}
                        }
  
  #set my.outdir as a character
  my.outdir<-as.character(my.outdir)
  
  
  #######added 18/10, additional controls on the paramteres passed by the user
  if(min.distance<0)   {my.error<-"The minimum distance cannot be negative"
                        stop("The directory that you choose to store the results does not exist")} 
  if(num.tests<0)   {my.error<-"The number of tests cannot be negative"
                     stop("The directory that you choose to store the results does not exist")} 
  ####################################################################################
  
  
  
  
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
  #######pre-selection of the questions###
  ######### and check of the database#
  ####################################
  
  
  
  #check if there is a variable called Question.ID in the database of questions, if not stop
  if(is.null(my.data$Question.ID)) {my.error<-"You need to include a column called Question.ID or Question ID in the database of questions" 
                                    stop("You need to include a column called Question.ID in the database of questions")}
  
  #indicator of which lines in the database contain the beginning of a question - and the info about the
  #row-ID of the database of questions (and not QUESTION.ID!), moved up 18/2/2010
  which.questions<-which(!is.na(my.data$Question.ID))
  
  #modified 18/2/2010: checking if all Questions.IDs are different
  if(sum(duplicated(my.data$Question.ID[which.questions]))>0) {
    #which.duplicated.QID<<-my.data$Question.ID[which.questions][which(duplicated(my.data$Question.ID[which.questions]))]
	which.duplicated.QID=my.data$Question.ID[which.questions][which(duplicated(my.data$Question.ID[which.questions]))]
    my.error<-as.character(c("Question.ID must be unique for each question. Questions that have repeated IDs are:", as.character(which.duplicated.QID))) 
    stop("Question.ID must be unique for each question")}
  
  ###-as.character(c("Some of the questions that you selected are not present in the database of questions (Question ID does not match)\n Missing questions are: ", as.character(unique.not.matched)))
  
  
  
  
  #check if there is a column called Points in the database of questions, if not stop 
  if(is.null(my.data$Points)) { my.error<-"You need to include a column called Points is the database of questions"
                                stop("You need to include a column called Points is the database of questions")} 
  
  #check if there is a column called Points in the database of questions, if not stop 
  if(is.null(my.data$Question)) {my.error<-"You need to include a column called Question is the database of questions"
                                 stop("You need to include a column called Question is the database of questions")}
  
  
  #if there is no column called PermuteOK in the database of questions, add one, with indicator FALSE for all the questions (sub-questions will not be permuted)
  if(is.null(my.data$PermuteOK)) 
  {my.data$PermuteOK<-NA 
   my.data$PermuteOK[which.questions]<-FALSE
  }
  
  #if permuteOK is missing for some of the questions set it to FALSE    - subquestions will not be permuted
  my.data$PermuteOK[which.questions][is.na(my.data$PermuteOK[which.questions])]<-FALSE
  
  
  #number of blank lines to be left after each question and sub-question was not specified, no extra-space will be left
  if(is.null(my.data$ExtraLines)) my.data$ExtraLines<-rep(NA, dim(my.data)[1])
  
  
  #if there is no column called Include in the database of questions, add one, with indicator TRUE for all the questions (all the questions will be included if the user did not specify an Include column)
  if(is.null(my.data$Include)) 
  {my.data$Include<-NA 
   my.data$Include[which.questions]<-TRUE
  }
  
  
  #includes the questions for which the Include indicator is missing
  my.data$Include[which.questions][is.na(my.data$Include[which.questions])]<-TRUE
  
  
  #check if the Points were specified for each question in the database of questions, if not stop 
  if(any(is.na(my.data$Points[which.questions]))) {my.error<-"You need to specify the number of points for each of the question included in the database" 
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
  
  #If it was indicated that some questions in the database should be excluded 
  if(any(my.data$Include[which.questions]==FALSE)){   
    
    #exclude the questions that are indicated to be excluded by my.include variable
    #also the rows that are related to those questions (subquestions)
    
    #if(any(my.data[,my.include]==FALSE, na.rm=TRUE)){
    
    which.exclude<-!my.data$Include[which.questions]
    which.rows.exclude<-numeric(0)
    for(i in c(1:how.many.questions)[which.exclude]){
      #cat(i, "\n")
      which.rows.exclude<-c(which.rows.exclude, c(which.questions[i]:end.questions[i]))
    }
    
    #exclude the questions that were indicated as FALSE in the column my.include
    my.data<-my.data[-which.rows.exclude,]
    #}
    
    #recalculate the quantities of interest for the filtered data set
    #indicator of which lines in the database contain the beginning of a question - and the info about the
    #row-ID for the beginning of questions
    
    which.questions<-which(!is.na(my.data$Question.ID))
    
    #how many questions are contained in the database
    how.many.questions<-length(which.questions)   
    
    #end of questions row-ID
    #end.questions<-c(which.questions[-1]-1, dim(my.data)[1])
    
  } #end if Include  
  
  #how many records are included in the database - different than number of questions, because every question can include sub-questions
  how.many.records<-dim(my.data)[1]
  
  #dealing with the questions to be excluded in case a particular question in included in a test (as specified in ExcludeIf)
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
  
  
  #checking if the column Topic was included in the database of questions
  if(is.null(my.data$Topic)) #my.data$Topic<-rep("", dim(my.data)[1])
    my.data$Topic<-NA
  #getting rid of the problem of case-sensitive topics
  my.data$Topic<-toupper(my.data$Topic)
  
  
  #final vector containing the topics 
  data.Topic<-as.factor(my.data$Topic)
  
  #vector containing the questions selected in the min.distance previous tests
  Index.questions<-vector("list", min.distance)
  
  #initializing a vector for saving the names of the files, exams and solutions
  if(repeat.each.test>1) my.filenames<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames<-vector("character", num.tests)
  
  
  #define two variables for indicating if the solutions should be generated or not (yes (T) or no (F) with generate.solutions), and if they should be in a short version (short(T) or not(F) in generate.solutions.short)
  ###modified 15/2/2010: to allow short transcription of solutions, all references to generate.solutions.short were added after this date 
  generate.solutions<-toupper(generate.solutions)   
  generate.solutions.short<-ifelse(generate.solutions=="SHORT", TRUE, FALSE)
  generate.solutions<-ifelse(generate.solutions=="FALSE", FALSE, TRUE)
  ##########################################################
  
  
  if(generate.solutions) {
    if(repeat.each.test>1) my.filenames.sol<-vector("character", num.tests*(repeat.each.test+1)) else my.filenames.sol<-vector("character", num.tests)
  }
  
  
  
  #######################################################################################################
  #beginning of the selection of the questions to be included in each (unique) test - indexed by tt
  #permuted tests will be generated later
  #######################################################################################################
  
  for(tt in 1:num.tests){
    
    cat("Generating exam number ", tt, "\n")
    
    #check if the topics to be included were specified and select the questions
    if(!is.null(topics)) {
      #checks if the specified topics correspond to those included in the database
      #could be improved for partial matching, here only exact matching works fine
      topics<-toupper(topics)
      
      if(any(!is.element(topics, data.Topic[which.questions]))){my.error<-"Some or all the topics that you specified are not included in your database"
                                                                stop("Some or all the topics that you specified are not included in your database") }
      
      #checks if the number of points for each topic were specified
      ###the last condition could be removed, if it is not necessary that the sum is 100
      ###if(is.null(topics.points) | length(topics.points)!=length(topics) | sum(topics.points)!=100), removed the requirement that the sum is 100
      if(is.null(topics.points) | length(topics.points)!=length(topics)){ my.error<-"Specify the number of points that you want to include for each of the selected topics"
                                                                          stop("Specify the number of points that you want to include for each of the selected topics")}
      
      #initialize the list that will contain the row-ID of the selected questions for each topic
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
          my.error<-paste("There are not enough questions for topic ", topics[i], "to meet the requirements on the number of points that were specified")
          stop("There are not enough questions for this topic to meet the requirements on the number of points that were         specified")
        }
        
        while(my.done==FALSE){
          
          
          #if an error is found reset - up to 100 times -> to give a second chance to situations in which an incompatible value is selected early and this selection cannot be corrected, an algorithm that finds the optimal combination of questions was not implemented
          if(length(which.questions.per.topic)==0 & num.errors>100) {
            cat("A problem was encountered for topic: ", topics[i], "\n")
            my.error<-paste("There are not enough questions for topic ", topics[i], "to meet the requirements on the number of points that were specified")
            stop("There are not enough questions for this topic to meet the requirements on the number of points that were         specified.")
          } #end if
          
          
          #if no questions are left for selection but number of errors is still small (less than 100), 
          #reset all the parameters as at the beginning of the selection of the questions
          if(length(which.questions.per.topic)==0 & num.errors<=100) {
            sum.points<-0
            num.errors<-num.errors+1
            index.questions[i]<-vector("list", 1)
            
            #reset the list of possible questions
            which.questions.per.topic<-which(!is.na(my.data$Question.ID) & data.Topic==topics[i] & my.data$Points<=topics.points[i]
                                             #exclude also the questions listed in the excludeIf list for the questions that were already selected
                                             & (!is.element(c(1:how.many.records), unlist(exclude.list[unlist(index.questions[1:i])])))
                                             & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude))))
            
            #next #chaged from break
            
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
                                            & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude)))  )   
          
          
          
          #}#end if
          if(sum.points==topics.points[i]) my.done<-TRUE
          
        }#end while
      }#end for i
    }#if topics not null       
    
    
    #no topics were specified, proceed using all the questions
    else if(is.null(topics)) {
      if(is.null(tot.points)) { my.error<-"You need to specify the total number of points to include in the exam"
                                stop("You need to specify the total number of points to include in the exam (using tot.points variable)")}
      
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
        my.error<-"There are not enough questions in your questions database to select enough questions using the criteria that were specified. Check the consistency of the required total number of points with the points for each question included in the database."
        stop("There are not enough questions in your questions database to select enough questions using the criteria that were specified. Check the consistency of the required total number of points with the points for each question included in the database.")
      }
      
      while(my.done==FALSE){
        #if an error is found reset - up to 100 times -> to give a second chance to situations in which an incompatible value is selected early and this selection cannot be corrected, an algorithm that finds the optimal combination of questions was not implemented
        if(length(which.questions.per.topic)==0 & num.errors<=100) {
          sum.points<-0
          num.errors<-num.errors+1
          index.questions<-numeric(0)
          #break
          #next
          #reset questions: select only the questions that have the selected topics and a number of points that is less than the selected limit
          which.questions.per.topic<-which(!is.na(my.data$Question.ID) & my.data$Points<=tot.points
                                           #exclude also the questions listed in the excludeIf list for the questions that were already selected
                                           & (!is.element(c(1:how.many.records), unlist(exclude.list[index.questions])))
                                           & (!is.element(c(1:how.many.records), unlist(Index.questions.4exclude))))		
        } #end if
        
        if(length(which.questions.per.topic)==0 & num.errors>100) {
          my.error<-"There are not enough questions in your questions database to select enough questions using the criteria that were specified."
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
      Index.questions.4exclude[[1]]<-index.questions.vector
    }
    
    
    
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
      
    }))
    
    
    
    Remember.questions.index.end[[tt]]<-end.index.questions.vector
    
    #number of questions selected
    num.questions<-length(index.questions.vector)
    
    #############################################################################
    #################end of selection of questions###############################
    #############################################################################
    
    
    
    
    ##############################################
    # beginning of writing of the files
    #############################################
    
    #generating the name of the file with the exams
    if(use.Sweave)     zz<-file.path(my.outdir, paste(my.prefix, tt, ".rnw", sep="")) else     zz<-file.path(my.outdir,  paste(my.prefix, tt, ".tex", sep=""))
    #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
    #tests 
    my.filenames[tt]<-zz
    
    
    #########################
    #writes the header
    #########################
    
    
    #if also the solutions need to be generated
    if(generate.solutions)  {
      
      #check if there was no Answer column in the database of questions
      if(is.null(my.data$Answer)) {my.data$Answer<-NA
                                   warning("You did not include a column with the answers in the database of questions - it must be called: Answer")	
      }
      #generate the name of the file with the answers
      if(use.Sweave) zz.sol<-file.path(my.outdir, paste(my.prefix,  tt, "sol",  ".rnw", sep=""))  else zz.sol<-file.path(my.outdir, paste(my.prefix,  tt, "sol",  ".tex", sep=""))
      
      #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
      #solutions 
      my.filenames.sol[tt]<-zz.sol
      
    }#end if generate.solutions
    
    
    my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}", head.prefix,     round(runif(1)*100), tt, " }", sep="")
    sink(zz)
    
    #    cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 "\\usepackage[slovene]{babel}", 		"\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n")
    
    
    #preamble of the latex file
    #different header is specified if Sweave is used
    
    ##modified 18/2/2010: added  "\\usepackage[pdftex]{graphicx,color}" to handle graphs and colors - useful for users that include graphs in the tests. It is necessary if Sweave is not used; if used together with Swaeve it MUST appear in the header before Sweave, otherwise it generates a conflict with the Sweave package.
    
    if(use.Sweave){
      cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage[pdftex]{graphicx,color}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n")  
      if(generate.solutions)             cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage[pdftex]{graphicx,color}", "\\usepackage{Sweave}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) 
    }			else     {cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}",  "\\usepackage[pdftex]{graphicx,color}", "\\begin{document}", "\\maketitle{}", sep="\n")
                  
                  if(generate.solutions) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}",  "\\usepackage[pdftex]{graphicx,color}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)
                  
    }#end else if use.Sweave
    
    cat(my.header)
    if(generate.solutions)    {cat(my.header, file=zz.sol, append=TRUE)}
    
    
    
    #######################################
    #beginning of writing of the questions
    #######################################
    
    
    
    cat("\\begin{itemize}\n")
    if(generate.solutions) cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)
    
    #save the random seed at the beginning of writing of the file
    my.old.seed<-.Random.seed
    
    
    for(i in 1:length(index.questions.vector)){
      #writes the number of question, points and main question
      cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
      
      #######################################
      # writing the solution files, as well
      # short or long versions
      #######################################
      
      if(generate.solutions) {
        if(!generate.solutions.short) cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }", as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE) else  cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }"), sep="", file=zz.sol, append=TRUE) #modified 15/2/2010: if short solutions were required (generate.solutions="SHORT") only the answeres are reported
        #if(my.data$Answer[index.questions.vector[i]]!="" |  !is.na(my.data$Answer[index.questions.vector[i]])) 
        if(!is.na(my.data$Answer[index.questions.vector[i]])) 
          cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
      }   #end if(generate.solutions)
      
      #writing extra-lines, if required
      if(!is.na(my.data$ExtraLines[index.questions.vector[i]])) {
        #  cat((rep("\\newline", my.data$ExtraLines[index.questions.vector[i]], sep="")))
        #modified Feb2012 to avoid the "there is no line to end" error in latex, newline replaced by vspace
        #  cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")))
        #if(generate.solutions) if(!generate.solutions.short) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE) #else cat("\\vspace{\\baselineskip}", file=zz.sol, append=TRUE) 
        if(generate.solutions) if(!generate.solutions.short) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE) #else cat("\\vspace{\\baselineskip}", file=zz.sol, append=TRUE) 
        
      }
      
      if(end.index.questions.vector[i]!=index.questions.vector[i]){
        cat("\\begin{enumerate}[(a)]\n") 
        if(generate.solutions) cat("\\begin{enumerate}[(a)]\n", file=zz.sol, append=TRUE) 
        #permuting the sub-questions
        if(my.data[index.questions.vector[i],]$PermuteOK==TRUE)
          perm.order<-resample(c((index.questions.vector[i]+1): end.index.questions.vector[i])) else perm.order<-c((index.questions.vector[i]+1): end.index.questions.vector[i])
        
        for(j in perm.order){
          cat("\\item ") 
          cat(as.character(my.data$Question[j]), "\n")
          if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[j], sep="")))
          if(generate.solutions) {
            cat("\\item ", file=zz.sol, append=TRUE) 
            #writes the questions only if the extended version of the solutions was requested, modified 15/2/2010
            if(!generate.solutions.short) cat(as.character(my.data$Question[j]), "\n", file=zz.sol, append=TRUE)
            if(!is.na(my.data$Answer[j])) 
              cat("{\\bf", as.character(my.data$Answer[j]), "}",  file=zz.sol, append=TRUE)
            #add extra lines only in the extended version of the solutions, modified 15/2/2010
            if(!is.na(my.data$ExtraLines[j]) & !generate.solutions.short) cat((rep("\\vspace{\\baselineskip}", (my.data$ExtraLines[j]-1), sep="")), file=zz.sol, append=TRUE)
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
  
  ##############################################################################   
  ###############end of writing of files for the main tests ####################
  ##############################################################################
  
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
        if(use.Sweave) zz<-file.path(my.outdir, paste(my.prefix, pp, tt, ".rnw", sep="")) else zz<-file.path(my.outdir, paste(my.prefix, pp, tt, ".tex", sep=""))
        
        #saving the name of the file
        my.filenames[num.tests+repeat.each.test*(tt-1)+pp]<-zz
        
        #if also the solutions need to be generated
        if(generate.solutions) {
          
          #generate the name of the file with the answers
          if(use.Sweave) zz.sol<-file.path(my.outdir, paste(my.prefix,  pp, tt, "sol",  ".rnw", sep="")) else zz.sol<-file.path(my.outdir, paste(my.prefix,  pp, tt, "sol",  ".tex", sep="")) 
          
          #saving the names of the files (rnw or tex, depending on whether Sweave is used or not)
          #solutions 
          my.filenames.sol[num.tests+repeat.each.test*(tt-1)+pp]<-zz.sol
          
          
        }#end if generate.solutions
        
        
        
        
        #my.test.id<-paste(pp, tt, sep="")
        
        my.header<-paste("\\framebox{", head.name, ":\\hspace{3cm}",  head.id, ":\\hspace{3cm}", head.points,  ":\\hspace{2cm}",
                         head.prefix, round(runif(1)*100), pp, tt, " }", sep="")
        
        sink(zz)
        
        #####################################################
        ################writing header######################
        ####################################################
        
        #preamble of the latex file, modified 18/2/2010: added graphics and color packages, must appear BEFORE Sweave
        if(use.Sweave) cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage[utf8]{inputenc}", "\\usepackage{enumerate}", "\\usepackage[pdftex]{graphicx,color}", "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n") else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}",   "\\usepackage[cm,empty]{fullpage}", "\\usepackage[pdftex]{graphicx,color}", "\\begin{document}", "\\maketitle{}", sep="\n")
        
        if(generate.solutions) {if(use.Sweave)  cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""), "\\usepackage{enumerate}", "\\usepackage[pdftex]{graphicx,color}", "\\usepackage{Sweave}", "\\usepackage[cm,empty]{fullpage}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol) else cat("\\documentclass{article}", paste("\\title{", my.title, "}", sep=""), paste("\\date{", my.date, "}", sep=""),                 paste("\\usepackage[", my.language, "]{babel}", sep=""),  "\\usepackage[utf8]{inputenc}","\\usepackage{enumerate}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage[pdftex]{graphicx,color}", "\\begin{document}", "\\maketitle{}", sep="\n", file=zz.sol)}
        
        
        cat(my.header)
        
        ##################################
        #beginning of the questions
        ##################################
        
        
        cat("\\begin{itemize}\n")
        if(generate.solutions) {cat(my.header, file=zz.sol, append=TRUE)
                                cat("\\begin{itemize}\n", file=zz.sol, append=TRUE)}
        
        for(i in 1:num.questions){
          cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }",
                as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="")
          if(generate.solutions){ if(!generate.solutions.short)  cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }",
                                                                       as.character(my.data$Question[index.questions.vector[i]]), "\n"), sep="", file=zz.sol, append=TRUE)    else   cat(c("\\item[", i, "] {\\small $\\left[", my.data$Points[index.questions.vector[i]], "\\right]$ }"), sep="", file=zz.sol, append=TRUE) 
                                  if(!is.na(my.data$Answer[index.questions.vector[i]])) 
                                    cat("{\\bf", "\n", as.character(my.data$Answer[index.questions.vector[i]]), "}",  file=zz.sol, append=TRUE)
          }
          
          if(!is.na(my.data$ExtraLines[index.questions.vector[i]])){
            cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")))
            if(generate.solutions & !generate.solutions.short) 
              cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[index.questions.vector[i]], sep="")), file=zz.sol, append=TRUE)
          }
          
          if(end.index.questions.vector[i]!=index.questions.vector[i]){
            cat("\\begin{enumerate}[(a)]\n")
            if(generate.solutions) cat("\\begin{enumerate}[(a)]\n", file=zz.sol, append=TRUE)
            if(my.data[index.questions.vector[i],]$PermuteOK==TRUE)
              perm.order<-resample(c((index.questions.vector[i]+1): end.index.questions.vector[i])) else perm.order<-c((index.questions.vector[i]+1): end.index.questions.vector[i])
            
            ############permuting sub-questions##################	
            
            for(j in perm.order){
              cat("\\item ") 
              cat(as.character(my.data$Question[j]), "\n")
              if(!is.na(my.data$ExtraLines[j])) cat((rep("\\vspace{\\baselineskip}", my.data$ExtraLines[j], sep="")))
              if(generate.solutions) {
                cat("\\item ", file=zz.sol, append=TRUE) 
                if(!generate.solutions.short)	cat(as.character(my.data$Question[j]), "\n", file=zz.sol, append=TRUE)
                if(!is.na(my.data$Answer[j])) 
                  cat("{\\bf", as.character(my.data$Answer[j]), "}",  file=zz.sol, append=TRUE)
                if(!is.na(my.data$ExtraLines[j]) & !generate.solutions.short) cat((rep("\\vspace{\\baselineskip}", (my.data$ExtraLines[j]-1), sep="")), file=zz.sol, append=TRUE)
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
  
  ################# end of writing of the permuted tests ########################   
  
  #cat(my.filenames)
  #cat(my.filenames.sol)
  
  
  
  #######################
  # move the files
  #######################
  
  
  ################ moving the extra files needed to generate the exams ################
  
  if(!is.null(files.to.move)){
     #moves the files
    file.copy(files.to.move, my.outdir)
    }#end move the files
  
  
  
  
  
  ################################
  #compiling Sweave files 
  ################################
  
  #save the initial working directory
  my.oldwd<-getwd()
  #add on.exit: RETURNS TO original dirctory if an error occurs			
  on.exit(setwd(my.oldwd)	, add=T)
  
  
  if(use.Sweave){
    #obtaing tex files from rnw files, Sweave must be accessible from the directory my.outdir
    #my.files<-dir(path=my.outdir, pattern=".rnw", full.names=TRUE)   
    ###	my.oldwd<-getwd() - moved up
    setwd(my.outdir)
    
    
    
    #for reproducibility of the solutions, save the seed
    #my.old.seed<<-.Random.seed
    
    #use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
    for(i in 1:length(my.filenames)){
      set.seed(i+my.seed)
      #### temporary removed command #shell(Sweave(my.filenames[i]))
      
      ################## change to avoid blocking the tex program with this call #######################
      ######shell(Sweave(my.filenames[i]))      ### removed 24012011
      
      
      Sweave(my.filenames[i])			 #################changed 24012011
      #texi2dvi(my.filenames[i], pdf=T)			 #################changed 24012011
      ################## change #######################
      
      
      #				my.file<-my.filenames[i]
      #			my.command<-paste("Sweave", my.file)
      #run pdflatex, and check if there were errors, stop the function in case of errors
      #			out.pdflatex<-try(system(my.command))
      
      if(generate.solutions){
        set.seed(i+my.seed)
        ###	shell(Sweave(my.filenames.sol[i]))
        Sweave(my.filenames.sol[i])
        #texi2dvi(my.filenames.sol[i], pdf=T)			 #################changed 24012011
        
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
    #go back to the original directory if an error occurs
    on.exit(setwd(my.oldwd)	, add=T)
    
    #use the names of the saved files, avoids compiling files with rnw extension present in my.outdir before running this program 
    #if(generate.solutions) my.files<-c(my.filenames, my.filenames.sol) else my.files<-my.filenames
    
    for(i in 1:length(my.files)){
      #my.file<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")	
      #my.file<-unlist(strsplit(my.file,  "\\\\"))
      ##extract just the name of the file, without using the path
      #my.file<-my.file[length(my.file)]
      
      #extract just the name of the file, without using the path and use the tex files only
      my.file=paste(unlist(strsplit(basename(my.files[i]), "\\."))[[1]], ".tex", sep="")
      
     # cat(my.file, "transf file\n")
    #  cat(my.files[i], "OF file\n")
    #  cat(my.outdir, "OD\n")
      
      #my.file=basename(my.files[i])
      #cat(my.file, "\n")
      
      
      ##my.command<-paste("pdflatex", my.file)
      #run pdflatex, and check if there were errors, stop the function in case of errors
      ##out.pdflatex<-try(system(my.command))
      
      #updated July 2013, uses texi2dvi instead of pdflatex
      out.pdflatex=try(texi2dvi(my.file, pdf=TRUE, clean=TRUE, quiet=TRUE))
      
      #if there was an error - returns a code different than 0
      if(!is.null(out.pdflatex))         {
        #return to the original directory
        setwd(my.oldwd)
        my.error<-"There was an error in compiling LaTeX in PDF files with pdflatex - more details are displayed in the R console"
        stop("There was an error compiling the LaTeX file(s)")
        
      }# end out.pdflatex, error in pdf compilation
    } #end for i
    setwd(my.oldwd)	
  }#end if compile.pdf
  
  
  #saving the names of the final output files, tex or pdf, depending on the selected options
  #for(i in 1:length(my.files)){
  #  if(compile.pdf) my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".pdf", sep="")	else my.files[i]<-paste((strsplit(my.files[i], "\\."))[[1]][1], ".tex", sep="")
  #}	
  
  #outputting questions ID instead of row number
  Remember.questions.index<-lapply(Remember.questions.index, function(x) my.data$Question.ID[x])
  
  names(Remember.questions.index)<-paste("Test", 1:num.tests)   
  #close the open connections, if any - can be problematic if errors occur and the function is stopped
  ####	on.exit{(if(sink.number()!=0) for(i in 1:sink.number()) sink()) moved up the on.exit()
  #return to starting directory
  ####   setwd(my.oldwd)	moved up the on.exit()
  # }  #end on.exit
  
  

  ################ save separately the name of the files and the directory where they are stored
  
  cat("\n\n\n\n", my.files, "names of the files \n\n\n\n")
  names.files=basename(my.files)
  
  for(i in 1:length(names.files)){
    if(compile.pdf) names.files[i]=paste(unlist(strsplit(basename(names.files[i]), "\\."))[[1]], ".pdf", sep="") else 
      names.files[i]=paste(unlist(strsplit(basename(names.files[i]), "\\."))[[1]], ".tex", sep="")}
  
  
  
  dir.files=dirname(my.files)[1]
  
  #cat("\n\n\n\n", names.files, "names of the files \n\n\n\n")
  #cat("\n\n\n\n", dir.files, "names of the files \n\n\n\n")
  
  if(compile.pdf==TRUE & merge.pdf==TRUE) Merge.pdf(my.files=names.files, my.dir=dir.files, outfile="MergedFiles")
  
  
  #if(compile.pdf==TRUE){
  #  #names of the files, without the path
  #  names.files=unlist(lapply(strsplit(unlist(lapply(strsplit(my.files, "\\."), 
  #                                                                       function(x) paste(x[1], ".pdf", sep=""))), "\\\\"), function(x) x[length(x)]))
  #  #path only
  #  dir.files=unlist(lapply(strsplit(unlist(lapply(strsplit(my.files, "\\."), 
  #                                                 function(x) paste(x[1], ".pdf", sep=""))), "\\\\"), function(x) x[-length(x)]))[1]
  #} else {
  #  #names of the files, without the path
  #  names.files=unlist(lapply(strsplit(unlist(lapply(strsplit(my.files, "\\."), 
  #                                                   function(x) paste(x[1], ".tex", sep=""))), "\\\\"), function(x) x[length(x)]))
  #  #path only
  #  dir.files=unlist(lapply(strsplit(unlist(lapply(strsplit(my.files, "\\."), 
  #                                                 function(x) paste(x[1], ".tex", sep=""))), "\\\\"), function(x) x[-length(x)]))[1]
      
  #  }
  
  
  
  my.files=file.path(dir.files, names.files)
  
  merged.file=ifelse(compile.pdf==TRUE & merge.pdf==TRUE, file.path(dir.files, "MergedFiles.pdf"), "Merging of the PDF files not requested")
  
  return(list(Questions=Remember.questions.index, files=my.files, names.files=names.files, dir.files=dir.files,  merged.file=merged.file, errors=my.error))       
}#end function genertest()   

####################################################################
################end genertest() ####################################
####################################################################







