#cat(getwd(), "out \n\n\n")

shinyServer(function(input, output) {
  
  #cat(getwd(), "in \n\n\n")
  
  # values <- reactiveValues()
  #  if (interactive() == TRUE) {
  #   observe({
  #    if (input$upload != 0) {
  #      values$fpath <- try(file.choose(), silent=TRUE)
  #    }
  #  })
  #} else {
  #  observe({
  #    if (is.null(input$file1) || nrow(input$file1) == 0)
  #      values$fpath <- NULL
  #    else
  #      values$fpath <- input$file1[1,'datapath']
  #  })
  #}
  
  
  
  #values <- reactiveValues()
  #values$topics<-observe({
    
  #  if(is.null(input$file1))
  #    return()
    
  #  # Get the data set with the appropriate name
  #  #dat1 <- get(input$file1)
  #  dat1=read.csv(input$file1$datapath, header=TRUE, sep="\t")
  #  topics <- names(table(dat1$Topic, exclude=c("","NA","end")))
  #  return(topics)
    
  #}) 
  
  
 
  
  
  
  #shows the data included in the uploaded file
  output$contents <- renderTable({
    
        
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    
    
    
    dataset=read.csv(input$file1$datapath, header=TRUE, sep="\t")
    #  dataset=read.csv(values$fpath, header=TRUE, sep="\t")
    
    
    
    
  })#end renderTable
  
  
  
  #shows the data included in the uploaded file, summarized by topic and number of points per question
  output$dataSummary <- renderTable({
    
    
    
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    
    
    
    dataset=read.csv(input$file1$datapath, header=TRUE, sep="\t")
    #  dataset=read.csv(values$fpath, header=TRUE, sep="\t")
    
    
    addmargins(table(Topic=dataset$Topic, Points=dataset$Points, exclude=c("", "end", "NA", 0)))
    
  })#end renderTable
  
  
  
  
  #read the dataset, store it as a reactive variable
  dataset1 <- reactive({
    dataset=read.csv(input$file1$datapath, header=TRUE, sep="\t")
  })
  
  
  #construct the matrix with the IDs of the questions to use to construct the tests
  output$QID <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1))
      return()
    
    # Get the data set with the appropriate name
    #dat1 <- get(input$file1)
#    dat1=read.csv(input$file1$datapath, header=TRUE, sep="\t")
#    topics <- names(table(dat1$Topic, exclude=c("","NA","end")))
    
    # Create the checkboxes and do not select them all by default
    #matrixInput("topics.points", "Topics and Points", cbind.data.frame(topics, 0))
    
    my.df=data.frame(matrix(NA, ncol=input$num.tests, nrow=10+1))#, row.names=paste("Question ID", 1:10), col.names=paste("Test", 1:input$num.tests))
    
    #dimnames(my.df)[[1]]=paste("Question", 1:10)
    #dimnames(my.df)[[2]]=c("Question ID to include", paste("Test", 1:input$num.tests))
    
    #my.df[,1]=c("Questions", paste("ID", 1:10))
    my.df[1,]= paste("Test", 1:input$num.tests)
    
    
    matrixInput("QID", "Questions to include in each test (use the numbers that appear in the Questions.ID column of your questions database. You can add more questions using the + button, do not modify the header (first row) of the matrix)", my.df)
    
    # Create the checkboxes and do not select them all by default
    #checkboxGroupInput("topics", "Choose topics", 
    #                   choices  = topics,
    #                   selected = NULL)
  })
  
  
  
  #f=function(num.tests, tot.points){
  #  num.tests*tot.points  
  #}
  
  #dataset=read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  
  
  output$Tot.points=renderText(f(input$num.tests, input$tot.points))
  
  #output$QID=renderText(input$file1$name)
  #output$QID=renderText(class(dataset1))
  #output$QID=renderText(input$file1$datapath)
  
  #browser()
  dat=reactive({
    
    if(is.null(input$file1) | all(is.na(input$QID[-1,])))
      return()
    
    # Get the data set with the appropriate name
    #dat1 <- get(input$file1)
    #dat1=read.csv(input$file1$datapath, header=TRUE, sep="\t")
    
    
    #obtain the list with the questions
    list.QID=apply(input$QID[-1,],2, function(x) x[!is.na(x) & x!=0] )
	
	
	#Bug fixed Sept2013: transform in a list, if not done automatically (as it is done when the number of questions is not the same for all the tests
	
	if(class(list.QID)=="matrix")  list.QID=as.list(as.data.frame(list.QID))
	
    #remove possibly empty tests
    list.QID=list.QID[unlist(lapply(list.QID, function(x) length(x)>0))]
    
	
	
    
    
    #cat(input$my.outdir, "\n\n\n")
    
    #set to NULL the outdir if not provided by the user, otherwise use the path provided
    if(input$my.outdir=="") {my.outdir=NULL} else my.outdir=input$my.outdir
                            
   
    #cat(class(input$files.to.move), "files to move \n\n\n")
    
    #browser()
    #if(input$start==TRUE){
    g.out=genertestPreSelect(input$file1$datapath, my.outdir=my.outdir, #my.outdir=NULL, #getwd(),
                             #list.QID=list(c(1,2,3), c(2,3,4), c(3,4,5)), ### temp
                             list.QID=list.QID, 
                             num.tests=input$num.tests,   
                   repeat.each.test=input$repeat.each.test, my.seed=input$my.seed, 
                   #topics=topics, topics.points=topics.points, 
                   # tot.points=input$tot.points, 
                 #  min.distance=input$min.distance, 
                   generate.solutions=input$generate.solutions, 
                   my.title=input$my.title, my.date=input$my.date, my.prefix=input$my.prefix,     
                   head.name=input$head.name, head.id=input$head.id, head.points=input$head.points, head.prefix=input$head.prefix, 
                   my.language=input$my.language, 
                   use.Sweave=input$use.Sweave, 
                   compile.pdf=input$compile.pdf,
                   merge.pdf=input$merge.pdf, 
                   my.final.sentence=input$my.final.sentence, 
                   files.to.move=input$files.to.move$datapath, names.files.to.move=input$files.to.move$name)
    
  
    
    
      #Merge.pdf(genertest.output=g.out, outfile = "mergedFiles")
    
    
      return(g.out)#} 
    
  })# end reactive()
  
  
  #  output$outdir <- renderPrint({
  #    (my.out$files)
  #  })
  
  
  #output$genertest=renderText(genertest(dataset1, num.tests=input$num.tests, tot.points=input$tot.points))
  #output$genertest=renderText(input$file1$datapath)
  
  
  #if(reactive(input$start)==TRUE){
  
  # dat=reactive(  
  #    out=genertest(as.character(inFile$datapath), my.outdir=NULL,  num.tests=input$num.tests, 
  #                  #repeat.each.test=input$repeat.each.test, my.seed=input$my.seed, 
  #                  #topics=NULL, topics.points=NULL, 
  #                  tot.points=reactive(input$tot.points) 
  ##                  #, min.distance=input$min.distance, generate.solutions=input$generate.solutions, 
  #                  #my.title=input$my.title, my.date=input$my.date, my.prefix=input$my.prefix,     
  #                  #head.name=input$head.name, head.id=input$head.id, head.points=input$head.points, head.prefix=input$head.prefix, my.language="english", use.Sweave=input$use.Sweave, 
  #                  #compile.pdf=input$compile.pdf,
  #                  #my.final.sentence=NULL, files.to.move=NULL
  #    )
  
  
  # )
  
  
  #output$genertest=renderPrint(dat()[[1]])
  output$genertest=renderPrint(dat())
 # output$topics=renderPrint(paste("Questions: ", cat(dat()$Questions),  "Files: ", cat(dat()$files), "Errors: ", cat(dat()$errors),  sep="\n"))
  output$topics=renderText(paste("Questions: ", paste(dat()$Questions, collapse="\n"),  "Files: ", paste(dat()$files, collapse="\n"), "Errors: ", 
                                  paste(dat()$errors, collapse="\n"),  collapse="\n"))
  
  #output$topics=renderTable({
  #  num.el=unlist(lapply(TMP, length))
   # max.el=max(num.el)+1
  #  rbind(c("Questions", paste(dat()$Questions, collapse=", "), rep("", max.el-num.el[1])), 
  #        c("Files", paste(dat()$names.files, collapse=", "),  rep("", max.el-1)), 
  #        c("Directory", dat()$dir.files,  rep("", max.el-1)),
  #        c("Error", dat()$errors,  rep("", max.el-1))
  #    )
    
          
          
   #       })
   
  
  output$topics=renderTable({
    #foo=as.data.frame(as.matrix(dat()$files), as.matrix(lapply(dat()$Questions, function(x) paste(x, collapse=", "))))
    #foo=cbind.data.frame(as.matrix(dat()$files), as.matrix(rep(unlist(lapply(dat()$Questions, function(x) paste(x, collapse=", ")))), length(dat()$files)/length(dat()$Questions)))
    foo=cbind.data.frame(as.matrix(c(dat()$files, dat()$merged.file)), as.matrix(c(rep(unlist(lapply(dat()$Questions, function(x) paste(x, collapse=", "))), length(dat()$files)/length(dat()$Questions)), "Merged file")))
    names(foo)=c("Files", "Questions")
    #foo=rbind.data.frame(foo, c(dat()$merged.file, "Merged file"))
    foo
    })
    
    
    
 
  
  
  #paste(cat("Files: "), cat(TMP$files) , sep="\n")
  #output$topics=renderText(isolate(values$topic))
  #output$genertest=renderPrint(dat[[1]])
  #output$genertest=reactiveText(dat[[1]])
  #output$dat2=isolate(dat())
  
  #browser() 
  
})#end shinyServer
