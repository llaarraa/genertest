shinyServer(function(input, output) {
  
  
  
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
  
  
  
  
  
  dataset1 <- reactive({
    dataset=read.csv(input$file1$datapath, header=TRUE, sep="\t")
  })
  
  
  output$topics.points <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1))
      return()
    
    # Get the data set with the appropriate name
    #dat1 <- get(input$file1)
    dat1=read.csv(input$file1$datapath, header=TRUE, sep="\t")
    topics <- names(table(dat1$Topic, exclude=c("","NA","end")))
    
    # Create the checkboxes and do not select them all by default
    #matrixInput("topics.points", "Topics and Points", cbind.data.frame(topics, 0))
    matrixInput("topics.points", "Points for each topic (Do NOT change the names of the topics, edit the number of points only)", cbind.data.frame(topics, rep(0, length(topics))))
    
    # Create the checkboxes and do not select them all by default
    #checkboxGroupInput("topics", "Choose topics", 
    #                   choices  = topics,
    #                   selected = NULL)
  })
  
  
  
  f=function(num.tests, tot.points){
    num.tests*tot.points  
    
    
  }
  
  #dataset=read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  
  
  output$Tot.points=renderText(f(input$num.tests, input$tot.points))
  
  #output$QID=renderText(input$file1$name)
  #output$QID=renderText(class(dataset1))
  #output$QID=renderText(input$file1$datapath)
  
  #browser()
  dat=reactive({
    
    if(is.null(input$file1))
      return()
    
    # Get the data set with the appropriate name
    #dat1 <- get(input$file1)
    #dat1=read.csv(input$file1$datapath, header=TRUE, sep="\t")
    #topics <- names(table(dat1$Topic, exclude=c("","NA","end")))
    
    #remove the topics if not selected
    if(sum(input$topics.points[,2])==0 | is.na(sum(input$topics.points[,2]))){
      topics.points=NULL
      topics=NULL
          
    } else {topics.points=input$topics.points[,2]
            topics=names(table(dataset1()$Topic, exclude=c("","NA","end")))  
            }
      
    #browser()
    #if(input$start==TRUE){
    g.out=genertest(input$file1$datapath, num.tests=input$num.tests,                             
                   repeat.each.test=input$repeat.each.test, my.seed=input$my.seed, 
                   topics=topics, topics.points=topics.points, 
                  #  topics.points=input$topics.points[,2], 
                   # topics=names(table(dataset1()$Topic, exclude=c("","NA","end"))),
                   tot.points=input$tot.points, 
                   min.distance=input$min.distance, generate.solutions=input$generate.solutions, 
                   my.title=input$my.title, my.date=input$my.date, my.prefix=input$my.prefix,     
                   head.name=input$head.name, head.id=input$head.id, head.points=input$head.points, head.prefix=input$head.prefix, 
                   my.language=input$my.language, 
                   use.Sweave=input$use.Sweave, 
                   compile.pdf=input$compile.pdf,
                   my.final.sentence=input$my.final.sentence, merge.pdf=input$merge.pdf, 
                   files.to.move=NULL)
    
    
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
