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
  
  
  
  
  dataset1 <- reactive({
    dataset=read.csv(input$file1$datapath, header=TRUE, sep="\t")
  })
  
  
  f=function(num.tests, tot.points){
    num.tests*tot.points  
    
    
  }
  
  #dataset=read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  
  
  output$Tot.points=renderText(f(input$num.tests, input$tot.points))
  
  #output$QID=renderText(input$file1$name)
  #output$QID=renderText(class(dataset1))
  #output$QID=renderText(input$file1$datapath)
  
  
  #observe({
    #if (input$action_button == 0) # tells action button to do nothing when not clicked ..
  #    return()
    #isolate({ 
    
  
  
  
  dat=reactive({
    #if (input$action_button == 0) # tells action button to do nothing when not clicked ..
    #  return()
    
    #if(input$start==TRUE){
    tmp<-genertest(input$file1$datapath, num.tests=input$num.tests,                             
                   repeat.each.test=input$repeat.each.test, my.seed=input$my.seed, 
                   topics=NULL, topics.points=NULL, 
                   tot.points=input$tot.points, 
                   min.distance=input$min.distance, generate.solutions=input$generate.solutions, 
                   my.title=input$my.title, my.date=input$my.date, my.prefix=input$my.prefix,     
                   head.name=input$head.name, head.id=input$head.id, head.points=input$head.points, head.prefix=input$head.prefix, 
                   my.language="english", 
                   use.Sweave=input$use.Sweave, 
                   compile.pdf=input$compile.pdf,
                   my.final.sentence=input$my.final.sentence, 
                   files.to.move=NULL)
    
    
    #    return(g.out)#} 
    
  #return(tmp)
   } )# end reactive()
  
  
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
  #  })
  #}) #end observe()
  
  output$dat=dat
  #output$dat2=isolate(dat())
  
  
  
})#end shinyServer