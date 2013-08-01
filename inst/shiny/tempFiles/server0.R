shinyServer(function(input, output) {
  
  
  output$contents <- renderTable({
    
    
    
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    
    genertest(as.character(inFile$datapath), my.outdir=NULL,  num.tests=input$num.tests, 
              #                  #repeat.each.test=input$repeat.each.test, my.seed=input$my.seed, 
              #                  #topics=NULL, topics.points=NULL, 
                               tot.points=reactive(input$tot.points)) 
              ##                  #, min.distance=input$min.distance, generate.solutions=input$generate.solutions, 
              #                  #my.title=input$my.title, my.date=input$my.date, my.prefix=input$my.prefix,     
              #                  #head.name=input$head.name, head.id=input$head.id, head.points=input$head.points, head.prefix=input$head.prefix, my.language="english", use.Sweave=input$use.Sweave, 
              #                  #compile.pdf=input$compile.pdf,
              #                  #my.final.sentence=NULL, files.to.move=NULL
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
    
    
    
    
    })#end renderTable
  
  
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
  
  renderText(input$num.tests)
  
  
  
  
  
  
})#end shinyServer