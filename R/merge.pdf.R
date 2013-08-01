`Merge.pdf` <-
function(genertest.output=NULL, my.files=NULL, my.dir=NULL, outfile="merge"){

  #function that merges pdf files within R, using pdfpages LaTeX package 
	#############function's arguments####################
  #genertest.output: object generated using the genertest function
	#my.files: name of the pdf files, without path; for example: c("Exam1.pdf", "Exam2.pdf")
	#my.dir: directory where the files are stored; for example: "c:\\Documents and Settings\\Exams"
	#outfile: name of the file with the merged pdfs; for example: "merged.pdf" 
	########################################################
	#output: a merged pdf file, in the same directory as the original pdf files - in my.dir

  my.error="There were no errors in merging the files"
  
	if(!is.null(genertest.output)){
	                my.files<-genertest.output$names.files
                  my.dir=genertest.output$dir.files
                  
	
	}
  
  if(is.null(genertest.output) & (is.null(my.files) | is.null(my.dir))) {
            stop("You pass either the output obtained with the genertest function or specify the files that you want to merge and the directory where they are stored")  
  }
  
    
    
  
	# for example the result in this case would be my.files<-c("Exam1.pdf", "Exam2.pdf", "Exam1sol.pdf","Exam2sol.pdf")
	
  
  
  
	#preparing the tex file for writing
	my.outfile<-file.path(my.dir, paste(outfile, ".tex", sep=""))
	sink(my.outfile)
	cat("\\documentclass{article}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{pdfpages}", "\\begin{document}",  sep="\n")	
	cat("\\includepdfmerge[pages=-]{")
	cat(my.files, sep=", ")
	cat("}",  "\\end{document}", sep="\n")
	sink()
	my.oldwd<-getwd()
	
  #my.command<-paste("pdflatex", outfile)
	setwd(my.dir)
	  
	out.pdflatex=try(texi2dvi(my.outfile, pdf=TRUE, clean=TRUE, quiet=TRUE))
	
	#if there was an error - returns a code different than 0
	if(!is.null(out.pdflatex))         {
	  #return to the original directory
	  setwd(my.oldwd)
	  my.error<-"There was an error in compiling LaTeX in PDF files with pdflatex - more details are displayed in the R console"
	  stop("There was an error compiling the LaTeX file(s)")
	  
	}# end out.pdflatex, error in pdf compilation


  
	setwd(my.oldwd)
	return(list(file=file.path(my.dir, paste(outfile, "pdf", sep=".")), errors=my.error))

}

