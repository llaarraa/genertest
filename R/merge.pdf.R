`Merge.pdf` <-
function(my.files, my.dir, outfile="merge"){
	#function that merges pdf files within R, using pdfpages LaTeX package 
	#############function's arguments####################
	#my.files: name of the pdf files, without path; for example: c("Exam1.pdf", "Exam2.pdf")
	#my.dir: directory where the files are stored; for example: "c:\\Documents and Settings\\Exams"
	#outfile: name of the file with the merged pdfs; for example: "merged.pdf" 
	########################################################
	#output: a merged pdf file, in the same directory as the original pdf files - in my.dir

	#preparing the tex file for writing
	my.outfile<-paste(paste(my.dir, outfile, sep="\\"), ".tex", sep="")
	sink(my.outfile)
	cat("\\documentclass{article}", "\\usepackage[cm,empty]{fullpage}", "\\usepackage{pdfpages}", "\\begin{document}",  sep="\n")	
	cat("\\includepdfmerge[pages=-]{")
	cat(my.files, sep=", ")
	cat("}",  "\\end{document}", sep="\n")
	sink()
	my.oldwd<-getwd()
	my.command<-paste("pdflatex", outfile)
	setwd(my.dir)
	system(my.command)
	setwd(my.oldwd)
	
}

