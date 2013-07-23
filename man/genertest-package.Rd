\name{genertest-package}
\alias{genertest-package}
\docType{package}
\title{
(Automated) Exam Generation using R
}
\description{
Package that generates tests with questions drawn from a tab-delimited database. It can be used to randomly draw questions, to permute questions and answers, to select questions to include in the tests according to their topic or difficulty, to generate questions containing Sweave syntax. 
}
\details{
\tabular{ll}{
Package: \tab genertest\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-01-14\cr
License: \tab GPL2\cr
LazyLoad: \tab yes\cr
}
Store your questions in a tab delimited database and use function genertest() to generate tests. Specify topics and difficulty of the questions to be included in the exams, the number of completely different or permuted exams to be generated; many other options are available. The output is a LaTeX or PDF file for each generated test. Function write.all() writes on a file that has the same structure as the exams all the questions included in the database; merge.pdf() can be used to merge PDF files containing different exams in a single file.
}
\author{
Lara Lusa

Maintainer: Lara Lusa <lara.lusa@mf.uni-lj.si>
}
\references{
\url{http://sites.google.com/site/lara3107/Home/software/genertest}
}
\keyword{package}
