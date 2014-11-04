R -e 'library(knitr); knit("EpitopeMatcherPresentation.Rnw")'; 
pdflatex EpitopeMatcherPresentation.tex
evince EpitopeMatcherPresentation.pdf
