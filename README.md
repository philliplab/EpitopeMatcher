EpitopeMatcher
==============

A package that can be used to find out how well the epitopes in a patient's virus' will be
recognized by the HLA's present in the patient.

It can be install directly from github using devtools:
```r
install.packages('devtools')
library(devtools)
install_github('EpitopeMatcher', 'philliplab')
library(EpitopeMatcher)
```

To run the web UI:
```r
library(EpitopeMatcher)
run_EpitopeMatcher_app()
```
