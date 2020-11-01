FROM rocker/drd@sha256:edbb713565a37868865712b4fbe63cf0c155d8fa9670401d0836e66c86b5f787
RUN apt-get update && apt-get install -y libssl-dev libxml2-dev libcurl4-gnutls-dev && \
    Rscript -e " \
        install.packages('devtools', repo = 'http://cran.rstudio.com/'); \
        if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); \
        BiocManager::install('Biostrings'); \
        library(devtools); install_github('rstudio/shiny'); install_github('philliplab/EpitopeMatcher'); \
    "
