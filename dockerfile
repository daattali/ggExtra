FROM rocker/tidyverse

ADD . /home/ggExtra

WORKDIR /home/ggExtra

ENV RunGgplot2Tests=yes

RUN apt-get update; \
  apt-get install -y --no-install-recommends libfreetype6; \
  Rscript -e "devtools::install_deps(dependencies = TRUE, quiet = TRUE)"; \
  Rscript -e "install.packages('rlang', repos = 'https://cran.rstudio.com/')"
  