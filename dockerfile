FROM rocker/tidyverse:4

ADD . /home/ggExtra

WORKDIR /home/ggExtra

ENV RunVisualTests=yes

RUN apt-get update && \
  apt-get install -y --no-install-recommends libfreetype6 && \
  Rscript -e "devtools::install_deps(dependencies = TRUE, quiet = TRUE)" && \
  echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site && \
  Rscript -e "install.packages('rlang')"
