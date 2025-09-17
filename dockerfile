FROM rocker/tidyverse:4.5.0

RUN R -e "install.packages('devtools', repos='https://cran.rstudio.com/')"
WORKDIR /pkg
COPY . /pkg/

RUN R -e "devtools::install_deps('/pkg', dependencies = TRUE)"
RUN R -e "devtools::install('/pkg')"

RUN R -e "remotes::install_version('vdiffr', version='1.0.8', repos='https://cran.rstudio.com/', upgrade='never')" && \
    R -e "remotes::install_version('svglite', version='2.2.1', repos='https://cran.rstudio.com/', upgrade='never')" && \
    R -e "remotes::install_version('fontquiver', version='0.2.1', repos='https://cran.rstudio.com/', upgrade='never')"

ARG GGPLOT2_VERSION=""
RUN if [ ! -z "$GGPLOT2_VERSION" ]; then \
      R -e "remotes::install_version('ggplot2', version='$GGPLOT2_VERSION', repos='https://cran.rstudio.com/', upgrade='never')"; \
    fi

ENV RunVisualTests=yes
ENV _R_CHECK_TESTS_NLINES_=0

CMD ["R", "-e", "setwd('/pkg'); devtools::test(stop_on_failure = TRUE)"]
