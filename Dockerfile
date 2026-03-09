FROM rocker/r-ver:4.4.1
RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssl-dev \
	libglpk40  \
	libxml2-dev
RUN R -e "install.packages(c('miniCRAN','withr','yaml','testthat'))"
RUN mkdir /miniCRAN
COPY ./script /script
CMD ["R", "-e", "\
  stack_file <- Sys.getenv('CRANDORE_STACK_FILE', ''); \
  if (nzchar(stack_file) && file.exists(stack_file)) { \
    source('/script/repos_snapshot.R'); source('/script/stack_runner.R'); \
  } else { \
    source('/script/repos_snapshot.R'); crandore(); \
  }"]
