FROM rocker/r-ver:4.4.1
RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssl-dev \
	libglpk40  \
	libxml2-dev
RUN R -e "install.packages(c('miniCRAN','withr'))"	
RUN mkdir /miniCRAN
COPY ./script /script
CMD ["R","-e", "source('/script/repos_snapshot.R');crandore()"]
