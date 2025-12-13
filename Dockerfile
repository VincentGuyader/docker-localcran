FROM rocker/r-base:4.4.1
RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssl-dev \
	libxml2-dev
RUN R -e "install.packages('miniCRAN',repos='http://cloud.r-project.org')"	
#RUN install2.r miniCRAN
RUN mkdir /miniCRAN
COPY ./script /script
CMD ["Rscript", "/script/repos_snapshot.R"]
