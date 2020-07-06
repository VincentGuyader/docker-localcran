FROM rocker/r-base
RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssl-dev \
	libxml2-dev
RUN install2.r miniCRAN
RUN mkdir /miniCRAN
COPY ./script /script
CMD ["r", "/script/repos_snapshot.R"]

