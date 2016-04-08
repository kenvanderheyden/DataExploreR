FROM ubuntu:14.04

MAINTAINER Ken "ken@mindstorms.be" 

## Set a default user. Available via runtime flag `--user docker` 
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly). 
RUN useradd docker \
	&& mkdir /home/docker \
	&& chown docker:docker /home/docker \
	&& addgroup docker staff

RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends \
	    sudo \
		locales \
		ca-certificates \
		wget \
		gdebi \
		software-properties-common \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV R_BASE_VERSION 3.2.2

RUN add-apt-repository "deb http://cran.rstudio.com/bin/linux/ubuntu $(lsb_release -cs)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \ 
	&& apt-get update \
	&& apt-get install -y --no-install-recommends \
		r-base=${R_BASE_VERSION}* \
		r-base-dev=${R_BASE_VERSION}* \
		r-recommended=${R_BASE_VERSION}*
   
# Download and install shiny server 
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb && \
	gdebi shiny-server-1.4.2.786-amd64.deb && \
	rm -f shiny-server-1.4.2.786-amd64.deb
    
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'caret', 'dplyr'), dependencies = TRUE, repos='https://cran.rstudio.com/')"

RUN R -e "install.packages(c('rattle'), dependencies = TRUE, repos='http://rattle.togaware.com/')"

EXPOSE 3838 

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
