##FROM debian:testing
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
    	gdebi-core \
		locales \
		ca-certificates \
		vim-tiny \
		less \
		wget \
		r-base \
		r-base-dev \
		r-recommended \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV R_BASE_VERSION 3.2.2

# Need this to add R repo
RUN apt-get update && apt-get install -y software-properties-common

# Add R apt repository
RUN add-apt-repository "deb http://cran.rstudio.com/bin/linux/ubuntu $(lsb_release -cs)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
    
CMD ["R"]
    
# Download and install libssl 
# http://launchpadlibrarian.net/244105462/libssl-dev_1.0.1f-1ubuntu2.18_amd64.deb
 RUN wget --no-verbose http://launchpadlibrarian.net/244105462/libssl-dev_1.0.1f-1ubuntu2.18_amd64.deb && \
    dpkg -i libssl-dev_1.0.1f-1ubuntu2.18_amd64.deb && \
    rm -f libssl-dev_1.0.1f-1ubuntu2.18_amd64.deb
   
# Download and install shiny server 

RUN wget --no-verbose https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb && \
	gdebi shiny-server-1.4.2.786-amd64.deb
    
RUN R -e "install.packages(c('shiny', 'rmarkdown'), dependencies = TRUE, repos='https://cran.rstudio.com/')"

RUN sudo su - -c "R -e \"install.packages(c('caret', 'dplyr', 'gplots'), dependencies = TRUE, repos='http://cran.rstudio.com/')\"" 

RUN sudo su - -c "R -e \"install.packages(c('rattle'), dependencies = TRUE, repos='http://rattle.togaware.com')\"" 

EXPOSE 3838 

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
