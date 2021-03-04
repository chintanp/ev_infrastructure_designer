FROM rocker/r-ver:4.0.2

RUN mkdir /build_zone
RUN mkdir /build_zone/logs
ADD . /build_zone
WORKDIR /build_zone

ENV GITHUB_PAT ''
RUN R -e 'Sys.getenv("GITHUB_PAT")'
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libpq-dev libssh2-1-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("sp",upgrade="never", version = "1.4-2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-5")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_version("lgr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("leafem",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.4.3")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("auth0",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("rgdal",upgrade="never", version = "1.5-16")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_github("r-lib/scales@bb1c423004ec2e951ff77253e784f4c23af5a5e3")'
RUN Rscript -e 'remotes::install_github("rstudio/shiny@753400144d5e47315f059dcbffa5963b2b1051c5")'
RUN Rscript -e 'remotes::install_github("rstudio/leaflet@8db80b6078ffb085d9a96fa41af8dd8b73d1df6a")'
RUN Rscript -e 'remotes::install_github("rstudio/leaflet.mapboxgl@dec90d124daaf5af814584eecfba505b3882e4d8")'
RUN Rscript -e 'remotes::install_github("RinteRface/bs4Dash@2f3aedec89c38e2dd876f02f3cb11cb9c1745eac")'
RUN Rscript -e 'remotes::install_github("dreamRs/shinyWidgets@330550cf7c53beb2ae51be15f4e1eccc1b7a50cf")'
RUN Rscript -e 'remotes::install_github("rstudio/DT@55a10b0d189ed252c5299a29951e666c4bed92b2")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@8707ddea4e4121a92ca26d2b0d5f596869a262fa")'

RUN R -e 'remotes::install_local(upgrade="never")'
# RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');evides::run_app()"
