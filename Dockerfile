FROM rocker/r-ver:4.3.1

RUN install2.r --error --skipinstalled \
  ggplot2 \
  purrr \
  remotes \  
  tidyr \
  && rm -rf /tmp/downloaded_packages

RUN installGithub.r \
  mrc-ide/orderly2 \
  MJomaba/MixDiff

RUN mkdir /analysis
WORKDIR /analysis
CMD R -e "orderly2::orderly_run('sim_params')"
CMD R -e "orderly2::orderly_run('sim_data_baseline')"
CMD R -e "orderly2::orderly_run('sim_data_eda')"