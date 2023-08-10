FROM rocker/r-ver:4.3.1

RUN install2.r --error --skipinstalled \
  DT \
  remotes \
  && rm -rf /tmp/downloaded_packages

RUN installGithub.r \
  mrc-ide/orderly2 \
  MJomaba/MixDiff

RUN mkdir /analysis

CMD R -e "orderly2::orderly_run('sim_params')"