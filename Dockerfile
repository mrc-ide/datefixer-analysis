FROM rocker/r-ver:4.3.1

RUN install2.r --error \
  DT \
  remotes

RUN installGithub.r \
  mrc-ide/orderly2