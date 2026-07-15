version_check <- function(package, version) {
  if (packageVersion(package) < version) {
    stop(sprintf(
      paste("Please upgrade %s to at least %s"),
      package, version))
  }
}
