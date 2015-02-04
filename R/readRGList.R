setMethodS3("readRGList", "AgilentDataFile", function(this, ..., verbose=FALSE) {
  use("limma")
  pathname <- getPathname(this)
  limma::read.maimages(pathname, source="agilent", ..., verbose=verbose)
})


setMethodS3("readRGList", "AgilentDataSet", function(this, ..., verbose=FALSE) {
  use("limma")
  pathnames <- getPathnames(this)
  limma::read.maimages(pathnames, source="agilent", ..., verbose=verbose)
})


############################################################################
# HISTORY:
# 2014-08-23
# o Created.
############################################################################
