setConstructorS3("AgilentDataSet", function(...) {
  extend(GenericTabularFileSet(...), "AgilentDataSet");
})


setMethodS3("byPath", "AgilentDataSet", function(static, ..., pattern=NULL, fileClass=getFileClass(static)) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'fileClass':
  clazz <- Class$forName(fileClass);
  dfStatic <- getStaticInstance(clazz);
  if (!inherits(dfStatic, getFileClass(static))) {
    throw("Argument 'fileClass' is not refering to an ", getFileClass(static),
                           " class: ", paste(class(dfStatic), collapse=", "));
  }

  # Argument 'pattern':
  if (!is.null(pattern)) {
    pattern <- Arguments$getRegularExpression(pattern);
  }

  # Default filename extension pattern
  if (is.null(pattern)) {
    pattern <- getExtensionPattern(dfStatic);
  }

  byPath.GenericDataFileSet(static, ..., pattern=pattern, fileClass=fileClass);
}, static=TRUE);


setMethodS3("getPlatform", "AgilentDataSet", function(this, ...) {
  "Agilent";
})

setMethodS3("getChipType", "AgilentDataSet", function(this, ...) {
  df <- getFile(this, 1);
  getChipType(df);
})


############################################################################
# HISTORY:
# 2009-11-09
# o Created.
############################################################################
