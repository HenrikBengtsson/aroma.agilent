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


setMethodS3("exportCopyNumbers", "AgilentDataSet", function(this, unf, ..., rootPath=c("rawCnData", "cnData"), force=FALSE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'unf':
  if (!inherits(unf, "UnitNamesFile")) {
    throw("Argument 'unf' is not a UnitNamesFile: ", class(unf)[1]);
  }

  # Argument 'rootPath':
  if (length(rootPath) > 1) {
    rootPath <- match.arg(rootPath);
  }
  rootPath <- Arguments$getWritablePath(rootPath);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Exporting ", class(this)[1]);
  dataSet <- getFullName(this);
  verbose && cat(verbose, "Input data set:", dataSet);

  tags <- c("AFE", "LogRatio");
  dataSet <- paste(c(dataSet, tags), collapse=",");
  verbose && cat(verbose, "Output data set:", dataSet);

  chipType <- getChipType(unf, fullname=FALSE);
  verbose && cat(verbose, "Chip type:", chipType);

  path <- file.path(rootPath, dataSet, chipType);
  path <- Arguments$getWritablePath(path);
  verbose && cat(verbose, "Output path:", path);

  for (ii in seq(this)) {
    df <- getFile(this, ii);
    verbose && enter(verbose, sprintf("Array #%d ('%s') of %d", 
                              ii, getName(df), nbrOfFiles(this)));

    dfOut <- exportCopyNumbers(df, dataSet=dataSet, unf=unf, 
                               rootPath=rootPath, verbose=less(verbose,1));

    verbose && cat(verbose, "Output file:");
    verbose && print(verbose, dfOut);

    verbose && exit(verbose);
  } # for (ii ...)

  ds <- AromaUnitTotalCnBinarySet$byPath(path);
  verbose && cat(verbose, "Output data set:");
  verbose && print(verbose, ds);

  verbose && exit(verbose);

  invisible(ds);
}) # exportCopyNumbers()


############################################################################
# HISTORY:
# 2009-11-10
# o Added exportCopyNumbers() to AgilentDataSet.
# 2009-11-09
# o Created.
############################################################################
