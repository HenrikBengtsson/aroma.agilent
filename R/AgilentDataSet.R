setConstructorS3("AgilentDataSet", function(...) {
  extend(GenericTabularFileSet(...), c("AgilentDataSet", uses("AromaPlatformInterface")))
})


setMethodS3("byPath", "AgilentDataSet", function(static, ..., pattern=NULL, fileClass=getFileClass(static)) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'fileClass':
  clazz <- Class$forName(fileClass);
  dfStatic <- getStaticInstance(clazz);
  dfStatic <- Arguments$getInstanceOf(dfStatic, getFileClass(static), .name="fileClass");

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


setMethodS3("as.character", "AgilentDataSet", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- sprintf("%s:", class(this)[1]);
  s <- c(s, sprintf("Name: %s", getName(this)));
  tags <- getTags(this);
  tags <- paste(tags, collapse=",");
  s <- c(s, sprintf("Tags: %s", tags));
  s <- c(s, sprintf("Path: %s", getPath(this)));
  s <- c(s, sprintf("Platform: %s", getPlatform(this)));
  s <- c(s, sprintf("Chip type: %s", getChipType(this)));
  n <- length(this);
  s <- c(s, sprintf("Number of arrays: %d", n));
  names <- getNames(this);
  s <- c(s, sprintf("Names: %s [%d]", hpaste(names), n));

  # Get header timestamps?
  maxCount <- getOption(aromaSettings, "output/timestampsThreshold", default=100L);
  if (n == 0) {
    s <- c(s, "Time period: NA");
  } else if (maxCount >= n) {
    ts <- getScanDates(this);
    # Note: If ts <- range(ts) is used and the different timestamps uses
    # tifferent 'tzone' attributes, e.g. if some files where scanning during
    # daylight savings time and some not, we will get a warning saying:
    # "'tzone' attributes are inconsistent".  By doing the below, we avoid
    # this warning (which confuses users).
    ts <- sort(ts);
    ts <- ts[c(1,n)];
    ts <- format(ts, "%Y-%m-%d %H:%M:%S");  # range() gives strange values?!?
    s <- c(s, sprintf("Time period: %s -- %s", ts[1], ts[2]));
  } else {
    s <- c(s, sprintf("Time period: [not reported if more than %.0f arrays]", as.double(maxCount)));
  }
  s <- c(s, sprintf("Total file size: %.2fMB", getFileSize(this)/1024^2));
  s <- c(s, sprintf("RAM: %.2fMB", objectSize(this)/1024^2));
  GenericSummary(s);
}, protected=TRUE)



setMethodS3("getPlatform", "AgilentDataSet", function(this, ...) {
  "Agilent";
})

setMethodS3("getChipType", "AgilentDataSet", function(this, ...) {
  df <- this[[1]];
  getChipType(df);
})


setMethodS3("getScanDates", "AgilentDataSet", function(this, ...) {
  res <- lapply(this, FUN=getScanDate, ...)
  res <- do.call("c", args=res);
  res
})


setMethodS3("exportCopyNumbers", "AgilentDataSet", function(this, ..., rootPath=c("rawCnData", "cnData"), force=FALSE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

  unf <- getUnitNamesFile(this);
  chipType <- getChipType(unf, fullname=FALSE);
  verbose && cat(verbose, "Chip type:", chipType);

  path <- file.path(rootPath, dataSet, chipType);
  path <- Arguments$getWritablePath(path);
  verbose && cat(verbose, "Output path:", path);

  for (ii in seq(this)) {
    df <- this[[ii]];
    verbose && enter(verbose, sprintf("Array #%d ('%s') of %d",
                              ii, getName(df), nbrOfFiles(this)));

    dfOut <- exportCopyNumbers(df, dataSet=dataSet, rootPath=rootPath,
                               force=force, verbose=less(verbose,1));

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
# 2015-04-01
# o Added getScanDates() and as.character().
# 2009-11-10
# o Added exportCopyNumbers() to AgilentDataSet.
# 2009-11-09
# o Created.
############################################################################
