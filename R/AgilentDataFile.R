# \section{File format}{
# <qoute [3]>
#  A full version of the text files contains three sections namely 
#  FEPARAMS, STATS, and FEATURES.
#
#  FEPARAMS is the top most section containing the parameter and
#  option settings under which the feature extraction software runs.
#  Examples include the protocol name, version of the grid placement
#  algorithm, offset values, etc. 
# 
#  STATS is the middle section that provides statistical descriptions
#  of the results (usually on per channel or image basis). Examples 
#  include average number of saturated features per channel, standard
#  deviation of the data points measured per channel, number of features
#  that are flagged as population outliers, etc. 
# 
#  FEATURES is the section that concerns most users as it contains
#  values/descriptions of the results for each individual feature 
#  (probe). Some of the important data include the physical locations
#  of probes (features) on an array, intensity measurements, quality 
#  flags, and biological annotations.  
# </qoute>
# }
#
# \references{
#   [1] Agilent Feature Extraction Software.
#   [2] https://stat.ethz.ch/pipermail/bioconductor/2007-August/018753.html
#   [3] TCGA_2007aa
#   [4] ...
# }
setConstructorS3("AgilentDataFile", function(..., .verify=TRUE) {
  this <- extend(GenericTabularFile(..., .verify=FALSE), "AgilentDataFile",
    .fileHeader = NULL
  );

  if (.verify)
    verify(this, ..., verbose=verbose);
  this;
})


setMethodS3("as.character", "AgilentDataFile", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- NextMethod("as.character", this, ...);
  class <- class(s);
  s <- c(s, sprintf("Number of text lines: %d", nbrOfLines(this, fast=TRUE)));

  class(s) <- class;
  s;
})


setMethodS3("getExtensionPattern", "AgilentDataFile", function(static, ...) {
  "[.](txt)$";
}, static=TRUE)

setMethodS3("getPlatform", "AgilentDataFile", function(this, ...) {
  "Agilent";
})

setMethodS3("getChipType", "AgilentDataFile", function(this, ...) {
  path <- getPath(this);
  dirname <- basename(path);
  dirname;
})


setMethodS3("verify", "AgilentDataFile", function(this, ..., verbose=FALSE) {
  # Nothing to do?
  if (is.null(getPathname(this)))
    return(invisible(this));


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  return(this);

  verbose && enter(verbose, "Validating file contents");

  tryCatch({
    data <- readDataFrame(this, skip=this$skip, nrow=10, verbose=verbose);
  }, error = function(ex) {
    throw("File format error of the tabular file ('", getPathname(this), "'): ", ex$message);
  })

  verbose && exit(verbose);

  invisible(this);
}, private=TRUE)



setMethodS3("readColumnNames", "AgilentDataFile", function(this, ...) {
  as.logical(this$readColumnNames);
})


setMethodS3("hasColumnHeader", "AgilentDataFile", function(this, ...) {
  identical(this$readColumnNames, TRUE);
})


setMethodS3("getColumnNames", "AgilentDataFile", function(this, ..., translate=TRUE) {
  # Argument 'translate':
  translate <- Arguments$getLogical(translate);

  if (hasColumnHeader(this)) {
    colnames <- getHeader(this, ...)$columns;
    if (translate) {
      colnames <- translateColumnNames(this, colnames);
    }
  } else {
    colnames <- this$.columnNames;
  }
  colnames;
})



setMethodS3("getHeader", "AgilentDataFile", function(this, ..., force=FALSE) {
  hdr <- this$.fileHeader;
  if (force || is.null(hdr)) {
    hdr <- readRawHeader(this, ...);
    if (hasColumnHeader(this)) {
      hdr$columns <- hdr$topRows[[1]];
    }
    this$.fileHeader <- hdr;
  }
  hdr;
})


setMethodS3("countLines", "AgilentDataFile", function(this, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  pathname <- getPathname(this);

  cmd <- sprintf("gawk '{sum += 1}; END {print sum}' '%s'", pathname);
  verbose && cat(verbose, "Command:");
  verbose && print(verbose, cmd);
  con <- pipe(cmd);
  sum <- readLines(con);
  as.integer(sum);
})


setMethodS3("readColumnsFast", "AgilentDataFile", function(this, columns=1, skip=0, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'columns':
  columns <- Arguments$getIndices(columns);

  # Argument 'skip':
  skip <- Arguments$getInteger(skip, range=c(0,Inf));

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  pathname <- getPathname(this);

  columnsStr <- paste(sprintf("$%d", columns), collapse=", ");
  cmd <- sprintf("gawk '{print %s;}' '%s'", columnsStr, pathname);
  verbose && cat(verbose, "Command:");
  verbose && print(verbose, cmd);

  # Open connection
  con <- pipe(cmd, open="rt");
  on.exit({
    if (!is.null(con)) {
      close(con);
    }
  }, add=TRUE);

  data <- readLines(con, ..., warn=FALSE);
  if (skip > 0) {
    keep <- seq(from=skip+1L, to=length(data));
    data <- data[keep];
    rm(keep);
  }

  close(con);
  con <- NULL;

  data;
})


setMethodS3("readColumnFast", "AgilentDataFile", function(this, column=1, ...) {
  # Argument 'column':
  if (is.character(column)) {
    columnNames <- getColumnNames(this);
    column <- match(column, columnNames) + 1L;
  }
  column <- Arguments$getIndex(column);
  readColumnsFast(this, columns=column, ...);
})


setMethodS3("readFeaturesColumn", "AgilentDataFile", function(this, column=1, n=-1L, ...) {
  # Argument 'column':
  allColumnNames <- getColumnNames(this);
  if (is.character(column)) {
    columnName <- column;
    column <- match(columnName, allColumnNames);
  }
  column <- Arguments$getIndex(column);
  columnName <- allColumnNames[column];

  # Argument 'n':
  n <- Arguments$getInteger(n, range=c(-1,Inf));

  sections <- readSectionsIdxs(this);
  section <- sections[["FEATURES"]];
  skip <- section$dataIdxs[1]-1L;
  if (n >= 0) {
    n <- n + skip;
  }
  readColumnsFast(this, columns=column+1L, n=n, skip=skip, ...);
})


setMethodS3("readFeaturesColumns", "AgilentDataFile", function(this, columns, ..., verbose=FALSE) {
  # Argument 'columns':
  allColumnNames <- getColumnNames(this);
  if (is.character(columns)) {
    columnNames <- columns;
    columns <- match(columnNames, allColumnNames);
  }
  columns <- Arguments$getIndices(columns, max=length(allColumnNames));
  columnNames <- allColumnNames[columns];

  data <- list();
  for (kk in seq(along=columns)) {
     values <- readFeaturesColumn(this, column=columns[kk], ...);
     data[[kk]] <- values;
  } # for (kk ...)

  data <- as.data.frame(data, stringsAsFactors=FALSE);
  names(data) <- columnNames;

  data;
})



setMethodS3("readSectionsIdxs", "AgilentDataFile", function(this, ...) {
  rowType <- readColumnFast(this, column=1);
  typeIdxs <- whichVector(rowType == "TYPE");
  nbrOfSections <- length(typeIdxs);
  keyIdxs <- typeIdxs + 1L;
  keys <- rowType[keyIdxs];
  endIdxs <- whichVector(rowType == "*");
  if (length(endIdxs) < nbrOfSections) {
    endIdxs <- c(endIdxs, length(rowType)+1);
  }
  dataIdxs <- whichVector(rowType == "DATA");

  sections <- vector("list", nbrOfSections);
  names(sections) <- keys;
  for (kk in seq(along=sections)) {
    idxs <- (keyIdxs[kk]+1):(endIdxs[kk]-1);
    section <- list(
      typeIdx = typeIdxs[kk],
      keyIdx = keyIdxs[kk],
      dataIdxs = intersect(dataIdxs, idxs)
    );
    sections[[kk]] <- section;
  }

  sections;
})

setMethodS3("readSection", "AgilentDataFile", function(this, section, n=-1L, ...) {
  # Argument 'n':
  n <- Arguments$getInteger(n, range=c(-1,Inf));

  range <- range(unlist(section, use.names=FALSE));
  nFile <- range[2];
  if (n >= 0) {
    nFile <- (range[1]+1L) + n;
  }

  bfr <- readLines(this, n=nFile);

  rrs <- (range[1]:nFile);
  bfr <- bfr[rrs];
  bfr <- strsplit(bfr, split="\t");
  bfr <- lapply(bfr, FUN=function(x) x[-1]);

  types <- bfr[[1]];  
  names <- bfr[[2]];  

  nrow <- length(bfr)-2L;
  ncol <- length(types);
  
  modes <- types;
  modes[modes == "text"] <- "character";
  modes[modes == "float"] <- "double";

  data <- bfr[-(1:2)];
  data <- unlist(data, use.names=FALSE);

  data <- matrix(data, nrow=nrow, ncol=ncol, byrow=TRUE);

  resKK <- vector("list", ncol);
  names(resKK) <- names;
  for (cc in seq(length=ncol)) {
    mode <- modes[cc];
    values <- data[,cc,drop=TRUE];
    if (mode == "boolean") {
      values <- as.integer(values);
      values <- as.logical(values);
    } else {
      storage.mode(values) <- mode;
    }
    resKK[[cc]] <- values;
  }
  resKK <- as.data.frame(resKK, stringsAsFactors=FALSE);

  resKK;
})

setMethodS3("readSections", "AgilentDataFile", function(this, sections, ...) {
  res <- vector("list", length(sections));
  names(res) <- names(sections);
  for (kk in seq(along=sections)) {
    section <- sections[[kk]];
    res[[kk]] <- readSection(this, section=section, verbose=verbose);
  } # for (kk ...)
  res;  
})

setMethodS3("readRawHeader", "AgilentDataFile", function(this, ...) {
  sections <- readSectionsIdxs(this, ...);
  knownHeaderFields <- c("FEPARAMS", "STATS");
  keep <- intersect(names(sections), knownHeaderFields);
  sections <- sections[keep];
  readSections(this, sections=sections, ...);
})

setMethodS3("getHeader", "AgilentDataFile", function(this, ..., force=FALSE) {
  hdr <- this$.hdr;
  if (force || is.null(hdr)) {
    hdr <- readRawHeader(this, ...);
    this$.hdr <- hdr;
  }
  hdr;
})

setMethodS3("getDimension", "AgilentDataFile", function(this, ..., force=FALSE) {
  dim <- this$.dimension;
  if (force || is.null(dim)) {
    hdr <- getHeader(this, ..., force=force);
    hdr <- hdr[["FEPARAMS"]];
    hdr <- hdr[c("Grid_NumRows", "Grid_NumCols")];
    dim <- unlist(hdr, use.names=FALSE);
    this$.dimension <- dim;
  }
  dim;
})


setMethodS3("getScanDate", "AgilentDataFile", function(this, ..., force=FALSE) {
  scanDate <- this$.scanDate;
  if (force || is.null(dim)) {
    hdr <- getHeader(this, ..., force=force);
    hdr <- hdr[["FEPARAMS"]];
    scanDate <- hdr[["Scan_Date"]];
    this$.scanDate <- scanDate;
  }
  scanDate;
})

setMethodS3("getBarcode", "AgilentDataFile", function(this, ..., force=FALSE) {
  barcode <- this$.barcode;
  if (force || is.null(dim)) {
    hdr <- getHeader(this, ..., force=force);
    hdr <- hdr[["FEPARAMS"]];
    barcode <- hdr[["FeatureExtractor_Barcode"]];
    this$.barcode <- barcode;
  }
  barcode;
})


setMethodS3("readFeatures", "AgilentDataFile", function(this, ...) {
  sections <- readSectionsIdxs(this, ...);
  section <- sections[["FEATURES"]];
  readSection(this, section=section, ...);
})


setMethodS3("getColumnNames", "AgilentDataFile", function(this, ..., force=FALSE) {
  colnames <- this$.colnames;
  if (force || is.null(colnames)) {
    data <- readFeatures(this, n=1, ...);
    colnames <- colnames(data);
    this$.colnames <- colnames;
  }
  colnames;
})



setMethodS3("readLines", "AgilentDataFile", function(con, ...) {
  # To please R CMD check
  this <- con;
  pathname <- getPathname(this);
  readLines(pathname, ...);
})


setMethodS3("nbrOfRows", "AgilentDataFile", function(this, ...) {
  nbrOfLines(this, ...);
})


setMethodS3("nbrOfLines", "AgilentDataFile", function(this, ..., force=FALSE) {
  nbrOfLines <- this$.nbrOfLines;
  if (force || is.null(nbrOfLines)) {
    nbrOfLines <- countLines(this, ...);
    this$.nbrOfLines <- nbrOfLines;
  }
  nbrOfLines;
})



setMethodS3("exportCopyNumbers", "AgilentDataFile", function(this, dataSet, unf, ..., rootPath=c("rawCnData", "cnData"), force=FALSE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'dataSet':
  dataSet <- Arguments$getCharacter(dataSet);

  # Argument 'unf':
  unf <- Arguments$getInstanceOf(unf, "UnitNamesFile");

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

  chipType <- getChipType(unf, fullname=FALSE);
  
  path <- file.path(rootPath, dataSet, chipType);
  path <- Arguments$getWritablePath(path);
  verbose && cat(verbose, "Data set path: ", path);

  # Tags added to each exported file data file
  tags <- c("logRatio", "total");

  # Unit indices (to be inferred)
  units <- NULL;

  fullname <- getFullName(this);
  fullname <- paste(c(fullname, tags), collapse=",");
  filename <- sprintf("%s.asb", fullname);
  pathname <- Arguments$getWritablePathname(filename, path=path, mustNotExist=FALSE);

  # Nothing to do?
  if (force || !isFile(pathname)) {
    # Export to a temporary file
    pathnameT <- sprintf("%s.tmp", pathname);
    if (isFile(pathnameT)) {
      throw("Temporary file already exists: ", pathnameT);
    }
  
    # Map unit indices
    if (is.null(units)) {
      verbose && enter(verbose, "Identifying unit indices");
      unitNames <- readFeaturesColumn(this, "ProbeName");
      units <- indexOf(unf, names=unitNames);
      keep <- whichVector(is.finite(units));
      unitNames <- unitNames[keep];
      units <- units[keep];
      verbose && cat(verbose, "Unit names:");
      verbose && str(verbose, unitNames);
      verbose && cat(verbose, "Units:");
      verbose && str(verbose, units);
      verbose && exit(verbose);
    }
  
    # Read data
    verbose && enter(verbose, "Reading column data");
    columnName <- "LogRatio";
    data <- readFeaturesColumns(this, columns=columnName);
    data <- lapply(data, FUN=function(x) x[keep]);
    data <- lapply(data, FUN=as.double);
    data <- as.data.frame(data);
    verbose && exit(verbose);
  
    # Allocate output data
    verbose && enter(verbose, "Generating 'srcFile' footer");
    srcFile <- list(
      filename = getFilename(this),
      filesize = getFileSize(this),
      checksum = getChecksum(this),
      columnName = columnName,
      valuesChecksum = digest(data)
    );
    verbose && str(verbose, srcFile);
    verbose && exit(verbose);
  
    on.exit({
      if (!is.null(pathnameT) && isFile(pathnameT)) {
        file.remove(pathnameT);
      }
    }, add=TRUE);
  
    verbose && enter(verbose, "Allocating temporary file");
    df <- AromaUnitTotalCnBinaryFile$allocateFromUnitNamesFile(unf, 
                                            filename=pathnameT, path=NULL);
    footer <- readFooter(df);
    footer$srcFile <- srcFile;
    writeFooter(df, footer);
    verbose && exit(verbose);
  
    verbose && enter(verbose, "Writing signals");
    for (cc in seq(along=data)) {
      df[units,cc] <- data[[cc]];
    }
    verbose && exit(verbose);
  
    verbose && enter(verbose, "Renaming temporary file");
    # Rename temporary file
    file.rename(pathnameT, pathname);
    if (isFile(pathnameT) || !isFile(pathname)) {
      throw("Failed to rename temporary file: ", pathnameT, " -> ", pathname);
    }
    pathnameT <- NULL;
    verbose && exit(verbose);
  } else {
    verbose && cat(verbose, "File already exported. Skipping.");
  }

  # Validate
  df <- AromaUnitTotalCnBinaryFile(pathname);
  verbose && cat(verbose, "Exported data file:");
  verbose && print(verbose, df);

  verbose && exit(verbose);

  invisible(df);
}) # exportCopyNumbers()


############################################################################
# HISTORY:
# 2009-11-09
# o Added exportCopyNumbers() for AgilentDataFile.
# 2009-11-07
# o Created from TabularTextFile.R.
############################################################################
