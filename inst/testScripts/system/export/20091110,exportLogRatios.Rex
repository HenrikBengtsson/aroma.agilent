if (interactive()) savehistory();
library("aroma.agilent");
library("aroma.cn");
sourceDirectory("aroma.agilent/R/", modifiedOnly=TRUE);

dataSet <- "HsuH_20091107";
chipType <- "HG-CGH-185K";

unf <- TextUnitNamesFile$byChipType(chipType);
print(unf);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
rootPath <- "rawData";
path <- file.path(rootPath, dataSet, chipType);
dsR <- AgilentDataSet$byPath(path);
print(dsR);

ds <- exportCopyNumbers(dsR, unf=unf, verbose=verbose);
verbose && print(verbose, ds);

ugp <- getAromaUgpFile(unf);
print(ugp);

stop();

xy <- readFeaturesColumns(df, columns=c("Row", "Col"));
xy <- lapply(xy, FUN=as.integer);
xy <- as.data.frame(xy);
xy <- as.matrix(xy);

signal <- "ProcessedSignal";
#signal <- "BGSubSignal";
signals <- paste(c("g", "r"), signal, sep="");

signals <- "LogRatio";
data <- readFeaturesColumns(df, columns=signals);
data <- lapply(data, FUN=function(x) x[keep]);
data <- lapply(data, FUN=as.double);
data <- as.data.frame(data);
X <- as.matrix(data);
plotMvsA(X, pch=".");

C <- 2*X[,1]/X[,2];
pos <- getPositions(ugp);

z <- C;
z <- z/max(z,na.rm=TRUE);
col <- gray(z);

Clim <- c(0,8);
xScale <- 1e-6;

par(ask=TRUE);
for (chr in 1:24) {
  units <- getUnitsOnChromosome(ugp, chr);
  cn <- RawCopyNumbers(x=pos[units], cn=C[units], chromosome=chr);
  mu <- median(getSignals(cn), na.rm=TRUE);
  print(mu);
  plot(cn, cex=0.5, col="#999999", ylim=Clim, xScale=xScale);
  stext(side=3, pos=1, sprintf("Chr %2d", chr));
  stext(side=4, pos=1, sprintf("mu=%.2f", mu));
  cnS <- binnedSmoothing(cn, by=1e6);
  points(cnS, xScale=xScale);
  cnR <- segmentByCBS(cn);
  segs <- extractCopyNumberRegions(cnR);
  drawLevels(segs, xScale=xScale);
} # for (chr ...)

