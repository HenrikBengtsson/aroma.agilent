if (interactive()) savehistory();
library("aroma.agilent");
library("aroma.core");
source("aroma.agilent/R/AgilentDataFile.R");

chipType <- "HG-CGH-185K";
path <- file.path("annotationData", "chipTypes", chipType);
path <- Arguments$getWritablePath(path);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
filename <- "US45102938_252206012655_S01_CGH_105_Dec08_1_2.txt";
pathname <- Arguments$getReadablePathname(filename);

df <- AgilentDataFile(pathname);
#print(df);

data <- readFeatures(df);
str(data);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract (unitName, genomePosition)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dd <- data[,c("ProbeName", "SystematicName")];
colnames(dd)[1] <- "unitName";
colnames(dd)[2] <- "location";

# Drop all non-unique probes
#isDuplicated <- duplicated(dd$unitName);
#dups <- unique(dd$unitName[isDuplicated]);
#keep <- !is.element(dd$unitName, dups);
#dd <- dd[keep,,drop=FALSE];

unitNames <- dd$unitName;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Write unit names file
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
tags <- "HB20091107";
fullname <- paste(c(chipType, tags), collapse=",");
filename <- sprintf("%s,unitNames.txt", fullname);
pathname <- Arguments$getWritablePathname(filename, path=path, mustNotExist=TRUE);

dateTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z");
createdBy <- "Henrik Bengtsson, hb@stat.berkeley.edu";

platform <- "Agilent";
bfr <- NULL;
bfr <- c(bfr, sprintf("# platform: %s", platform));
bfr <- c(bfr, sprintf("# chipType: %s", chipType));
bfr <- c(bfr, sprintf("# createdBy: %s", createdBy));
bfr <- c(bfr, sprintf("# createdOn: %s", dateTime));
bfr <- c(bfr, sprintf("# nbrOfUnits: %d", length(unitNames)));
bfr <- c(bfr, sprintf("# dataChecksum: %s", digest(unitNames)));
bfr <- c(bfr, sprintf("# columnNames: %s", "unitName"));
bfr <- c(bfr, sprintf("# columnClasses: %s", "character"));
bfr <- c(bfr, "unitName");
bfr <- c(bfr, unitNames);

writeLines(con=pathname, bfr, sep="\n");

unf <- TextUnitNamesFile$byChipType(chipType, tags=tags);
print(unf);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract (chromosome, start, stop) positions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
pattern <- "^chr([0-9A-Z]{1,2}):([0-9]+)-([0-9]+)$";
chr <- gsub(pattern, "\\1", dd$location);
chr[chr == "X"] <- "23";
chr[chr == "Y"] <- "24";
chr[chr == "M"] <- "25";
chr <- as.integer(chr);
start <- as.integer(gsub(pattern, "\\2", dd$location));
stop <- as.integer(gsub(pattern, "\\3", dd$location));
length <- stop-start+1L;
# Sanity check
length <- Arguments$getIntegers(length, range=c(10,100), disallow=NULL);
ugpData <- data.frame(chr=chr, start=start, stop=stop, length=length);
print(summary(ugpData));
print(table(ugpData$chr));
print(table(ugpData$length));


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Write UGP file
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
tags <- "HB20091107";
ugp <- AromaUgpFile$allocateFromUnitNamesFile(unf, tags=tags);
print(ugp);
ugp[,1] <- ugpData$chr;
ugp[,2] <- ugpData$start;

ugp <- getAromaUgpFile(unf);
print(ugp);
