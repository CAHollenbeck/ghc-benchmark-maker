library(dplyr)
library(ggplot2)


# plot_separate :: DataFrame -> String -> String -> plots a graph... 
#
# Takes a DF and plots a graph of distinctly colored lines for each variable in SEPARATE,
# with the Y-axis as YVAR.
#
# df : a dataframe
# YVAR : The Y-axis for the graph
# SEPARATE : The column name of the value to be graphed with different colors
#
# TODO : This needs to be redone. The data frame format has changed, and this can be 
# simplified now that I know I'll only ever be graphing by project.
plot_separate <- function(df, YVAR, SEPARATE, my_ylim=0) {
  df$PROJECT <- as.factor(df$PROJECT) # Must be formatted as factor
  df$P <- as.numeric(df[[SEPARATE]]) # Encode the thing to be separated as numeric in a new column
  #ymax = max(df$YVAR) + 30 # Upper limit of the y axis for graphing
  if (my_ylim == 0 ) {
    my_ylim <- max(df[YVAR])
  }
  y_range <- c(0, my_ylim) # Range of y axis limits for graph

  # Get the list of projects in order by their factor number
  projname_inorder <- unique(df[c(SEPARATE, "P")])
  projname_inorder <- projname_inorder[order(projname_inorder$P),]
  projname_inorder <- projname_inorder[[SEPARATE]]
  
  # Grab a subset for one value of P, just to get an X axis range
  justone <- subset(df, df$P==unique(df$P)[1])
  run_range <- c(1, nrow(justone))
  
  par(mar=c(9.1, 4.1, 1.1, 2.1), xpd=TRUE)
  plot(run_range,
       y_range,
       type="n",
       xlab="Configuration",
       ylab="Test time",
       main="Programs' average test times per iteration") # Empty plot across x, y ranges
  
  # Convert projects to numeric. Use these numbers
  # to generate colors for the line chart.
  n_df_colors <- max(df$P) # Get number of colors
  df_list_colors <- rainbow(n_df_colors) # generate colors
  
  # Add the name of the colors to the data frame to attach them to their projects
  df$COLOR <- df_list_colors[df$P] # There's probably a better way to do this.

  Ps <- unique(df$P)
  
  # Iterate over every segment
  for (i in Ps) {
    set_ <- subset(df, P==i)
    p_color <- unique(set_$COLOR)
    lines(c(1:nrow(set_)), set_[[YVAR]], type="b", lwd=1.5,
          lty=0,
          col=p_color)
  }

  p_and_color <- df %>% group_by(PROJECT) %>% distinct(P, .keep_all = TRUE) 

  legend(0.5, 0, legend=p_and_color[[SEPARATE]], cex=0.8, col=p_and_color$COLOR,
         lty=1, title=SEPARATE)
}

#############################################################
#
#                 FUNCTIONS TO BUILD THE DATA FRAMES
#
#############################################################

# sanity_expect :: Val -> Val -> String -> Error?
#
# Checks whether the two given values are equal. If not,
# throw an error printing the given message.
sanity_expect <- function(expected, actual, msg) {
  if (expected != actual) {
    stop(paste(msg, paste("\nexpected:", expected, "returned:", actual)))
  }
}

# clean :: DataFrame -> DataFrame
#
# Get the desired time type for each program, averages across all runs.
clean <- function(df) {
  tmp <- df
  # Don't discard real time. That's the only one we want.
  tmp <- tmp[!tmp$TYPE_TIME=="sys",] # Discard sys time
  tmp <- tmp[!tmp$TYPE_TIME=="user",] # Discard user time
  #tmp <- tmp[!tmp$TYPE_TIME=="real",] # Discard real time # uncomment for debug
  
  # TODO remove these after refactoring the timer to not include them
  tmp$KF <- NULL
  tmp$CT <- NULL
  
  tmp <- tmp[!tmp$RUN==1,] # Drop the first run; build time may be conflated with test time
  # This isn't necessary in later runs of the timer, where it automatically
  # drops the first run. Do it anyway for now. TODO.
  
  return(tmp)
}

# getProjConfigAvg :: DataFrame -> DataFrame
#
# Average together all of the run times in each configuration.
# Store this average in the data frame.
# No need to group by date, assuming batches are already grouped
# by date.
getProjConfigAvg <- function(df) {
  tmp <- df %>% dplyr::group_by(PROJECT, CONFIG) %>% mutate(PROJ_CONFIG_AVG=mean(TIME))
  return(tmp)
}

# getDistinctConfigAvg :: DataFrame -> DataFrame
#
# Remove rows with duplicated averages for each configuration.
# Hereafter, it won't be necessary to retain times for each individual run
# in each configuration.
getDistinctConfigAvg <- function(df) {
  tmp <- df
  tmp <- tmp %>% distinct(PROJECT, CONFIG, PROJ_CONFIG_AVG, .keep_all=TRUE)
  return(tmp)
}

# getRSpeedup :: Num -> Num -> Num
#
# Ratio of old/new time
getRSpeedup <- function(old, new) {
  rtrS <- old/new # Don't round
  return(rtrS)  
}

# gmMean :: Vec(Num) -> Num
#
# Calculate the geometric mean of the best observed improvements for 
# each project (oracle):
gmMean <- function(x, na.rm=TRUE){
  nozeros <- x[x > 0] # Remove zeros from geom mean
  # geom mean cannot handle zeros, and they also mean 0% improvement or no change,
  # so they provide no important information.
  if (length(nozeros) != length(x)) {
    # This shouldn't happen because times are never exactly zero,
    # but error if it does happen.
    #stop("Speedup lengths of different value after removal of zeros")
  }
  exp(sum(log(nozeros), na.rm=na.rm) / length(nozeros))
}

# # Test convert percentage to time ratio:
# # old/new
# r1 <- getRunTimeRatio(50)
# if(r1 != .5) {
#   stop(paste("Run time ratio", r1, "should be .5"))
# }
# r2 <- getRunTimeRatio(-100)
# if(r2 != 2) {
#   stop(paste("Run time ratio", r2,"should be 2"))
# }

## Test geometric mean
#gmT <- gmMean(c(r1, r2))
#if(gmT != 1) {
#  stop("Geom mean", gmT, "should be 1")
#}

# process :: DataFrame -> DataFrame
#
# Runs the unprocessed DF through cleaning and calculation of
# average times.
process <- function(df) {
  tmp <- df %>% clean() %>%
    getProjConfigAvg() %>%
    getDistinctConfigAvg()
  return(tmp)
}

#############################################################
#
#                 ADDITIONAL FUNCTIONS
#
#############################################################




# getWholeConfigRTR :: DataFrame -> DataFrame
#
# Get the geometric mean of runtime ratios across unique configurations
# in each configuration.
getWholeConfigRTR <- function(df) {
  tmp <- df %>% dplyr::group_by(CONFIG, DATE) %>% mutate(CONFIG_RTR=gmMean(RTR)) # Geometric mean for ratios
  return(tmp)
}

# getWholeConfigSpeedup :: DataFrame -> DataFrame
#
# Get the geometric mean of all the run time ratios across the projects
# in each configuration.
getWholeConfigRSpeedup <- function(df) {
  tmp <- df %>% dplyr::group_by(CONFIG, DATE) %>% mutate(CONFIG_RSPEEDUP=gmMean(RSpeedup)) # Geometric mean for ratios
  return(tmp)
}



# TODO:
# These are the types and column titles for the data.
# There are two separate versions because after December 2020, I took out KF and CT
# because they were never changing and never being used. I hope to collect enough
# complete data (for all 10 projects) to discard the data collected before 2021.
# Then it will all be consistent.
col_names4 = c("PROJECT",
               "TYPE_TIME",
               "TIME",
               "CONFIG",
               "RUN",
               "FI",
               "AA",
               "NTA",
               "DAC",
               "RAC",
               "RC",
               "AC",
               "FFD",
               "FDD",
               "CB",
               "CA",
               "BA",
               "CnS",
               "LS",
               "UI",
               'temp1', 
               'temp2',
               'temp3', 
               'temp4',
               'temp5',
               'temp6',
               'temp7',
               'temp8',
               'temp9',
               'temp10')

classes4 = c('factor', # project
             'factor',    # time type
             'numeric',   # run time
             'factor',    # configuration
             'factor',    # run
             'numeric',   # FI
             'numeric',   # AA
             'numeric',   # NTA
             'numeric',   # DAC
             'numeric',   # RAC
             'numeric',   # RC
             'numeric',   # AC
             'numeric',   # FFD
             'numeric',   # FDD
             'numeric',   # CB
             'numeric',   # CA
             'numeric',   # BA
             'numeric',   # CnS
             'numeric',   # LS
             'numeric',   # UI
             'numeric',   # temp1
             'numeric',   # temp2
             'numeric',   # temp3
             'numeric',   # temp4
             'numeric',   # temp5
             'numeric',   # temp6
             'numeric',   # temp7
             'numeric',   # temp8
             'numeric',   # temp9
             'numeric')   # temp10

classes3 = c('factor', # project
             'factor',    # time type
             'numeric',   # run time
             'factor',    # configuration
             'factor',    # run
             'numeric',   # FI
             'numeric',   # AA
             'numeric',   # NTA
             'numeric',   # DAC
             'numeric',   # RAC
             'numeric',   # RC
             'numeric',   # AC
             'numeric',   # FFD
             'numeric',   # FDD
             'numeric',   # CB
             'numeric',   # CA
             'numeric',   # BA
             'numeric',   # temp1
             'numeric',   # temp2
             'numeric',   # temp3
             'numeric',   # temp4
             'numeric',   # temp5
             'numeric',   # temp6
             'numeric',   # temp7
             'numeric',   # temp8
             'numeric',   # temp9
             'numeric')   # temp10
#'numeric')   # temp11

classes2 = c('factor', # project
             'factor',    # time type
             'numeric',   # run time
             'factor',    # configuration
             'factor',    # run
             'numeric',   # FI
             'numeric',   # AA
             'numeric',   # NTA
             'numeric',   # DAC
             'numeric',   # RAC
             'numeric',   # RC
             'numeric',   # AC
             'numeric',   # FFD
             'numeric',   # FDD
             'numeric',   # CB
             'numeric',   # CA
             'numeric',   # CS
             'numeric',   # temp1
             'numeric',   # temp2
             'numeric',   # temp3
             'numeric',   # temp4
             'numeric',   # temp5
             'numeric',   # temp6
             'numeric',   # temp7
             'numeric',   # temp8
             'numeric',   # temp9
             'numeric',   # temp10
             'numeric')   # temp11

col_names3 = c("PROJECT",
               "TYPE_TIME",
               "TIME",
               "CONFIG",
               "RUN",
               "FI",
               "AA",
               "NTA",
               "DAC",
               "RAC",
               "RC",
               "AC",
               "FFD",
               "FDD",
               "CB",
               "CA",
               "BA",
               'temp1', 
               'temp2',
               'temp3', 
               'temp4',
               'temp5',
               'temp6',
               'temp7',
               'temp8',
               'temp9',
               'temp10')
#'temp11')

col_names2 = c("PROJECT",
               "TYPE_TIME",
               "TIME",
               "CONFIG",
               "RUN",
               "FI",
               "AA",
               "NTA",
               "DAC",
               "RAC",
               "RC",
               "AC",
               "FFD",
               "FDD",
               "CB",
               "CA",
               "CS",
               'temp1', 
               'temp2',
               'temp3', 
               'temp4',
               'temp5',
               'temp6',
               'temp7',
               'temp8',
               'temp9',
               'temp10',
               'temp11')

classes0 = c('factor', # project
             'factor',    # time type
             'numeric',   # run time
             'factor',    # configuration
             'factor',    # run
             'numeric',   # keeness factor, TODO: delete
             'numeric',   # creation threshold, TODO: delete
             'numeric',   # FI
             'numeric',   # AA
             'numeric',   # NTA
             'numeric',   # DAC
             'numeric',   # RAC
             'numeric',   # RC
             'numeric',   # AC
             'numeric',   # FFD
             'numeric',   # FDD
             'numeric',   # CB
             'numeric',   # CA
             'numeric',   # CS
             'numeric',   # temp1
             'numeric',   # temp2
             'numeric',   # temp3
             'numeric',   # temp4
             'numeric',   # temp5
             'numeric',   # temp6
             'numeric',   # temp7
             'numeric',   # temp8
             'numeric',   # temp9
             'numeric',   # temp10
             'numeric')   # temp11

classes1 = c('factor', # project
             'factor',    # time type
             'numeric',   # run time
             'factor',    # configuration
             'factor',    # run
             'numeric',   # FI
             'numeric',   # AA
             'numeric',   # NTA
             'numeric',   # DAC
             'numeric',   # RAC
             'numeric',   # RC
             'numeric',   # AC
             'numeric',   # FFD
             'numeric',   # FDD
             'numeric',   # CB
             'numeric',   # CA
             'numeric',   # CS
             'numeric',   # temp1
             'numeric',   # temp2
             'numeric',   # temp3
             'numeric',   # temp4
             'numeric',   # temp5
             'numeric',   # temp6
             'numeric',   # temp7
             'numeric',   # temp8
             'numeric',   # temp9
             'numeric',   # temp10
             'numeric')   # temp11

col_names0 = c("PROJECT",
               "TYPE_TIME",
               "TIME",
               "CONFIG",
               "RUN",
               "KF",   # Keenness factor. TODO: remove from timer
               "CT",   # Creation threshold. TODO: remove from timer
               "FI",
               "AA",
               "NTA",
               "DAC",
               "RAC",
               "RC",
               "AC",
               "FFD",
               "FDD",
               "CB",
               "CA",
               "CS",
               'temp1', 
               'temp2',
               'temp3', 
               'temp4',
               'temp5',
               'temp6',
               'temp7',
               'temp8',
               'temp9',
               'temp10',
               'temp11')

col_names1 = c("PROJECT",
               "TYPE_TIME",
               "TIME",
               "CONFIG",
               "RUN",
               "FI",
               "AA",
               "NTA",
               "DAC",
               "RAC",
               "RC",
               "AC",
               "FFD",
               "FDD",
               "CB",
               "CA",
               "CS",
               'temp1', 
               'temp2',
               'temp3', 
               'temp4',
               'temp5',
               'temp6',
               'temp7',
               'temp8',
               'temp9',
               'temp10',
               'temp11')

# loadDFs :: [File] -> DataFrame
#
# This is an alternative to getEverything that does NOT
# process the rows--it just loads the CSVs into a DF.
getDFfromCSV <- function(filelist, cl, cn) {
  #filelist <- BR_default
  #cl <- classes3
  #cn <- col_names3
  df <- data.frame(matrix(ncol = length(cn), nrow = 0))
  
  for (f in filelist) {
    #f <- filelist[1]
    fname <- f[[1]]
    tmp <- read.csv(fname,
                    header=FALSE,
                    colClasses=cl,
                    col.names=cn)
    tmp$DATE <- f[[2]] # Add the date to the data set as a column
    df <- rbind(df, tmp) # rbind it into accumulated df
  }
  
  return(df)
}



# getEverything :: [[String, String]] -> DataFrame
#
# Take in a list of string tuples of (filename, date),
# retrieve the rows from the CSVs in the files, construct
# the data frames with process(), and rbind them all into
# a dataframe.
#
# This function processes each file PER DATE.
getEverything <- function(filelist, cl, cn) {
  df <- data.frame(matrix(ncol = length(n), nrow = 0))
  
  for (f in filelist) {
    fname <- f[[1]]
    tmp <- read.csv(fname,
                    header=FALSE,
                    colClasses=cl,
                    col.names=cn)
    tmp$DATE <- f[[2]] # Add the date to the data set as a column
    
    tmp <- tmp %>% process() # process this date's data set
    df <- rbind(df, tmp) # rbind it into accumulated df
  }
  
  return(df)
}

##############################################
#
# Get the minimum average timings for both
# the default DF and the randomized DF
#
##############################################


# getMinDF :: DataFrame -> DataFrame
#
# Given a data frame of all timings, return a data frame
# of the minimum timings for each project by config average.
# Returned DFs will be ordered by the project name list.
# TODO: Deduplicate and keep this one
getMinDF <- function(df, targetcol) {
  # An empty data frame to hold the fastest timings of the default GHC timings
  min_df <- data.frame(matrix(ncol = length(col_names3), nrow = 0))
  
  for (p in project_names) {
    # Find the row in the defaults df where the project has the
    # fastest average execution time
    p_fastest_time <- min(df[df$PROJECT==p,][[targetcol]]) # scalar
    # Get the fastest run time. Take the 1st row, if there are multiple
    p_fastest_row <- head(df[df$PROJECT==p &
                               df[[targetcol]]==p_fastest_time,], 1)
    min_df <- rbind(min_df, p_fastest_row) # rbind project min row to mins df
  }
  
  return(min_df)
}

##############################################
#
# Graph the minima of a DF against the minima
# of another Df
#
##############################################
graphMins <- function(mindf, tabletitle, compare_mins) {
    ggplot(mapping=aes(x=mindf$PROJECT, y)) +
    geom_bar(data=data.frame(x=1:nrow(compare_mins), y=(compare_mins$PROJ_CONFIG_AVG)), width=0.8, stat='identity')   +
    geom_bar(data=data.frame(x=1:nrow(mindf),
                             y=(mindf$PROJ_CONFIG_AVG)),
             width=0.6, stat='identity', fill='lightgray') +
    geom_text(data=data.frame(x=(seq(1, nrow(compare_mins))),
                              y=compare_mins$PROJ_CONFIG_AVG),
              aes(label=paste(mindf$SPEEDUP,"%")),
              vjust=-0.2) +
    theme_classic() + 
    ggtitle(tabletitle) + 
    xlab("Packages") + 
    ylab("Execution time (seconds)") +
    ylim(0, 120) + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
}


##############################################
#
#  Help to create data frames
#
##############################################

dfLists <- function(pdir, datestrlist) {
  out <- list()
  for (ds in datestrlist) {
    out <- c(out, list(list(file.path(pdir, paste(ds, ".csv", sep="")), ds)))
  }
  return(out)
}

# Concatenate directories & files
getPath <- function(parentdir, newitem) {
  return(file.path(parentdir, newitem, fsep=.Platform$file.sep))
}

###########################
#
# Some constants
#
###########################

wd <- getwd()
datadir <- getPath(wd, "DATA")
imagedir <- getPath(wd, "IMAGES")

hwrankselectSTR <- "hw-rankselect-0.13.3.1"
loopSTR <- "loop-0.3.0"
listlikeSTR <- "ListLike-4.6.3"
metricsSTR <- "metrics-0.4.1.1"
midiSTR <- "midi-0.2.2.2"
monoidSTR <- "monoid-subclasses-1.0.1"
nonemptySTR <- "nonempty-containers-0.3.3.0"
polySTR <- "poly-0.3.3.0"
reinterpretSTR <- "reinterpret-cast-0.1.0"
setcoverSTR <- "set-cover-0.1"

project_names <- c(
  hwrankselectSTR,
  loopSTR,
  listlikeSTR,
  metricsSTR,
  midiSTR,
  monoidSTR,
  nonemptySTR,
  polySTR,
  reinterpretSTR,
  setcoverSTR
)

getShortName <- function(n) {
  if (n == polySTR) {
    return("poly")
  } else if (n == hwrankselectSTR) {
    return("hw-rankselect")
  } else if (n == midiSTR) {
    return("midi")
  } else if (n == monoidSTR){
    return("monoid-subclasses")
  } else if (n == reinterpretSTR) {
    return("reinterpret-cast")
  } else if (n == loopSTR) {
    return("loop")
  } else if (n == setcoverSTR) {
    return("set-cover")
  } else if (n == metricsSTR) {
    return("metrics")
  } else if (n == nonemptySTR) {
    return("nonempty-containers")
  } else if (n == listlikeSTR) {
    return("ListLike")
  }
}

getShortNameVec <- function(v) {
  sapply(v, function(x) getShortName(x))
}
