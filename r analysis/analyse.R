# Load the packages
library(sqldf)

##################################
#             HELPER             #
##################################

# This function sets the annotation count in relation to the repository count at the specific quarter
set_relative_count <- function(annotation) {
  # 
}

##################################
#           LOAD DATA            #
##################################

annotations = read.csv("result-github-september-2015.csv")                  # read annotation file 
repositories = read.csv("repositories-github-september-2015.csv")           # read repositories file
annotation_use = read.csv("annotations_in_repos-github-september-2015.csv") # read how many repositories contain a specific annotation

##################################
#           CLEAN DATA           #
##################################

# My suggested timeframe is 2005/1 - 2014/1?
# Remove all data before 2000, since there are too less repositories (maybe even 2000 is too early)
# Also remove all data where annotation count is smaller than 500 ???
filtered_annotations = subset(annotations, year > 1999 & count > 499)

# Remove all annotations which are used in less than ? repositories
annotation_use = subset(annotation_use, count > 99)
filtered_annotations = sqldf("SELECT a.annotation, a.year, a.quarter, a.count FROM filtered_annotations as a INNER JOIN annotation_use as au ON a.annotation = au.annotation")

# Unique annotations
print(nrow(subset(filtered_annotations,!duplicated(filtered_annotations$annotation)))) # 454

# Normalize data
lapply(filtered_annotations, set_relative_count)

##################################
#          ANALYZE DATA          #
##################################



##################################
#           PLOT DATA            #
##################################
