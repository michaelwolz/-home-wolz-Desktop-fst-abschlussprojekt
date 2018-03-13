# Load the packages
library(sqldf)
library(ggplot2)
library(RColorBrewer)

##################################
#             HELPER             #
##################################

# This function sets the annotation count in relation to the revision count at the specific quarter
set_relative_count <- function(annotation) {
  # 
}

##################################
#           LOAD DATA            #
##################################

annotations = read.csv("result-github-september-2015.csv")                      # read annotation file 
annotation_use = read.csv("annotations_in_repos-github-september-2015.csv")     # read how many repositories contain a specific annotation
revisions = read.csv("revisions-github-september-2015.csv")                     # read revisions file
repository_creation = read.csv("repository-created-github-september-2015.csv")  # read repository creation dates


##################################
#         FIND BOUNDARIES        #
##################################

# Revisions


# Searching for a reason, why the amount of revisions is going down after 2013
barplot(repository_creation$count, main = "Repository creation date", 
        xlab = "Year/Quarter", ylab = "Amount of repositories created", names.arg = paste(substr(repository_creation$year, 3, 4), repository_creation$quarter, sep="/"))


##################################
#           CLEAN DATA           #
##################################

# My suggested timeframe is 2005/1?
# Also remove all data where annotation count is smaller than 500 ???
filtered_annotations = subset(annotations, year > 2005 & count > 499)

# Remove all annotations which are used in less than ? repositories
annotation_use = subset(annotation_use, count > 3)
#filtered_annotations = sqldf("SELECT a.annotation, a.year, a.quarter, a.count FROM filtered_annotations as a INNER JOIN annotation_use as au ON a.annotation = au.annotation")

# Unique annotations
#print(nrow(subset(filtered_annotations,!duplicated(filtered_annotations$annotation)))) # 454

# Normalize data
#lapply(filtered_annotations, set_relative_count)

##################################
#          ANALYZE DATA          #
##################################

# Get Top 15 annotations
top15 = tail(annotation_use[order(annotation_use$count),], 15)
sum_annotation_use = sum(annotation_use$count)
par(mai=c(1,2,1,1))
bp = barplot(top15$count / sum_annotation_use * 100, main="Top 15 most used annotations", horiz=TRUE, names.arg=top15$annotation, las=1, xlim=c(0,20))
text((top15$count / sum_annotation_use * 100), bp, paste(round((top15$count / sum_annotation_use * 100), 2), "%"), pos=4, cex=0.8)

# Look at Top 15 annotations in context of time
top15_annotations_time = sqldf("SELECT a.annotation, a.year, a.quarter, a.count FROM filtered_annotations as a INNER JOIN top15 as t ON a.annotation = t.annotation")
annotation_plot = ggplot(top15_annotations_time, aes(x=paste(substr(year, 3,4), quarter, sep="/"), y=count, group=annotation, colour=annotation)) + geom_line() + geom_point() + xlab("Year/Quarter") + ylab("% Annotation") + ggtitle("Top 15 annotations over time") + labs(color="Annotation")  
plot(annotation_plot)
