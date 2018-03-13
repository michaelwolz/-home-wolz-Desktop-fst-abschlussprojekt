# Load the packages
library(sqldf)
library(ggplot2)

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


##################################
#          ANALYZE DATA          #
##################################

# Get Top 15 annotations by use
top15_by_use_tmp = aggregate(list(max_count=filtered_annotations$count), by=list(annotation=filtered_annotations$annotation), max)
top15_by_use = tail(top15_by_use_tmp[order(top15_by_use_tmp$max_count),], 15)

p = sum(top15_by_use_tmp$max_count)

par(mai=c(1,2,1,1))
bp = barplot(top15_by_use$max_count / p * 100, main="Top 15 most used annotations", horiz=TRUE, names.arg=top15_by_use$annotation, las=1, xlim=c(0,56))
text((top15_by_use$max_count / p * 100), bp, paste(round((top15_by_use$max_count / p * 100), 2), "%"), pos=4, cex=0.8)

# Get Top 15 annotations by projects
top15 = tail(annotation_use[order(annotation_use$count),], 15)
sum_annotation_use = sum(annotation_use$count)
par(mai=c(1,2,1,1))
bp = barplot(top15$count / sum_annotation_use * 100, main="Top 15 most annotations in projects", horiz=TRUE, names.arg=top15$annotation, las=1, xlim=c(0,20))
text((top15$count / sum_annotation_use * 100), bp, paste(round((top15$count / sum_annotation_use * 100), 2), "%"), pos=4, cex=0.8)

# Look at Top 15 annotations in context of time
top15_annotations_time = sqldf("SELECT a.annotation, a.year, a.quarter, a.count FROM filtered_annotations as a INNER JOIN top15 as t ON a.annotation = t.annotation")

annotation_plot = ggplot(top15_annotations_time, aes(x=paste(substr(year, 3,4), quarter, sep="/"), y=count, group=annotation, colour=annotation)) + geom_line() + geom_point() + xlab("Year/Quarter") + ylab("% Annotation") + ggtitle("Top 15 annotations over time") + labs(color="Annotation")  
plot(annotation_plot)

top15_merge_revisions = merge(top15_annotations_time, annotation_use, by=c("annotation"), suffixes=c("Annotations", "Revision"))
top15_merge_revisions$relativeUse = with(top15_merge_revisions, countAnnotations / countRevision * 100)
annotation_plot3 = ggplot(top15_merge_revisions, aes(x=paste(substr(year, 3,4), quarter, sep="/"), y=relativeUse, group=annotation, colour=annotation)) + geom_line() + geom_point() + xlab("Year/Quarter") + ylab("% Annotation") + ggtitle("Top 15 annotations over time") + labs(color="Annotation")  
plot(annotation_plot3)

max_values = aggregate(list(max_count=top15_annotations_time$count), by=list(annotation=top15_annotations_time$annotation), max)
top15_merged = merge(top15_annotations_time, max_values, by=c("annotation"))
top15_merged$relativeUse = with(top15_merged, count / max_count * 100)
annotation_plot4 = ggplot(top15_merged, aes(x=paste(substr(year, 3,4), quarter, sep="/"), y=relativeUse, group=annotation, colour=annotation)) + geom_line() + geom_point() + xlab("Year/Quarter") + ylab("% Annotation") + ggtitle("Top 15 annotations over time") + labs(color="Annotation")  
plot(annotation_plot4)

# Plot only deprecated 
deprecated = sqldf("SELECT a.annotation, a.year, a.quarter, a.count FROM filtered_annotations as a WHERE a.annotation = 'Deprecated'")
max_dep = max(deprecated$count)
dep_plot = ggplot(deprecated, aes(x=paste(substr(year, 3,4), quarter, sep="/"), y=count / max_dep * 100, group=annotation,colour=annotation)) + geom_line() + geom_point() + xlab("Year/Quarter") + ylab("% Annotation") + ggtitle("Top 15 annotations over time") + labs(color="Annotation")  
plot(dep_plot)
