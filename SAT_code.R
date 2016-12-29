### This project focuses on 
### School Quality Reports - New York City Department of Education (2014-2015)
### Data: www.
### Codebook: http://schools.nyc.gov/NR/rdonlyres/1B63FCE4-29D5-450F-8AF3-BB4CEA947CB1/0/HSFamilyGuide.pdf

library(openxlsx)
library(dplyr)
library(ggplot2)
library(VIM)        #for the visualization and imputation of missing values
library(mice)       #multivariate imputation by chained equations library
library(Hmisc)      #the Harrell miscellaneous library for imputation
library(GGally)     #the correlation grid
library(corrplot)
library(psych)      #Library that contains helpful PCA functions

#library(grid)

# load data
# School Quality Reports - New York City Department of Education (2014-2015)
# code book: http://schools.nyc.gov/NR/rdonlyres/1B63FCE4-29D5-450F-8AF3-BB4CEA947CB1/0/HSFamilyGuide.pdf

fileUrl = "https://data.cityofnewyork.us/download/vrfr-9k4d/application%2Fvnd.ms-excel"
download.file(fileUrl, destfile = "2015_School_Quality_Reports.xlsx", mode="wb")
school_quality1 <- read.xlsx("2015_School_Quality_Reports.xlsx", sheet = 1, startRow = 2, colNames = TRUE)
school_quality5 <- read.xlsx("2015_School_Quality_Reports.xlsx", sheet = 5, startRow = 2, colNames = TRUE)

# transform the data
# retaining only the columns relevant to analysis
sheet_1 = school_quality1 %>%
  select(
    1, 2, 4, 12:17, 24:40
    )
sheet_5 = 
  school_quality5 %>%
  select(
    1, 2,
    Avg.Math = Average.Score.SAT.Math,
    Avg.Reading = Average.Score.SAT.Critical.Reading,
    Avg.Writing = Average.Score.SAT.Writing
  ) %>%
  mutate (
    Avg.Total = Avg.Math + Avg.Reading + Avg.Writing  # add a new column for average total SAT score
  )

##### add in more columns on sheet 5 for regents

SAT_summary_2015 = inner_join(sheet_5, sheet_1, by = "DBN")


# == imputation =================================

# see codebook on website for why some missing???????

# As we can see, there are approximately X missing SAT scores (16.7% of the schools in the dataset).
# These are missing because less than X students 
# However, these are the dependant variable so we will not impute the values. 
# These are not missing completely at random (MCAR) becuase they depend on
# whether a certain number of students took the test. They may be missing not at random (MNAR)
# as a school with a lower number of students taking the test may be more likely to have a lower
# score. See the correlation graph between score and number of students taking test - percentage
# of students taking test

sapply(SAT_summary_2015, function(x) sum(is.na(x)))
aggr(SAT_summary_2015[,c(3:7, 26:31)])  # visualization and imputation of missing values
md.pattern(SAT_summary_2015)            # numerical analysis of missing values

SAT_summary_2015_complete = SAT_summary_2015 %>% filter(Avg.Total != "NA")

length(which(SAT_summary_2015$Avg.Math != "NA"))


# graphs ------------------------------------------------------------------


#graphs
ggplot(SAT_summary_2015, aes(x=Avg.Total)) + 
  geom_histogram(binwidth = 100, 
                 col="white", 
                 fill="steelblue") + 
  labs(title="NYC Public Schools SAT Score Histogram") +
  labs(x="Average Score for School", y="Count") +
  geom_vline(aes(xintercept=mean(Avg.Total, na.rm=T)),   # Ignore NA values for mean
             color="darkgreen", linetype="dashed", size=1) +
  geom_text(
    mapping = aes(x2, y2, label = text2),
    data = data.frame(x2 = 1290, y2 = 110, text2 = "mean[score]==1273"),
    color = I("darkgreen"),
    parse = TRUE,
    hjust = 0
  ) +
  geom_vline(aes(xintercept=median(Avg.Total, na.rm=T)),   # Ignore NA values for mean
             color="purple", linetype="dashed", size=1) +
  geom_text(
    mapping = aes(x2, y2, label = text3),
    data = data.frame(x2 = 1244, y2 = 120, text3 = "median[score]==1227"),
    color = I("purple"),
    parse = TRUE,
    hjust = 0
  )
#make weighted average! weighted median?

#violin plots ======================

section_scores <- select(SAT_summary_2015, Avg.Math, Avg.Reading, Avg.Writing)
section_scores <- reshape2::melt(section_scores, id.vars = NULL)
section_scores <- filter(section_scores, value != "NA")
means <- aggregate(value ~  variable, section_scores, mean)
means[,2] <- round(means[,2])

ggplot(section_scores, aes(x = variable, y = value, fill = variable)) + 
  geom_violin() +
  geom_boxplot(width = 0.3) +
  stat_summary(fun.y=mean, colour="darkblue", geom="point", 
               shape=18, size=3, show.legend = FALSE) +             #find geom shape that looks like '-' instead of diamond?
  geom_text(data = means, aes(label = value, y = value + 12)) +
  labs(title = "Violin Plot Breakdown of Each SAT Section", x = "", y = "Section Score") +
  theme(legend.position="none")
#make weighted average! weighted median?

#=====correlation between sections =================

#numerically
cor(SAT_summary_2015[complete.cases(SAT_summary_2015), c("Avg.Math", "Avg.Reading", "Avg.Writing")],)

#graphically with pair plot
ggpairs(
  SAT_summary_2015[complete.cases(SAT_summary_2015), c("Avg.Math", "Avg.Reading", "Avg.Writing")],
  upper = list(continuous = wrap("cor", size = 10, color = "steelblue")), 
  lower = list(continuous = wrap("smooth", color = "steelblue")),
  diag = list(continuous = wrap("barDiag", bins = 100, color = "steelblue")),
  axisLabels = 'show') +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    axis.ticks = element_blank())

#======largest discrepancies between sections================

SAT_summary_2015_sections <-
  SAT_summary_2015 %>%
  select(
    Name = School.Name.x,
    Percent_ELL = Percent.English.Language.Learners,
    Avg.Total,
    Avg.Math,
    Avg.Reading,
    Avg.Writing
  ) %>%
  filter(Avg.Total != "NA") %>%
  mutate(
    mathtoreading = Avg.Math - Avg.Reading,
    mathtowriting = Avg.Math - Avg.Writing,
    readingtowriting = Avg.Reading - Avg.Writing
  ) %>% 
  arrange(mathtoreading)

ggplot(SAT_summary_2015_sections, aes(seq_along(mathtoreading), mathtoreading)) +
  geom_point(color = "steelblue")  +
  coord_flip() +
  labs(title = "Spread Between Math and Reading Sections", x = "", y = "Point Spread")

SAT_summary_2015_sections %>%
  select(
    Name,
    Percent_ELL,
    Avg.Total,
    Avg.Math,
    Avg.Reading,
    Avg.Writing,
    mathtoreading
  ) %>%
  filter(mathtoreading > 100 | mathtoreading < -48)
#beautify this section!

#==top schools============================================

top <- # remove this and use dataset with all variables? then limit in graph to ones used
  SAT_summary_2015 %>%
  select(School.Name.x, Avg.Total, Avg.Math, Avg.Reading, Avg.Writing) %>%
  arrange(desc(Avg.Total)) %>%
  top_n(37)

ggplot(top, aes(x = reorder(School.Name.x, Avg.Total), y = Avg.Total)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(1400, 2200)) +
  labs(title = "Top 10% NYC Public Schools SAT Scores", x = "", y = "Average SAT Score for School")

ggplot(top, aes(x = reorder(School.Name.x, Avg.Total), y = Avg.Math)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10% NYC Public Schools SAT Scores", x = "", y = "Average Math SAT Score for School")

ggplot(top, aes(x = reorder(School.Name.x, Avg.Total), y = Avg.Reading)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10% NYC Public Schools SAT Scores", x = "", y = "Average Reading SAT Score for School")

ggplot(top, aes(x = reorder(School.Name.x, Avg.Total), y = Avg.Writing)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10% NYC Public Schools SAT Scores", x = "", y = "Average Writing SAT Score for School")

#==bottom====================================

bottom <- # remove this and use dataset with all variables? then limit in graph to ones used
  SAT_summary_2015 %>%
  select(School.Name.x, Avg.Total, Avg.Math, Avg.Reading, Avg.Writing) %>%
  arrange(Avg.Total) %>%
  filter(Avg.Total != "NA") %>%
  top_n(-37)

ggplot(bottom, aes(x = reorder(School.Name.x, -Avg.Total), y = Avg.Total)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(900, 1200)) +
  labs(title = "Bottom 10% NYC Public Schools SAT Scores", x = "", y = "Average SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name.x, -Avg.Total), y = Avg.Math)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10% NYC Public Schools SAT Scores", x = "", y = "Average Math SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name.x, -Avg.Total), y = Avg.Reading)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10% NYC Public Schools SAT Scores", x = "", y = "Average Reading SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name.x, -Avg.Total), y = Avg.Writing)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10% NYC Public Schools SAT Scores", x = "", y = "Average Writing SAT Score for School")

# correlation of school characteristics to section scores =====================

#corrplot
names(SAT_summary_2015_complete) = gsub("\\.", " ", names(SAT_summary_2015_complete))
names(SAT_summary_2015_complete)[3] = "Average Math SAT Score"
names(SAT_summary_2015_complete)[4] = "Average Reading SAT Score"
names(SAT_summary_2015_complete)[5] = "Average Writing SAT Score"
names(SAT_summary_2015_complete)[6] = "Average Total SAT Score"

SAT.corr <- cor(SAT_summary_2015_complete[,-c(1,2,7,27,28,31)])
diag(SAT.corr) <- NA   #remove diagonal

corrplot(SAT.corr, method="circle", tl.cex = 0.6, tl.col = "black", tl.srt=45, order="hclust", type = "lower", na.label = " ")

# names(SAT_summary_2015) <- names(SAT_summary_2015_complete)

correlation_tot = sapply(SAT_summary_2015[complete.cases(SAT_summary_2015), -c(1:7)], function(x)
  cor(x, SAT_summary_2015[complete.cases(SAT_summary_2015), "Avg.Total"]))
correlation_tot = data.frame(labels(correlation_tot), unname(correlation_tot))
colnames(correlation_tot) = c("factor", "Avg.Total")

correlation_math = sapply(SAT_summary_2015[complete.cases(SAT_summary_2015), -c(1:7)], function(x)
  cor(x, SAT_summary_2015[complete.cases(SAT_summary_2015), "Avg.Math"]))
correlation_math = data.frame(labels(correlation_math), unname(correlation_math))
colnames(correlation_math) = c("factor", "Avg.Math")

correlation_reading = sapply(SAT_summary_2015[complete.cases(SAT_summary_2015), -c(1:7)], function(x)
  cor(x, SAT_summary_2015[complete.cases(SAT_summary_2015), "Avg.Reading"]))
correlation_reading = data.frame(labels(correlation_reading), unname(correlation_reading))
colnames(correlation_reading) = c("factor", "Avg.Reading")

correlation_writing = sapply(SAT_summary_2015[complete.cases(SAT_summary_2015), -c(1:7)], function(x)
  cor(x, SAT_summary_2015[complete.cases(SAT_summary_2015), "Avg.Writing"]))
correlation_writing = data.frame(labels(correlation_writing), unname(correlation_writing))
colnames(correlation_writing) = c("factor", "Avg.Writing")

correlationdf = data.frame(factor = correlation_tot$factor,
                           Avg.Total = correlation_tot$Avg.Total,
                           Avg.Math = correlation_math$Avg.Math,
                           Avg.Reading = correlation_reading$Avg.Reading,
                           Avg.Writing = correlation_writing$Avg.Writing) 

correlationdfsorted <- reshape2::melt(correlationdf[,c("factor","Avg.Total","Avg.Math", "Avg.Reading", "Avg.Writing")],id.vars = 1)
correlationdf$factor <- names(SAT_summary_2015_complete[-c(1:7)])
correlationdf$lab <- as.character(round(correlationdf$Avg.Total, 2))


ggplot(correlationdf, aes(reorder(factor, Avg.Total), Avg.Total)) +
  geom_bar(stat = "identity", col="white", fill = "steelblue") +
  labs(title = "Correlation of School Characteristics\nTo School's Average SAT Score",
       x = "", y = "Correlation") +
  coord_flip(ylim = c(-1, 1.1)) +
  geom_text(aes(label=lab),vjust=0.5, hjust=ifelse(sort(correlationdf$lab,decreasing=F)>0,-0.2,1.2), position=position_dodge(width=0.9), size=2.5) +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,15,0)))

ggplot(correlationdfsorted, aes(reorder(factor, value), value)) +
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge") +
  labs(title = "Correlation of School Characteristics To School's Average SAT Score",
       x = "", y = "Correlation") +
  coord_flip(ylim = c(-0.75, 0.75)) #+
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

# PCA =====================
install.packages("GPArotation")
library(GPArotation)
                 
#creating a scree plot with parallell analyses for choosing K
fa.parallel(SAT_summary_2015_complete2[,-c(1:2,7)],
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.

#just economic factors
fa.parallel(SAT_summary_2015[,c(18:24)],
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.

pc_SAT = principal(SAT_summary_2015_complete2[,-c(1:2,7)], #The data in question.
                      nfactors = 2, #The number of PCs to extract.
                      rotate = "none")
pc_SAT = principal(SAT_summary_2015[,c(18:24)], #The data in question.
                   nfactors = 2, #The number of PCs to extract.
                   rotate = "none")
pc_SAT

factor.plot(pc_SAT,
            labels = colnames(SAT_summary_2015[,c(18:24)]))

# GLM ==============

glm.model = glm(Avg.Total ~ . 
                - DBN 
                - School.Name.x 
                - School.Name.y 
                - Avg.Math 
                - Avg.Reading 
                - Avg.Writing
                - Average.Grade.8.English.Proficiency # unfairly high importance
                - Average.Grade.8.Math.Proficiency # unfairly high importance
                ,
  data = SAT_summary_2015_complete
)

summary(glm.model)
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(glm.model)


# location based analysis =====================

#loading the location file
directory = read.csv("HS_directory.csv")
locations = directory[c("dbn","school_name", "boro", "city", "zip", "Location.1")]
colnames(locations)[1] = "DBN"

#extract the coordinates
locations$coord <- gsub(".*\\((.*)\\).*", "\\1", locations$Location.1)
locations$lon <- unlist(lapply(strsplit(as.character(locations$coord), ", "), "[", 2))
locations$lat <- unlist(lapply(strsplit(as.character(locations$coord), ", "), "[", 1))
locations$lon <- as.numeric(locations$lon)
locations$lat <- as.numeric(locations$lat)

#join to file
SAT_summary_2015 <- inner_join(SAT_summary_2015, locations, by = "DBN")

boros =
  SAT_summary_2015 %>%
  group_by(boro) %>%
  summarize(average = mean(Avg.Total, na.rm = TRUE), count = n()) %>%
  arrange(desc(count))

avg = ggplot(boros, mapping = aes(x=boro, y=average)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(1000, 1500)) +
  labs(title = "Public Schools SAT Scores By Borough", x = "", y = "Average SAT Score for School")

count = ggplot(boros, mapping = aes(x=boro, y=count)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(0, 200)) +
  labs(title = "Number of Public Schools SAT Scores In Borough", x = "", y = "Average SAT Score for School")

grid.newpage()
grid.draw(rbind(ggplotGrob(avg), ggplotGrob(count), size = "last"))