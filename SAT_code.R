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
#library(grid)

#load data
#School Quality Reports - New York City Department of Education (2014-2015)
#code book: http://schools.nyc.gov/NR/rdonlyres/1B63FCE4-29D5-450F-8AF3-BB4CEA947CB1/0/HSFamilyGuide.pdf

fileUrl = "https://data.cityofnewyork.us/download/vrfr-9k4d/application%2Fvnd.ms-excel"
download.file(fileUrl, destfile = "2015_School_Quality_Reports.xlsx", mode="wb")
school_quality1 <- read.xlsx("2015_School_Quality_Reports.xlsx", sheet = 1, startRow = 2, colNames = TRUE)
school_quality5 <- read.xlsx("2015_School_Quality_Reports.xlsx", sheet = 5, startRow = 2, colNames = TRUE)

#transform the data
#retaining only the columns relevant to analysis
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

#allowed to delete columns with missing?
#see codebook on website for why some missing


sapply(SAT_summary_2015, function(x) sum(is.na(x)))
aggr(SAT_summary_2015[,c(3:7, 26:31)])  # visualization and imputation of missing values
md.pattern(SAT_summary_2015)            # numerical analysis of missing values

imputed.1nn = kNN(SAT_summary_2015, k = 1) #Imputing using 1NN.

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

SAT_summary_2015_sections =
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

#correlation of school characteristics to section scores =====================

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

ggplot(correlationdf, aes(reorder(factor, Avg.Total), Avg.Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Correlation of School Characteristics To School's Average SAT Score",
       x = "", y = "Correlation") +
  coord_flip(ylim = c(-0.75, 0.75)) #+
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(correlationdfsorted, aes(reorder(factor, value), value)) +
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge") +
  labs(title = "Correlation of School Characteristics To School's Average SAT Score",
       x = "", y = "Correlation") +
  coord_flip(ylim = c(-0.75, 0.75)) #+
#theme(axis.text.x = element_text(angle = 90, hjust = 1))
