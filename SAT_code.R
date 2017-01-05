### This project focuses on 
### School Quality Reports - New York City Department of Education (2014-2015)
### Data: www.
### Codebook: http://schools.nyc.gov/NR/rdonlyres/1B63FCE4-29D5-450F-8AF3-BB4CEA947CB1/0/HSFamilyGuide.pdf

library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)        #for the visualization and imputation of missing values
library(mice)       #multivariate imputation by chained equations library
library(Hmisc)      #the Harrell miscellaneous library for imputation
library(GGally)     #the correlation grid
library(corrplot)
library(psych)      #Library that contains helpful PCA functions
library(car)        #advanced utilities for regression modeling
library(relaimpo)   #relative variable imp for regression
library(tree)
library(randomForest)
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

ggplot(SAT_summary_2015, aes(x=Avg.Math, y=Avg.Reading)) + stat_bin2d(bins=50)

#graphs
ggplot(SAT_summary_2015, aes(x=Avg.Total)) + 
  geom_histogram(binwidth = 40, 
                 col="white", 
                 fill="steelblue") + 
  labs(title="Distribution of SAT Scores", subtitle="New York City Public Schools", x="Average Score for School", y="Total Count") +
  geom_vline(aes(xintercept=mean(Avg.Total, na.rm=T)),   # Ignore NA values for mean
             color="darkgreen", linetype="dashed", size=0.8) +
  geom_text(
    mapping = aes(x2, y2, label = text2),
    data = data.frame(x2 = 1290, y2 = 60, text2 = "mean[score]==1273"),
    color = I("darkgreen"),
    parse = TRUE,
    hjust = 0
  ) +
  geom_vline(aes(xintercept=median(Avg.Total, na.rm=T)),   # Ignore NA values for mean
             color="purple", linetype="dashed", size=0.8) +
  geom_text(
    mapping = aes(x2, y2, label = text3),
    data = data.frame(x2 = 1244, y2 = 65, text3 = "median[score]==1227"),
    color = I("purple"),
    parse = TRUE,
    hjust = 0
  ) +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))
#make weighted average! weighted median?

#violin and box plot ======================

section_scores <- select(SAT_summary_2015, Avg.Math, Avg.Reading, Avg.Writing)
names(section_scores) <- c("Math", "Reading", "Writing")
section_scores <- reshape2::melt(section_scores, id.vars = NULL)
section_scores <- filter(section_scores, value != "NA")
means <- aggregate(value ~  variable, section_scores, mean)
means[,2] <- round(means[,2])

ggplot(section_scores, aes(x = variable, y = value, fill = variable)) + 
  geom_violin(color="steelblue",fill="steelblue",size=1) +
  geom_boxplot(fill="steelblue",width = 0.3) +
  stat_summary(fun.y=mean, colour="darkblue", geom="point", 
               shape="_", size=5, show.legend = FALSE) +  #shape=16, size 1
  geom_text(data = means, aes(label = value, y = value + 12)) +
  labs(title = "Breakdown of Each SAT Section", subtitle="Violin Plot with Boxplot",x = "", y = "Section Score") +
  theme(legend.position="none") +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))
  
#make weighted average! weighted median?

#correlation between sections =================

#numerically
cor(SAT_summary_2015[complete.cases(SAT_summary_2015), c("Avg.Math", "Avg.Reading", "Avg.Writing")],)

#graphically with pair plot
grid <- SAT_summary_2015[complete.cases(SAT_summary_2015), c("Avg.Math", "Avg.Reading", "Avg.Writing")]
names(grid) <- c("Math", "Reading", "Writing")

ggpairs(
  grid,
  upper = list(continuous = wrap("cor", size = 5)), 
  lower = list(continuous = wrap("smooth", color = "steelblue")),
  diag = list(continuous = wrap("barDiag", bins = 100, color = "steelblue")),
  axisLabels = 'show',
  title="Correlation Between Sections") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    axis.ticks = element_blank()) +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(20,0,0,0)))

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

top <-
  SAT_summary_2015 %>%
  select(School.Name.x,Avg.Math, Avg.Reading, Avg.Writing,  Avg.Total) %>%
  arrange(desc(Avg.Total)) %>%
  top_n(37)

top.m <- top %>% select(-Avg.Total)
names(top.m) <- c("School.Name.x", "Math", "Reading", "Writing")
top.m <- melt(top.m, id.vars = "School.Name.x")
top.m$variable <- factor(top.m$variable, levels = c("Writing", "Reading", "Math")) #switch order of legend

ggplot(top, aes(x = reorder(School.Name.x, Avg.Total), y = Avg.Total)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(1400, 2200)) +
  geom_text(aes(label=Avg.Total),vjust=0.3, hjust=-.2, position=position_dodge(width=0.9), size=2.5) +
  labs(title = "Top SAT Scores", subtitle = "NYC Public Schools, Top 10%",
       x = "", y = "Average SAT Score") +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))

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

#stacked by section
ggplot(top.m, aes(x = reorder(School.Name.x, value), y = value, fill=variable)) +
  geom_bar(stat='identity',position = "fill") +
  scale_fill_manual(values = c("brown1","brown3", "steelblue"),
                    breaks = c("Math", "Reading", "Writing"),
                    labels = c("Math", "Reading", "Writing")) +
  coord_flip() +
  labs(title = "SAT Score By Section", subtitle="NYC Public Schools, Top 10%", x = "", y = "") +
  geom_text(aes(label=value), size=2,position = "fill",hjust = 2, vjust=0.3, color="white") +
  # geom_hline(aes(yintercept=.6666), color="gray", linetype="dashed", size=0.4) +
  # geom_hline(aes(yintercept=.3333), color="gray", linetype="dashed", size=0.4) +
  theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))


#==bottom====================================

bottom <-
  SAT_summary_2015 %>%
  select(School.Name.x, Avg.Math, Avg.Reading, Avg.Writing, Avg.Total) %>%
  filter(Avg.Total != "NA") %>%
  arrange(Avg.Total) %>%
  top_n(-37)

bottom.m <- bottom %>% select(-Avg.Total)
names(bottom.m) <- c("School.Name.x", "Math", "Reading", "Writing")
bottom.m <- melt(bottom.m, id.vars = "School.Name.x")
bottom.m$variable <- factor(bottom.m$variable, levels = c("Writing", "Reading", "Math")) #switch legend order

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

#stacked by section
ggplot(bottom.m, aes(x = reorder(School.Name.x, value), y = value, fill=variable)) +
  geom_bar(stat='identity',position = "fill") +
  scale_fill_manual(values = c("brown1","brown3", "steelblue"),
                    breaks = c("Math", "Reading", "Writing"),
                    labels = c("Math", "Reading", "Writing")) +
  coord_flip() +
  labs(title = "SAT Score By Section", subtitle="NYC Public Schools, Top 10%", x = "", y = "") +
  geom_text(aes(label=value), size=2,position = "fill",hjust = 2, vjust=0.3, color="white") +
  # geom_hline(aes(yintercept=.6666), color="gray", linetype="dashed", size=0.4) +
  # geom_hline(aes(yintercept=.3333), color="gray", linetype="dashed", size=0.4) +
  theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))

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
  theme(plot.title=element_text(margin=margin(0,0,15,0),hjust = 0.5))

ggplot(correlationdfsorted, aes(reorder(factor, value), value)) +
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge") +
  labs(title = "Correlation of School Characteristics To School's Average SAT Score",
       x = "", y = "Correlation") +
  coord_flip(ylim = c(-0.75, 0.75)) #+
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

# PCA =====================
# install.packages("GPArotation")
# library(GPArotation)
                 
#creating a scree plot with parallell analyses for choosing K
fa.parallel(SAT_summary_2015_complete2[,-c(1:2,7)],
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.

#just economic factors
fa.parallel(SAT_summary_2015_complete2[,c(18:24)],
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.

pc_SAT = principal(SAT_summary_2015_complete2[,-c(1,2,7)], #The data in question.
                      nfactors = 2, #The number of PCs to extract.
                      rotate = "none")
pc_SAT = principal(SAT_summary_2015_complete2[,c(18:24)], #The data in question.
                   nfactors = 2, #The number of PCs to extract.
                   rotate = "none")
pc_SAT

factor.plot(pc_SAT,
            labels = colnames(SAT_summary_2015[,-c(1:2,7)]))
factor.plot(pc_SAT,
            labels = colnames(SAT_summary_2015_complete2[,c(18:24)]))

# GLM ==============

SAT_summary_2015_complete_scaled = scale(SAT_summary_2015_complete[,-c(1:5,7)])
SAT_summary_2015_complete_scaled = as.data.frame(SAT_summary_2015_complete_scaled)
summary(SAT_summary_2015_complete_scaled)

lm.model.scaled = lm(`Average Total SAT Score` ~ .
                     # - `Average Grade 8 English Proficiency` # unfairly high importance
                     # - `Average Grade 8 Math Proficiency` # unfairly high importance
                     ,
                     data = SAT_summary_2015_complete_scaled)
summary(lm.model.scaled)

lm.model = lm(`Average Total SAT Score` ~ . 
                - DBN 
                - `School Name x`
                - `School Name y` 
                - `Average Math SAT Score` 
                - `Average Reading SAT Score` 
                - `Average Writing SAT Score`
                # - `Average Grade 8 English Proficiency` # unfairly high importance
                # - `Average Grade 8 Math Proficiency` # unfairly high importance
                ,
              data = SAT_summary_2015_complete)

summary(lm.model)
vif(lm.model)
av.Plots(lm.model)

# layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
# plot(glm.model)

lm.relimp = calc.relimp(lm.model, type = c("lmg"), rela = TRUE)
lm.scaled.relimp = calc.relimp(lm.model.scaled, type = c("lmg"), rela = TRUE)
lmg = read.csv("lmg.csv", header = F, stringsAsFactors = F)
lmg$V1 = trimws(lmg$V1)

ggplot(lmg, aes(reorder(V1, V2), V2)) +
  geom_bar(stat = "identity", col="white", fill = "steelblue") +
  labs(title = "Variable Relative Importance", subtitle = "Shapley Value Regression",
       x = "", y = "Relative importance (contribution percentage)") +
  coord_flip(ylim = c(0,0.25)) +
  geom_text(aes(label=paste0(round(V2,3)*100,"%")),vjust=0.5, hjust=-.2, position=position_dodge(width=0.9), size=2.5) +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))

# random forest =====================

tree.SAT = tree(Avg.Total ~ .
                - DBN
                - School.Name.x
                - School.Name.y
                - Avg.Math
                - Avg.Reading
                - Avg.Writing
                # - Average.Grade.8.Math.Proficiency
                # - Average.Grade.8.English.Proficiency
                ,
                data = data.frame(SAT_summary_2015), 
                subset = complete.cases(data.frame(SAT_summary_2015)))
summary(tree.SAT)
plot(tree.SAT, main = "hello")
text(tree.SAT, pretty = 0)

#cleaning data for tree formula
SAT_summary_2015_complete2 <- SAT_summary_2015_complete
names(SAT_summary_2015_complete2) <- gsub("-", "", names(SAT_summary_2015))
SAT_summary_2015_complete2 <- SAT_summary_2015_complete2[complete.cases(SAT_summary_2015_complete2),]
# temp <- names(SAT_summary_2015_complete2)
# temp <- as.factor(temp)


tree.SAT2 <- rpart(Avg.Total ~ Enrollment +
                     Rigorous.Instruction..Percent.Positive +
                     Collaborative.Teachers..Percent.Positive +
                     Supportive.Environment..Percent.Positive +
                     Effective.School.Leadership..Percent.Positive +
                     Strong.FamilyCommunity.Ties..Percent.Positive +
                     Trust..Percent.Positive +
                     Average.Grade.8.English.Proficiency +
                     Average.Grade.8.Math.Proficiency  +
                     Percent.English.Language.Learners +
                     Percent.Students.with.Disabilities +
                     Percent.SelfContained  +
                     Economic.Need.Index  +
                     Percent.in.Temp.Housing..4yr +
                     Percent.HRA.Eligible +
                     Percent.Asian +
                     Percent.Black +
                     Percent.Hispanic +
                     Percent.White +
                     Years.of.principal.experience.at.this.school +
                     Percent.of.teachers.with.3.or.more.years.of.experience +
                     Student.Attendance.Rate +
                     Percent.of.Students.Chronically.Absent +
                     Teacher.Attendance.Rate
                   ,
                   method="anova",
                   data = SAT_summary_2015_complete2
                   )
printcp(tree.SAT2)
plotcp(tree.SAT2)
plot(tree.SAT2, uniform=TRUE)
text(tree.SAT2, use.n=TRUE, all=TRUE, cex=.8)
library(rpart.plot)
prp(tree.SAT2, type=1, extra=1)
fancyRpartPlot(tree.SAT2)

#random forest
set.seed(0)
rf.SAT = randomForest(Avg.Total ~ .
                         - DBN
                         - School.Name.x
                         - School.Name.y
                         - Avg.Math
                         - Avg.Reading
                         - Avg.Writing
                         # - Average.Grade.8.Math.Proficiency
                         # - Average.Grade.8.English.Proficiency
                         , 
                         data = data.frame(SAT_summary_2015), 
                         subset = complete.cases(data.frame(SAT_summary_2015)),
                         importance = TRUE)
rf.SAT
varImpPlot(rf.SAT, type = 1)
importance(rf.SAT, type = 1)

varimp = read.csv("varimp.csv", header = F)

# varimp = as.data.frame(importance(rf.SAT, type = 1))
# varimp = cbind(labels(varimp)[[1]], varimp[1:24,])
# varimp = as.data.frame(varimp)
varimp$V1 = gsub("\\.", " ", varimp$V1)
varimp$V1 = trimws(varimp$V1)
varimp$V1 = gsub("   ", " ", varimp$V1)
# varimp$V2 = as.numeric(as.character(varimp$V2))


ggplot(varimp, aes(reorder(V1, V2), V2)) +
  geom_bar(stat = "identity", col="white", fill = "steelblue") +
  labs(title = "Variable Relative Importance", subtitle = "Random Forest\n",
       x = "", y = "") +
  coord_flip(ylim = c(-3,25)) +
  geom_text(aes(label=paste0(round(V2,1),"%")),vjust=0.5, hjust=ifelse(sort(varimp$V2, decreasing = F)>0,-0.2,1.1), position=position_dodge(width=0.9), size=2.5) +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.subtitle=element_text(hjust=0.5)) 
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))


#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(24)
for (mtry in 1:24) {
  fit = randomForest(Avg.Total ~ .
                     - DBN
                     - School.Name.x
                     - School.Name.y
                     - Avg.Math
                     - Avg.Reading
                     - Avg.Writing
                     # - Average.Grade.8.Math.Proficiency
                     # - Average.Grade.8.English.Proficiency
                     ,
                     data = data.frame(SAT_summary_2015), 
                     subset = complete.cases(data.frame(SAT_summary_2015)),
                     mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:24, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")


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