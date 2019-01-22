StuP.df <- read.csv("StudentsPerformance.csv")
View(StuP.df)
sapply(StuP.df,function(x) sum(is.na(x)))


library(ggplot2)
StuP.df$writing.grade <- ifelse(StuP.df$writing.score >= 80,'A(80~100)',
                             ifelse(StuP.df$writing.score <80 & StuP.df$writing.score>=60,'B(60~79)',
                                    ifelse(StuP.df$writing.score <60 & StuP.df$writing.score >=40,'C(40~59)','F(0~39)')))

StuP.df$math.grade <- ifelse(StuP.df$math.score >= 80,'A(80~100)',
                                ifelse(StuP.df$math.score <80 & StuP.df$math.score>=60,'B(60~79)',
                                       ifelse(StuP.df$math.score <60 & StuP.df$math.score >=40,'C(40~59)','F(0~39)')))
                                              
ggplot(StuP.df)+
  geom_point(aes(x= reading.score,y= writing.score, color=factor(StuP.df$math.grade)),
             alpha = .7)+
  xlab("Reading Score")+ylab("Writing Score")+
  scale_color_discrete(name="Math Grade")+
    theme_classic()

ggplot(StuP.df)+
  geom_point(aes(x=reading.score,y= math.score, color=factor(StuP.df$writing.grade)),
             alpha = .7)+
  xlab("Reading Score")+ylab("Math Score")+
  scale_color_discrete(name="Writing Grade")+
  theme_classic()



par(mfcol = c(3,1))
hist(StuP.df$math.score, xlab = "Math Score", col = "navy", nclass = 20, border = 'navy',main = "Histogram of Math Score")
hist(StuP.df$reading.score, xlab = "Reading Score", col = "orange", nclass = 20, border ='orange',main = "Histogram of Reading Score")
hist(StuP.df$writing.score, xlab = "Writing Score", col = "firebrick", nclass = 20, border ='firebrick',main = "Histogram of Writing Score")

par(mfcol=c(1,1))
library(GGally)
ggpairs(StuP.df[,c(6:8)])

ggplot(StuP.df, aes(x=parental.level.of.education,fill=race.ethnicity))+
  geom_bar(position = "dodge")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=0.1)+
  xlab("Parental Level of Education")+ylab("Count")+
  scale_fill_discrete(name="Race/Ethnicity")+
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))+
  theme_classic()



