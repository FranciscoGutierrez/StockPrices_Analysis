require(ggplot2)
library(reshape2)
library(car)
library(Rcmdr)
# References: 
## https://personality-project.org/r/r.guide.html#oneway
## http://www.gardenersown.co.uk/education/lectures/r/anova.htm
## http://www2.lv.psu.edu/jxm57/irp/chisquar.html


#### First Study
# study.dots   <- read.csv("data/s1_dots.csv")
# study.charts <- read.csv("data/s1_charts.csv")
# study.series <- read.csv("data/s1_maps.csv")
# qf.charts    <- read.csv("data/qf1_charts.csv")
# qf.dots      <- read.csv("data/qf1_dots.csv")
# qf.series    <- read.csv("data/qf1_maps.csv")

#### Second Study
study.dots   <- read.csv("data/s2_dots.csv")
study.charts <- read.csv("data/s2_charts.csv")
study.series <- read.csv("data/s2_series.csv")
qf.charts    <- read.csv("data/qf2_charts.csv")
qf.dots      <- read.csv("data/qf2_dots.csv")
qf.series    <- read.csv("data/qf2_series.csv")

# <Questions>
# Easy  : 1,4,7,10
# Medium: 2,5,8,11
# Hard  : 3,6,9,12
# Add missing "difficulty" parameter.

filter.users <- function(main.study, qf.study) {
    split.study  <- split(main.study, main.study$userid)
    some.users   <- c() 
    for (user in split.study) {
        some.users <- c(some.users, as.character(user$userid[1]))
    }
    some.users <- data.frame(some.users)
    output <- main.study[ ! main.study$userid %in% some.users[1,], ]
    return(output)
}

### Filter all datasets!!!

study.dots   <- filter.users(study.dots,  qf.charts)
study.charts <- filter.users(study.charts,qf.dots  )
study.series <- filter.users(study.series,qf.series)


avg.total.timespent <- function() {
    time <- sum(study.dots$timespent, study.series$timespent, study.charts$timespent)
    n    <- sum(nrow(table(study.dots$userid)),nrow(table(study.charts$userid)),nrow(table(study.series$userid)))

    return((time/1000)/n)
}

add.difficulty <- function (study, row = 19){
    ls.difficulty <- c()
    for (i in 1:nrow(study)) {
        q <- study[i,19]
        if (q == 1)  ls.difficulty <- c(ls.difficulty, "easy") 
        if (q == 2)  ls.difficulty <- c(ls.difficulty, "medium") 
        if (q == 3)  ls.difficulty <- c(ls.difficulty, "hard") 
        if (q == 4)  ls.difficulty <- c(ls.difficulty, "easy") 
        if (q == 5)  ls.difficulty <- c(ls.difficulty, "medium") 
        if (q == 6)  ls.difficulty <- c(ls.difficulty, "hard") 
        if (q == 7)  ls.difficulty <- c(ls.difficulty, "easy") 
        if (q == 8)  ls.difficulty <- c(ls.difficulty, "medium") 
        if (q == 9)  ls.difficulty <- c(ls.difficulty, "hard") 
        if (q == 10) ls.difficulty <- c(ls.difficulty, "easy") 
        if (q == 11) ls.difficulty <- c(ls.difficulty, "medium") 
        if (q == 12) ls.difficulty <- c(ls.difficulty, "hard") 
    }
    study[ , "difficulty"] <- ls.difficulty
    # Subset the data
    study  <- subset(study, 
                     select = c("userid"   ,
                                "timespent", 
                                "question" ,
                                "correct"  ,
                                "actions",
                                "viz",
                                "difficulty"))
    return(study);
}

# <Dots>
# Easy   - (Accuracy, Speed, Actions) [Mean, Median] 
# Medium - (Accuracy, Speed, Actions) [Mean, Median]
# Hard   - (Accuracy, Speed, Actions) [Mean, Median]

create.boxplot <- function(study, difficulty = "easy"){
    
    split.study <- split(study, study$userid)
    
    df.easy   <- data.frame()
    df.medium <- data.frame()
    df.hard   <- data.frame()
    
    ls.id       <- c()
    ls.viz      <- c() 
    ls.speed    <- c() 
    ls.actions  <- c() 
    ls.accuracy <- c() 
    
    for (user in split.study) {
        user.speed   <- 0
        user.actions <- 0
        user.correct <- 0
        user.id      <- user$userid[1]
        user.viz     <- user$viz[1]
        if ((nrow(user) > 11) & (nrow(user) < 15)) {
            for (i in 1:nrow(user)) {
                # Do the thing...
                if ( user$difficulty[i] == difficulty) {
                    user.speed    <- user$timespent[i] + user.speed
                    user.actions  <- user$actions[i]   + user.actions
                    if (user$correct[i] == "TRUE") user.correct  <- user.correct + 1
                }
            }
            if(((user.speed/1000)/4) < 200) {
                ls.id        <- c(ls.id,       toString(user.id))
                ls.viz       <- c(ls.viz,      toString(user.viz))
                ls.speed     <- c(ls.speed,    (user.speed/1000)/4)
                ls.actions   <- c(ls.actions,  user.actions/4)
                ls.accuracy  <- c(ls.accuracy, user.correct)
            }
        }
    }
    
    ls.accuracy <- ls.accuracy/4
    
    output <- data.frame(ls.id,            
                         ls.viz,            
                         ls.speed,
                         ls.actions,
                         ls.accuracy)
    
    return(output)
}

### Main process...

# Clean study data.
clean.study.charts <- add.difficulty(study.charts)
clean.study.dots   <- add.difficulty(study.dots)
clean.study.series <- add.difficulty(study.series)

# Create data frames for boxplot.
charts.boxplot.easy   <- create.boxplot(clean.study.charts, "easy")
series.boxplot.easy   <- create.boxplot(clean.study.series, "easy")
dots.boxplot.easy     <- create.boxplot(clean.study.dots,   "easy")

series.boxplot.medium <- create.boxplot(clean.study.series, "medium")
charts.boxplot.medium <- create.boxplot(clean.study.charts, "medium")
dots.boxplot.medium   <- create.boxplot(clean.study.dots,   "medium")

charts.boxplot.hard   <- create.boxplot(clean.study.charts, "hard")
series.boxplot.hard   <- create.boxplot(clean.study.series, "hard")
dots.boxplot.hard     <- create.boxplot(clean.study.dots,   "hard")

#############################################################
easy.speed <- c()
easy.viz   <- c()
easy.speed <- c(easy.speed, charts.boxplot.easy$ls.speed)
easy.viz   <- c(easy.viz,   as.vector(charts.boxplot.easy$ls.viz))
easy.speed <- c(easy.speed, series.boxplot.easy$ls.speed)
easy.viz   <- c(easy.viz,   as.vector(series.boxplot.easy$ls.viz))
easy.speed <- c(easy.speed, dots.boxplot.easy$ls.speed)
easy.viz   <- c(easy.viz,   as.vector(dots.boxplot.easy$ls.viz))
#############################################################
medium.speed <- c()
medium.viz   <- c()
medium.speed <- c(medium.speed, charts.boxplot.medium$ls.speed)
medium.viz   <- c(medium.viz,   as.vector(charts.boxplot.medium$ls.viz))
medium.speed <- c(medium.speed, series.boxplot.medium$ls.speed)
medium.viz   <- c(medium.viz,   as.vector(series.boxplot.medium$ls.viz))
medium.speed <- c(medium.speed, dots.boxplot.medium$ls.speed)
medium.viz   <- c(medium.viz,   as.vector(dots.boxplot.medium$ls.viz))
#############################################################
hard.speed <- c()
hard.viz   <- c()
hard.speed <- c(hard.speed, charts.boxplot.hard$ls.speed)
hard.viz   <- c(hard.viz,   as.vector(charts.boxplot.hard$ls.viz))
hard.speed <- c(hard.speed, series.boxplot.hard$ls.speed)
hard.viz   <- c(hard.viz,   as.vector(series.boxplot.hard$ls.viz))
hard.speed <- c(hard.speed, dots.boxplot.hard$ls.speed)
hard.viz   <- c(hard.viz,   as.vector(dots.boxplot.hard$ls.viz))
#############################################################
easy.actions <- c()
easy.viz     <- c()
easy.actions <- c(easy.actions, charts.boxplot.easy$ls.actions)
easy.viz     <- c(easy.viz,     as.vector(charts.boxplot.easy$ls.viz))
easy.actions <- c(easy.actions, series.boxplot.easy$ls.actions)
easy.viz     <- c(easy.viz,     as.vector(series.boxplot.easy$ls.viz))
easy.actions <- c(easy.actions, dots.boxplot.easy$ls.actions)
easy.viz     <- c(easy.viz,     as.vector(dots.boxplot.easy$ls.viz))
#############################################################
medium.actions <- c()
medium.viz     <- c()
medium.actions <- c(medium.actions, charts.boxplot.medium$ls.actions)
medium.viz     <- c(medium.viz,     as.vector(charts.boxplot.medium$ls.viz))
medium.actions <- c(medium.actions, series.boxplot.medium$ls.actions)
medium.viz     <- c(medium.viz,     as.vector(series.boxplot.medium$ls.viz))
medium.actions <- c(medium.actions, dots.boxplot.medium$ls.actions)
medium.viz     <- c(medium.viz,     as.vector(dots.boxplot.medium$ls.viz))
#############################################################
hard.actions <- c()
hard.viz     <- c()
hard.actions <- c(hard.actions, charts.boxplot.hard$ls.actions)
hard.viz     <- c(hard.viz,     as.vector(charts.boxplot.hard$ls.viz))
hard.actions <- c(hard.actions, series.boxplot.hard$ls.actions)
hard.viz     <- c(hard.viz,     as.vector(series.boxplot.hard$ls.viz))
hard.actions <- c(hard.actions, dots.boxplot.hard$ls.actions)
hard.viz     <- c(hard.viz,     as.vector(dots.boxplot.hard$ls.viz))
#############################################################
easy.accuracy <- c()
easy.viz      <- c()
easy.accuracy <- c(easy.accuracy, charts.boxplot.easy$ls.accuracy)
easy.viz      <- c(easy.viz,      as.vector(charts.boxplot.easy$ls.viz))
easy.accuracy <- c(easy.accuracy, series.boxplot.easy$ls.accuracy)
easy.viz      <- c(easy.viz,      as.vector(series.boxplot.easy$ls.viz))
easy.accuracy <- c(easy.accuracy, dots.boxplot.easy$ls.accuracy)
easy.viz      <- c(easy.viz,      as.vector(dots.boxplot.easy$ls.viz))
#############################################################
medium.accuracy <- c()
medium.viz      <- c()
medium.accuracy <- c(medium.accuracy, charts.boxplot.medium$ls.accuracy)
medium.viz      <- c(medium.viz,      as.vector(charts.boxplot.medium$ls.viz))
medium.accuracy <- c(medium.accuracy, series.boxplot.medium$ls.accuracy)
medium.viz      <- c(medium.viz,      as.vector(series.boxplot.medium$ls.viz))
medium.accuracy <- c(medium.accuracy, dots.boxplot.medium$ls.accuracy)
medium.viz      <- c(medium.viz,      as.vector(dots.boxplot.medium$ls.viz))
#############################################################
hard.accuracy <- c()
hard.viz      <- c()
hard.accuracy <- c(hard.accuracy, charts.boxplot.hard$ls.accuracy)
hard.viz      <- c(hard.viz,      as.vector(charts.boxplot.hard$ls.viz))
hard.accuracy <- c(hard.accuracy, series.boxplot.hard$ls.accuracy)
hard.viz      <- c(hard.viz,      as.vector(series.boxplot.hard$ls.viz))
hard.accuracy <- c(hard.accuracy, dots.boxplot.hard$ls.accuracy)
hard.viz      <- c(hard.viz,      as.vector(dots.boxplot.hard$ls.viz))
#############################################################

df.speed.easy      <- data.frame(easy.viz,   easy.speed) 
df.speed.medium    <- data.frame(medium.viz, medium.speed) 
df.speed.hard      <- data.frame(hard.viz,   hard.speed) 
df.actions.easy    <- data.frame(easy.viz,   easy.actions) 
df.actions.medium  <- data.frame(medium.viz, medium.actions) 
df.actions.hard    <- data.frame(hard.viz,   hard.actions) 
df.accuracy.easy   <- data.frame(easy.viz,   easy.accuracy) 
df.accuracy.medium <- data.frame(medium.viz, medium.accuracy) 
df.accuracy.hard   <- data.frame(hard.viz,   hard.accuracy) 


lm.speed.easy.anova      <- anova(lm(easy.speed      ~ easy.viz,   data=df.speed.easy))
lm.speed.medium.anova    <- anova(lm(medium.speed    ~ medium.viz, data=df.speed.medium))
lm.speed.hard.anova      <- anova(lm(hard.speed      ~ hard.viz,   data=df.speed.hard))
lm.actions.easy.anova    <- anova(lm(easy.actions    ~ easy.viz,   data=df.actions.easy))
lm.actions.medium.anova  <- anova(lm(medium.actions  ~ medium.viz, data=df.actions.medium))
lm.actions.hard.anova    <- anova(lm(hard.actions    ~ hard.viz,   data=df.actions.hard))
lm.accuracy.easy.anova   <- anova(lm(easy.accuracy   ~ easy.viz,   data=df.accuracy.easy))
lm.accuracy.medium.anova <- anova(lm(medium.accuracy ~ medium.viz, data=df.accuracy.medium))
lm.accuracy.hard.anova   <- anova(lm(hard.accuracy   ~ hard.viz,   data=df.accuracy.hard))

leveneTest(easy.speed     , easy.viz)
leveneTest(medium.speed   , medium.viz)
leveneTest(hard.speed     , hard.viz) 
leveneTest(easy.actions   , easy.viz) 
leveneTest(medium.actions , medium.viz)
leveneTest(hard.actions   , hard.viz)   
leveneTest(easy.accuracy  , easy.viz)   
leveneTest(medium.accuracy, medium.viz) 
leveneTest(hard.accuracy  , hard.viz)   

speed.easy.anova      <- aov(easy.speed      ~ easy.viz,   data=df.speed.easy)
speed.medium.anova    <- aov(medium.speed    ~ medium.viz, data=df.speed.medium)
speed.hard.anova      <- aov(hard.speed      ~ hard.viz,   data=df.speed.hard)
actions.easy.anova    <- aov(easy.actions    ~ easy.viz,   data=df.actions.easy)
actions.medium.anova  <- aov(medium.actions  ~ medium.viz, data=df.actions.medium)
actions.hard.anova    <- aov(hard.actions    ~ hard.viz,   data=df.actions.hard)
accuracy.easy.anova   <- aov(easy.accuracy   ~ easy.viz,   data=df.accuracy.easy)
accuracy.medium.anova <- aov(medium.accuracy ~ medium.viz, data=df.accuracy.medium)
accuracy.hard.anova   <- aov(hard.accuracy   ~ hard.viz,   data=df.accuracy.hard)

# Post Hoc Study
tukey.speed.easy      <- TukeyHSD(speed.easy.anova)
tukey.speed.medium    <- TukeyHSD(speed.medium.anova)
tukey.speed.hard      <- TukeyHSD(speed.hard.anova)
tukey.actions.easy    <- TukeyHSD(actions.easy.anova)
tukey.actions.medium  <- TukeyHSD(actions.medium.anova)
tukey.actions.hard    <- TukeyHSD(actions.hard.anova)
tukey.accuracy.easy   <- TukeyHSD(accuracy.easy.anova)
tukey.accuracy.medium <- TukeyHSD(accuracy.medium.anova)
tukey.accuracy.hard   <- TukeyHSD(accuracy.hard.anova)

# Bonferroni correction for post hoc study...
# p.adjust(tukey.speed.easy$easy.viz[,4],        method = "holm")
# p.adjust(tukey.speed.medium$medium.viz[,4],    method = "holm")
# p.adjust(tukey.speed.hard$hard.viz[,4],        method = "holm")
# p.adjust(tukey.actions.easy$easy.viz[,4],      method = "holm")
# p.adjust(tukey.actions.medium$medium.viz[,4],  method = "holm")
# p.adjust(tukey.actions.hard$hard.viz[,4],      method = "holm")
# p.adjust(tukey.accuracy.easy$easy.viz[,4],     method = "holm")
# p.adjust(tukey.accuracy.medium$medium.viz[,4], method = "holm")
# p.adjust(tukey.accuracy.hard$hard.viz[,4],     method = "holm")

# Do the T-test for each pair...
t.speed.easy.dots.charts        <- t.test(dots.boxplot.easy$ls.speed,        charts.boxplot.easy$ls.speed)
t.speed.easy.series.charts      <- t.test(series.boxplot.easy$ls.speed,      charts.boxplot.easy$ls.speed)
t.speed.easy.series.dots        <- t.test(series.boxplot.easy$ls.speed,      dots.boxplot.easy$ls.speed)
t.speed.medium.dots.charts      <- t.test(dots.boxplot.medium$ls.speed,      charts.boxplot.medium$ls.speed)
t.speed.medium.series.charts    <- t.test(series.boxplot.medium$ls.speed,    charts.boxplot.medium$ls.speed)
t.speed.medium.series.dots      <- t.test(series.boxplot.medium$ls.speed,    dots.boxplot.medium$ls.speed)
t.speed.hard.dots.charts        <- t.test(dots.boxplot.hard$ls.speed,        charts.boxplot.hard$ls.speed)
t.speed.hard.series.charts      <- t.test(series.boxplot.hard$ls.speed,      charts.boxplot.hard$ls.speed)
t.speed.hard.series.dots        <- t.test(series.boxplot.hard$ls.speed,      dots.boxplot.hard$ls.speed)
t.actions.easy.dots.charts      <- t.test(dots.boxplot.easy$ls.actions,      charts.boxplot.easy$ls.actions)
t.actions.easy.series.charts    <- t.test(series.boxplot.easy$ls.actions,    charts.boxplot.easy$ls.actions)
t.actions.easy.series.dots      <- t.test(series.boxplot.easy$ls.actions,    dots.boxplot.easy$ls.actions)
t.actions.medium.dots.charts    <- t.test(dots.boxplot.medium$ls.actions,    charts.boxplot.medium$ls.actions)
t.actions.medium.series.charts  <- t.test(series.boxplot.medium$ls.actions,  charts.boxplot.medium$ls.actions)
t.actions.medium.series.dots    <- t.test(series.boxplot.medium$ls.actions,  dots.boxplot.medium$ls.actions)
t.actions.hard.dots.charts      <- t.test(dots.boxplot.hard$ls.actions,      charts.boxplot.hard$ls.actions)
t.actions.hard.series.charts    <- t.test(series.boxplot.hard$ls.actions,    charts.boxplot.hard$ls.actions)
t.actions.hard.series.dots      <- t.test(series.boxplot.hard$ls.actions,    dots.boxplot.hard$ls.actions)
t.accuracy.easy.dots.charts     <- t.test(dots.boxplot.easy$ls.accuracy,     charts.boxplot.easy$ls.accuracy)
t.accuracy.easy.series.charts   <- t.test(series.boxplot.easy$ls.accuracy,   charts.boxplot.easy$ls.accuracy)
t.accuracy.easy.series.dots     <- t.test(series.boxplot.easy$ls.accuracy,   dots.boxplot.easy$ls.accuracy)
t.accuracy.medium.dots.charts   <- t.test(dots.boxplot.medium$ls.accuracy,   charts.boxplot.medium$ls.accuracy)
t.accuracy.medium.series.charts <- t.test(series.boxplot.medium$ls.accuracy, charts.boxplot.medium$ls.accuracy)
t.accuracy.medium.series.dots   <- t.test(series.boxplot.medium$ls.accuracy, dots.boxplot.medium$ls.accuracy)
t.accuracy.hard.dots.charts     <- t.test(dots.boxplot.hard$ls.accuracy,     charts.boxplot.hard$ls.accuracy)
t.accuracy.hard.series.charts   <- t.test(series.boxplot.hard$ls.accuracy,   charts.boxplot.hard$ls.accuracy)
t.accuracy.hard.series.dots     <- t.test(series.boxplot.hard$ls.accuracy,   dots.boxplot.hard$ls.accuracy)

# Mann-Whitney test...
#
s.speed.easy.dots.charts        <- as.data.frame(cbind(dots.boxplot.easy$ls.speed,        charts.boxplot.easy$ls.speed))
s.speed.easy.series.charts      <- as.data.frame(cbind(series.boxplot.easy$ls.speed,      charts.boxplot.easy$ls.speed))
s.speed.easy.series.dots        <- as.data.frame(cbind(series.boxplot.easy$ls.speed,      dots.boxplot.easy$ls.speed))
s.speed.medium.dots.charts      <- as.data.frame(cbind(dots.boxplot.medium$ls.speed,      charts.boxplot.medium$ls.speed))
s.speed.medium.series.charts    <- as.data.frame(cbind(series.boxplot.medium$ls.speed,    charts.boxplot.medium$ls.speed))
s.speed.medium.series.dots      <- as.data.frame(cbind(series.boxplot.medium$ls.speed,    dots.boxplot.medium$ls.speed))
s.speed.hard.dots.charts        <- as.data.frame(cbind(dots.boxplot.hard$ls.speed,        charts.boxplot.hard$ls.speed))
s.speed.hard.series.charts      <- as.data.frame(cbind(series.boxplot.hard$ls.speed,      charts.boxplot.hard$ls.speed))
s.speed.hard.series.dots        <- as.data.frame(cbind(series.boxplot.hard$ls.speed,      dots.boxplot.hard$ls.speed))
s.actions.easy.dots.charts      <- as.data.frame(cbind(series.boxplot.easy$ls.actions,      charts.boxplot.easy$ls.actions))
s.actions.easy.series.charts    <- as.data.frame(cbind(series.boxplot.easy$ls.actions,    charts.boxplot.easy$ls.actions))
s.actions.easy.series.dots      <- as.data.frame(cbind(series.boxplot.easy$ls.actions,    dots.boxplot.easy$ls.actions))
s.actions.medium.dots.charts    <- as.data.frame(cbind(dots.boxplot.medium$ls.actions,    charts.boxplot.medium$ls.actions))
s.actions.medium.series.charts  <- as.data.frame(cbind(series.boxplot.medium$ls.actions,  charts.boxplot.medium$ls.actions))
s.actions.medium.series.dots    <- as.data.frame(cbind(series.boxplot.medium$ls.actions,  dots.boxplot.medium$ls.actions))
s.actions.hard.dots.charts      <- as.data.frame(cbind(dots.boxplot.hard$ls.actions,      charts.boxplot.hard$ls.actions))
s.actions.hard.series.charts    <- as.data.frame(cbind(series.boxplot.hard$ls.actions,    charts.boxplot.hard$ls.actions))
s.actions.hard.series.dots      <- as.data.frame(cbind(series.boxplot.hard$ls.actions,    dots.boxplot.hard$ls.actions))
s.accuracy.easy.dots.charts     <- as.data.frame(cbind(dots.boxplot.easy$ls.accuracy,     charts.boxplot.easy$ls.accuracy))
s.accuracy.easy.series.charts   <- as.data.frame(cbind(series.boxplot.easy$ls.accuracy,   charts.boxplot.easy$ls.accuracy))
s.accuracy.easy.series.dots     <- as.data.frame(cbind(series.boxplot.easy$ls.accuracy,   dots.boxplot.easy$ls.accuracy))
s.accuracy.medium.dots.charts   <- as.data.frame(cbind(dots.boxplot.medium$ls.accuracy,   charts.boxplot.medium$ls.accuracy))
s.accuracy.medium.series.charts <- as.data.frame(cbind(series.boxplot.medium$ls.accuracy, charts.boxplot.medium$ls.accuracy))
s.accuracy.medium.series.dots   <- as.data.frame(cbind(series.boxplot.medium$ls.accuracy, dots.boxplot.medium$ls.accuracy))
s.accuracy.hard.dots.charts     <- as.data.frame(cbind(dots.boxplot.hard$ls.accuracy,     charts.boxplot.hard$ls.accuracy))
s.accuracy.hard.series.charts   <- as.data.frame(cbind(series.boxplot.hard$ls.accuracy,   charts.boxplot.hard$ls.accuracy))
s.accuracy.hard.series.dots     <- as.data.frame(cbind(series.boxplot.hard$ls.accuracy,   dots.boxplot.hard$ls.accuracy))

s.speed.easy.dots.charts        <- melt(s.speed.easy.dots.charts)       
s.speed.easy.series.charts      <- melt(s.speed.easy.series.charts)
s.speed.easy.series.dots        <- melt(s.speed.easy.series.dots)
s.speed.medium.dots.charts      <- melt(s.speed.medium.dots.charts)
s.speed.medium.series.charts    <- melt(s.speed.medium.series.charts)
s.speed.medium.series.dots      <- melt(s.speed.medium.series.dots)  
s.speed.hard.dots.charts        <- melt(s.speed.hard.dots.charts)
s.speed.hard.series.charts      <- melt(s.speed.hard.series.charts)
s.speed.hard.series.dots        <- melt(s.speed.hard.series.dots)
s.actions.easy.dots.charts      <- melt(s.actions.easy.dots.charts)
s.actions.easy.series.charts    <- melt(s.actions.easy.series.charts)
s.actions.easy.series.dots      <- melt(s.actions.easy.series.dots)
s.actions.medium.dots.charts    <- melt(s.actions.medium.dots.charts)
s.actions.medium.series.charts  <- melt(s.actions.medium.series.charts)
s.actions.medium.series.dots    <- melt(s.actions.medium.series.dots)
s.actions.hard.dots.charts      <- melt(s.actions.hard.dots.charts)
s.actions.hard.series.charts    <- melt(s.actions.hard.series.charts)
s.actions.hard.series.dots      <- melt(s.actions.hard.series.dots)
s.accuracy.easy.dots.charts     <- melt(s.accuracy.easy.dots.charts)
s.accuracy.easy.series.charts   <- melt(s.accuracy.easy.series.charts)
s.accuracy.easy.series.dots     <- melt(s.accuracy.easy.series.dots)
s.accuracy.medium.dots.charts   <- melt(s.accuracy.medium.dots.charts)
s.accuracy.medium.series.charts <- melt(s.accuracy.medium.series.charts)
s.accuracy.medium.series.dots   <- melt(s.accuracy.medium.series.dots)
s.accuracy.hard.dots.charts     <- melt(s.accuracy.hard.dots.charts)
s.accuracy.hard.series.charts   <- melt(s.accuracy.hard.series.charts)
s.accuracy.hard.series.dots     <- melt(s.accuracy.hard.series.dots)

# Mann-Whitney test...
w.speed.easy.dots.charts        <- wilcox.test(s.speed.easy.dots.charts$value        ~ s.speed.easy.dots.charts$variable       , data=s.speed.easy.dots.charts       )
w.speed.easy.series.charts      <- wilcox.test(s.speed.easy.series.charts$value      ~ s.speed.easy.series.charts$variable     , data=s.speed.easy.series.charts     )
w.speed.easy.series.dots        <- wilcox.test(s.speed.easy.series.dots$value        ~ s.speed.easy.series.dots$variable       , data=s.speed.easy.series.dots       )
w.speed.medium.dots.charts      <- wilcox.test(s.speed.medium.dots.charts$value      ~ s.speed.medium.dots.charts$variable     , data=s.speed.medium.dots.charts     )
w.speed.medium.series.charts    <- wilcox.test(s.speed.medium.series.charts$value    ~ s.speed.medium.series.charts$variable   , data=s.speed.medium.series.charts   )
w.speed.medium.series.dots      <- wilcox.test(s.speed.medium.series.dots$value      ~ s.speed.medium.series.dots$variable     , data=s.speed.medium.series.dots     )
w.speed.hard.dots.charts        <- wilcox.test(s.speed.hard.dots.charts$value        ~ s.speed.hard.dots.charts$variable       , data=s.speed.hard.dots.charts       )
w.speed.hard.series.charts      <- wilcox.test(s.speed.hard.series.charts$value      ~ s.speed.hard.series.charts$variable     , data=s.speed.hard.series.charts     )
w.speed.hard.series.dots        <- wilcox.test(s.speed.hard.series.dots$value        ~ s.speed.hard.series.dots$variable       , data=s.speed.hard.series.dots       )
w.actions.easy.dots.charts      <- wilcox.test(s.actions.easy.dots.charts$value      ~ s.actions.easy.dots.charts$variable     , data=s.actions.easy.dots.charts     )
w.actions.easy.series.charts    <- wilcox.test(s.actions.easy.series.charts$value    ~ s.actions.easy.series.charts$variable   , data=s.actions.easy.series.charts   )
w.actions.easy.series.dots      <- wilcox.test(s.actions.easy.series.dots$value      ~ s.actions.easy.series.dots$variable     , data=s.actions.easy.series.dots     )
w.actions.medium.dots.charts    <- wilcox.test(s.actions.medium.dots.charts$value    ~ s.actions.medium.dots.charts$variable   , data=s.actions.medium.dots.charts   )
w.actions.medium.series.charts  <- wilcox.test(s.actions.medium.series.charts$value  ~ s.actions.medium.series.charts$variable , data=s.actions.medium.series.charts )
w.actions.medium.series.dots    <- wilcox.test(s.actions.medium.series.dots$value    ~ s.actions.medium.series.dots$variable   , data=s.actions.medium.series.dots   )
w.actions.hard.dots.charts      <- wilcox.test(s.actions.hard.dots.charts$value      ~ s.actions.hard.dots.charts$variable     , data=s.actions.hard.dots.charts     )
w.actions.hard.series.charts    <- wilcox.test(s.actions.hard.series.charts$value    ~ s.actions.hard.series.charts$variable   , data=s.actions.hard.series.charts   )
w.actions.hard.series.dots      <- wilcox.test(s.actions.hard.series.dots$value      ~ s.actions.hard.series.dots$variable     , data=s.actions.hard.series.dots     )
w.accuracy.easy.dots.charts     <- wilcox.test(s.accuracy.easy.dots.charts$value     ~ s.accuracy.easy.dots.charts$variable    , data=s.accuracy.easy.dots.charts    )
w.accuracy.easy.series.charts   <- wilcox.test(s.accuracy.easy.series.charts$value   ~ s.accuracy.easy.series.charts$variable  , data=s.accuracy.easy.series.charts  )
w.accuracy.easy.series.dots     <- wilcox.test(s.accuracy.easy.series.dots$value     ~ s.accuracy.easy.series.dots$variable    , data=s.accuracy.easy.series.dots    )
w.accuracy.medium.dots.charts   <- wilcox.test(s.accuracy.medium.dots.charts$value   ~ s.accuracy.medium.dots.charts$variable  , data=s.accuracy.medium.dots.charts  )
w.accuracy.medium.series.charts <- wilcox.test(s.accuracy.medium.series.charts$value ~ s.accuracy.medium.series.charts$variable, data=s.accuracy.medium.series.charts)
w.accuracy.medium.series.dots   <- wilcox.test(s.accuracy.medium.series.dots$value   ~ s.accuracy.medium.series.dots$variable  , data=s.accuracy.medium.series.dots  )
w.accuracy.hard.dots.charts     <- wilcox.test(s.accuracy.hard.dots.charts$value     ~ s.accuracy.hard.dots.charts$variable    , data=s.accuracy.hard.dots.charts    )
w.accuracy.hard.series.charts   <- wilcox.test(s.accuracy.hard.series.charts$value   ~ s.accuracy.hard.series.charts$variable  , data=s.accuracy.hard.series.charts  )
w.accuracy.hard.series.dots     <- wilcox.test(s.accuracy.hard.series.dots$value     ~ s.accuracy.hard.series.dots$variable    , data=s.accuracy.hard.series.dots    )

## LEVENES TEST!
l.speed.easy.dots.charts        <- leveneTest(s.speed.easy.dots.charts$value        ~ s.speed.easy.dots.charts$variable       , data=s.speed.easy.dots.charts       )
l.speed.easy.series.charts      <- leveneTest(s.speed.easy.series.charts$value      ~ s.speed.easy.series.charts$variable     , data=s.speed.easy.series.charts     )
l.speed.easy.series.dots        <- leveneTest(s.speed.easy.series.dots$value        ~ s.speed.easy.series.dots$variable       , data=s.speed.easy.series.dots       )
l.speed.medium.dots.charts      <- leveneTest(s.speed.medium.dots.charts$value      ~ s.speed.medium.dots.charts$variable     , data=s.speed.medium.dots.charts     )
l.speed.medium.series.charts    <- leveneTest(s.speed.medium.series.charts$value    ~ s.speed.medium.series.charts$variable   , data=s.speed.medium.series.charts   )
l.speed.medium.series.dots      <- leveneTest(s.speed.medium.series.dots$value      ~ s.speed.medium.series.dots$variable     , data=s.speed.medium.series.dots     )
l.speed.hard.dots.charts        <- leveneTest(s.speed.hard.dots.charts$value        ~ s.speed.hard.dots.charts$variable       , data=s.speed.hard.dots.charts       )
l.speed.hard.series.charts      <- leveneTest(s.speed.hard.series.charts$value      ~ s.speed.hard.series.charts$variable     , data=s.speed.hard.series.charts     )
l.speed.hard.series.dots        <- leveneTest(s.speed.hard.series.dots$value        ~ s.speed.hard.series.dots$variable       , data=s.speed.hard.series.dots       )
l.actions.easy.dots.charts      <- leveneTest(s.actions.easy.dots.charts$value      ~ s.actions.easy.dots.charts$variable     , data=s.actions.easy.dots.charts     )
l.actions.easy.series.charts    <- leveneTest(s.actions.easy.series.charts$value    ~ s.actions.easy.series.charts$variable   , data=s.actions.easy.series.charts   )
l.actions.easy.series.dots      <- leveneTest(s.actions.easy.series.dots$value      ~ s.actions.easy.series.dots$variable     , data=s.actions.easy.series.dots     )
l.actions.medium.dots.charts    <- leveneTest(s.actions.medium.dots.charts$value    ~ s.actions.medium.dots.charts$variable   , data=s.actions.medium.dots.charts   )
l.actions.medium.series.charts  <- leveneTest(s.actions.medium.series.charts$value  ~ s.actions.medium.series.charts$variable , data=s.actions.medium.series.charts )
l.actions.medium.series.dots    <- leveneTest(s.actions.medium.series.dots$value    ~ s.actions.medium.series.dots$variable   , data=s.actions.medium.series.dots   )
l.actions.hard.dots.charts      <- leveneTest(s.actions.hard.dots.charts$value      ~ s.actions.hard.dots.charts$variable     , data=s.actions.hard.dots.charts     )
l.actions.hard.series.charts    <- leveneTest(s.actions.hard.series.charts$value    ~ s.actions.hard.series.charts$variable   , data=s.actions.hard.series.charts   )
l.actions.hard.series.dots      <- leveneTest(s.actions.hard.series.dots$value      ~ s.actions.hard.series.dots$variable     , data=s.actions.hard.series.dots     )
l.accuracy.easy.dots.charts     <- leveneTest(s.accuracy.easy.dots.charts$value     ~ s.accuracy.easy.dots.charts$variable    , data=s.accuracy.easy.dots.charts    )
l.accuracy.easy.series.charts   <- leveneTest(s.accuracy.easy.series.charts$value   ~ s.accuracy.easy.series.charts$variable  , data=s.accuracy.easy.series.charts  )
l.accuracy.easy.series.dots     <- leveneTest(s.accuracy.easy.series.dots$value     ~ s.accuracy.easy.series.dots$variable    , data=s.accuracy.easy.series.dots    )
l.accuracy.medium.dots.charts   <- leveneTest(s.accuracy.medium.dots.charts$value   ~ s.accuracy.medium.dots.charts$variable  , data=s.accuracy.medium.dots.charts  )
l.accuracy.medium.series.charts <- leveneTest(s.accuracy.medium.series.charts$value ~ s.accuracy.medium.series.charts$variable, data=s.accuracy.medium.series.charts)
l.accuracy.medium.series.dots   <- leveneTest(s.accuracy.medium.series.dots$value   ~ s.accuracy.medium.series.dots$variable  , data=s.accuracy.medium.series.dots  )
l.accuracy.hard.dots.charts     <- leveneTest(s.accuracy.hard.dots.charts$value     ~ s.accuracy.hard.dots.charts$variable    , data=s.accuracy.hard.dots.charts    )
l.accuracy.hard.series.charts   <- leveneTest(s.accuracy.hard.series.charts$value   ~ s.accuracy.hard.series.charts$variable  , data=s.accuracy.hard.series.charts  )
l.accuracy.hard.series.dots     <- leveneTest(s.accuracy.hard.series.dots$value     ~ s.accuracy.hard.series.dots$variable    , data=s.accuracy.hard.series.dots    )

#par(bty='n', mfrow=c(1,1), las=1)
#plot(tukey)

#series.boxplot.medium$ls.speed   
#charts.boxplot.medium$ls.speed   
#dots.boxplot.medium$ls.speed   
    
#charts.boxplot.hard$ls.speed   
#series.boxplot.hard$ls.speed   
#dots.boxplot.hard$ls.speed   


conf.interval <- function(data) {
    data <- t.test(data)
    mean   <- data$estimate
    min    <- data$conf.int[1]
    max    <- data$conf.int[2]
    output <- data.frame(mean,min,max)
    return(output)
}

t2.aspect    <- c("e.acc","e.speed", "m.actions","m.acc","m.speed", "m.actions", "h.acc","h.speed", "h.actions")
t2.intuitive <- c(mean(series.boxplot.easy$ls.accuracy), mean(series.boxplot.easy$ls.speed), mean(series.boxplot.easy$ls.actions), mean(series.boxplot.medium$ls.accuracy), mean(series.boxplot.medium$ls.speed), mean(series.boxplot.medium$ls.actions), mean(series.boxplot.hard$ls.accuracy), mean(series.boxplot.hard$ls.speed), mean(series.boxplot.hard$ls.actions))
t2.detailed  <- c(mean(charts.boxplot.easy$ls.accuracy), mean(charts.boxplot.easy$ls.speed), mean(charts.boxplot.easy$ls.actions), mean(charts.boxplot.medium$ls.accuracy), mean(charts.boxplot.medium$ls.speed), mean(charts.boxplot.medium$ls.actions), mean(charts.boxplot.hard$ls.accuracy), mean(charts.boxplot.hard$ls.speed), mean(charts.boxplot.hard$ls.actions))
t2.compact   <- c(mean(dots.boxplot.easy$ls.accuracy)  , mean(dots.boxplot.easy$ls.speed),   mean(dots.boxplot.easy$ls.actions),   mean(dots.boxplot.medium$ls.accuracy),   mean(dots.boxplot.medium$ls.speed),   mean(dots.boxplot.medium$ls.actions  ), mean(dots.boxplot.hard$ls.accuracy),   mean(dots.boxplot.hard$ls.speed),   mean(dots.boxplot.hard$ls.actions  ))

table2 <- data.frame(t2.aspect, t2.intuitive, t2.detailed, t2.compact)


e.acc.int <- conf.interval(series.boxplot.easy$ls.accuracy)
e.acc.det <- conf.interval(charts.boxplot.easy$ls.accuracy)
e.acc.com <- conf.interval(dots.boxplot.easy$ls.accuracy)
m.acc.int <- conf.interval(series.boxplot.medium$ls.accuracy)
m.acc.det <- conf.interval(charts.boxplot.medium$ls.accuracy)
m.acc.com <- conf.interval(dots.boxplot.medium$ls.accuracy  )
h.acc.int <- conf.interval(series.boxplot.hard$ls.accuracy)
h.acc.det <- conf.interval(charts.boxplot.hard$ls.accuracy)
h.acc.com <- conf.interval(dots.boxplot.hard$ls.accuracy  )

ggplot(e.acc.int, aes(x = "Intuitive", y = mean)) + 
geom_point(data = e.acc.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
geom_point(data = e.acc.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
geom_point(data = e.acc.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
geom_errorbar(data = e.acc.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
geom_errorbar(data = e.acc.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
geom_errorbar(data = e.acc.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
coord_flip() +
labs(title ="", x = "Easy", y = "Accuracy")

ggplot(m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = m.acc.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = m.acc.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = m.acc.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = m.acc.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.acc.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.acc.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Accuracy")


ggplot(h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = h.acc.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = h.acc.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = h.acc.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = h.acc.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.acc.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.acc.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Accuracy")


e.act.int <- conf.interval(series.boxplot.easy$ls.actions)
e.act.det <- conf.interval(charts.boxplot.easy$ls.actions)
e.act.com <- conf.interval(dots.boxplot.easy$ls.actions)
m.act.int <- conf.interval(series.boxplot.medium$ls.actions)
m.act.det <- conf.interval(charts.boxplot.medium$ls.actions)
m.act.com <- conf.interval(dots.boxplot.medium$ls.actions)   
h.act.int <- conf.interval(series.boxplot.hard$ls.actions)
h.act.det <- conf.interval(charts.boxplot.hard$ls.actions)
h.act.com <- conf.interval(dots.boxplot.hard$ls.actions)

ggplot(e.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = e.act.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = e.act.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = e.act.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = e.act.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = e.act.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = e.act.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Easy", y = "Actions")

ggplot(m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = m.act.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = m.act.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = m.act.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = m.act.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.act.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.act.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Actions")


ggplot(h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = h.act.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = h.act.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = h.act.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = h.act.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.act.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.act.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Actions")

# ggsave("plot.png", width=3.5, height=1.5, dpi=300)

e.speed.int <- conf.interval(series.boxplot.easy$ls.speed)
e.speed.det <- conf.interval(charts.boxplot.easy$ls.speed)
e.speed.com <- conf.interval(dots.boxplot.easy$ls.speed)
m.speed.int <- conf.interval(series.boxplot.medium$ls.speed)
m.speed.det <- conf.interval(charts.boxplot.medium$ls.speed)
m.speed.com <- conf.interval(dots.boxplot.medium$ls.speed) 
h.speed.int <- conf.interval(series.boxplot.hard$ls.speed)
h.speed.det <- conf.interval(charts.boxplot.hard$ls.speed)
h.speed.com <- conf.interval(dots.boxplot.hard$ls.speed)   

ggplot(e.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = e.speed.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = e.speed.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = e.speed.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = e.speed.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = e.speed.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = e.speed.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Easy", y = "Speed")

ggplot(m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = m.speed.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = m.speed.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = m.speed.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = m.speed.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.speed.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = m.speed.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Speed")


ggplot(h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data = h.speed.int, aes(x="Intuitive",y = mean), color = "#08519c") + 
    geom_point(data = h.speed.det, aes(x="Detailed" ,y = mean), color = "#08519c") + 
    geom_point(data = h.speed.com, aes(x="Compact"  ,y = mean), color = "#08519c") + 
    geom_errorbar(data = h.speed.int, aes(x="Intuitive", ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.speed.det, aes(x="Detailed" , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    geom_errorbar(data = h.speed.com, aes(x="Compact"  , ymax = max, ymin = min), width=0.1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Speed")


write.table(series.boxplot.easy   , "s2.series.boxplot.easy.csv"  , sep=",")
write.table(charts.boxplot.easy   , "s2.charts.boxplot.easy.csv"  , sep=",")
write.table(dots.boxplot.easy     , "s2.dots.boxplot.easy.csv"    , sep=",")
write.table(series.boxplot.medium , "s2.series.boxplot.medium.csv", sep=",")
write.table(charts.boxplot.medium , "s2.charts.boxplot.medium.csv", sep=",")
write.table(dots.boxplot.medium   , "s2.dots.boxplot.medium.csv"  , sep=",")
write.table(series.boxplot.hard   , "s2.series.boxplot.hard.csv"  , sep=",")
write.table(charts.boxplot.hard   , "s2.charts.boxplot.hard.csv"  , sep=",") 
write.table(dots.boxplot.hard     , "s2.dots.boxplot.hard.csv"    , sep=",")





t.test(series.boxplot.easy$ls.accuracy)
t.test(series.boxplot.medium$ls.accuracy)
t.test(series.boxplot.hard$ls.accuracy)
t.test(charts.boxplot.easy$ls.accuracy)
t.test(charts.boxplot.medium$ls.accuracy)
t.test(charts.boxplot.hard$ls.accuracy)
t.test(dots.boxplot.easy$ls.accuracy)
t.test(dots.boxplot.medium$ls.accuracy)
t.test(dots.boxplot.hard$ls.accuracy)

t.test(series.boxplot.easy$ls.actions)
t.test(dots.boxplot.easy$ls.actions)
t.test(charts.boxplot.easy$ls.actions)

t.test(series.boxplot.medium$ls.actions)
t.test(charts.boxplot.medium$ls.actions)
t.test(dots.boxplot.medium$ls.actions)

t.test(series.boxplot.hard$ls.actions)
t.test(charts.boxplot.hard$ls.actions)
t.test(dots.boxplot.hard$ls.actions)

t.test(series.boxplot.easy$ls.speed)
t.test(series.boxplot.medium$ls.speed)
t.test(series.boxplot.hard$ls.speed)
t.test(charts.boxplot.easy$ls.speed)
t.test(charts.boxplot.medium$ls.speed)
t.test(charts.boxplot.hard$ls.speed)
t.test(dots.boxplot.easy$ls.speed)
t.test(dots.boxplot.medium$ls.speed)
t.test(dots.boxplot.hard$ls.speed)

