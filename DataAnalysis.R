# References: 
## https://personality-project.org/r/r.guide.html#oneway
## http://www.gardenersown.co.uk/education/lectures/r/anova.htm
## http://www2.lv.psu.edu/jxm57/irp/chisquar.html

study.dots   <- read.csv("data/study_dots.csv") 
study.charts <- read.csv("data/study_charts.csv") 
study.series <- read.csv("data/study_series.csv") 
qf.charts    <- read.csv("data/qf_charts.csv") 
qf.dots      <- read.csv("data/qf_dots.csv") 
qf.series    <- read.csv("data/qf_series.csv") 

# <Questions>
# Easy  : 1,4,7,10
# Medium: 2,5,8,11
# Hard  : 3,6,9,12
# Add missing "difficulty" parameter.

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

par(bty='n', mfrow=c(3,1), las=1)
#########
# Speed #
#########
boxplot(dots.boxplot.easy$ls.speed, 
        charts.boxplot.easy$ls.speed, 
        series.boxplot.easy$ls.speed, 
        dots.boxplot.medium$ls.speed, 
        charts.boxplot.medium$ls.speed, 
        series.boxplot.medium$ls.speed, 
        dots.boxplot.hard$ls.speed, 
        charts.boxplot.hard$ls.speed, 
        series.boxplot.hard$ls.speed, 
        ylim = c(0, 150), 
        main="speed", col="aliceblue",
        names = c("e.dots",
                  "e.charts",
                  "e.series",
                  "m.dots",
                  "m.charts",
                  "m.series",
                  "h.dots",
                  "h.charts",
                  "h.series"))
        points(1:9, c(mean(dots.boxplot.easy$ls.speed),
                      mean(charts.boxplot.easy$ls.speed),
                      mean(series.boxplot.easy$ls.speed),
                      mean(dots.boxplot.medium$ls.speed),
                      mean(charts.boxplot.medium$ls.speed),
                      mean(series.boxplot.medium$ls.speed),
                      mean(dots.boxplot.hard$ls.speed),
                      mean(charts.boxplot.hard$ls.speed),
                      mean(series.boxplot.hard$ls.speed)),
               pch="+", cex = 2)
###########
# Actions #
###########

        boxplot(dots.boxplot.easy$ls.actions, 
                charts.boxplot.easy$ls.actions, 
                series.boxplot.easy$ls.actions, 
                dots.boxplot.medium$ls.actions, 
                charts.boxplot.medium$ls.actions, 
                series.boxplot.medium$ls.actions, 
                dots.boxplot.hard$ls.actions, 
                charts.boxplot.hard$ls.actions, 
                series.boxplot.hard$ls.actions, 
                ylim = c(0, 20), 
                main="actions", col="aliceblue", 
                names = c("e.dots",
                          "e.charts",
                          "e.series",
                          "m.dots",
                          "m.charts",
                          "m.series",
                          "h.dots",
                          "h.charts",
                          "h.series"))
        points(1:9, c(mean(dots.boxplot.easy$ls.actions),
                      mean(charts.boxplot.easy$ls.actions),
                      mean(series.boxplot.easy$ls.actions),
                      mean(dots.boxplot.medium$ls.actions),
                      mean(charts.boxplot.medium$ls.actions),
                      mean(series.boxplot.medium$ls.actions),
                      mean(dots.boxplot.hard$ls.actions),
                      mean(charts.boxplot.hard$ls.actions),
                      mean(series.boxplot.hard$ls.actions)),
               pch="+", cex = 2)
        
############
# Accuracy #
############
        boxplot(dots.boxplot.easy$ls.accuracy, 
                charts.boxplot.easy$ls.accuracy, 
                series.boxplot.easy$ls.accuracy, 
                dots.boxplot.medium$ls.accuracy, 
                charts.boxplot.medium$ls.accuracy, 
                series.boxplot.medium$ls.accuracy, 
                dots.boxplot.hard$ls.accuracy, 
                charts.boxplot.hard$ls.accuracy, 
                series.boxplot.hard$ls.accuracy, 
                ylim = c(0, max(series.boxplot.hard$ls.accuracy)), 
                main="accuracy", col="aliceblue", 
                names = c("e.dots",
                          "e.charts",
                          "e.series",
                          "m.dots",
                          "m.charts",
                          "m.series",
                          "h.dots",
                          "h.charts",
                          "h.series"))
        points(1:9, c(mean(dots.boxplot.easy$ls.accuracy),
                      mean(charts.boxplot.easy$ls.accuracy),
                      mean(series.boxplot.easy$ls.accuracy),
                      mean(dots.boxplot.medium$ls.accuracy),
                      mean(charts.boxplot.medium$ls.accuracy),
                      mean(series.boxplot.medium$ls.accuracy),
                      mean(dots.boxplot.hard$ls.accuracy),
                      mean(charts.boxplot.hard$ls.accuracy),
                      mean(series.boxplot.hard$ls.accuracy)),
               pch="+", cex = 2)
        
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

df.speed.easy   <- data.frame(easy.viz,   easy.speed) 
df.speed.medium <- data.frame(medium.viz, medium.speed) 
df.speed.hard   <- data.frame(hard.viz,   hard.speed) 

df.actions.easy   <- data.frame(easy.viz,   easy.actions) 
df.actions.medium <- data.frame(medium.viz, medium.actions) 
df.actions.hard   <- data.frame(hard.viz,   hard.actions) 

df.accuracy.easy   <- data.frame(easy.viz,   easy.accuracy) 
df.accuracy.medium <- data.frame(medium.viz, medium.accuracy) 
df.accuracy.hard   <- data.frame(hard.viz,   hard.accuracy) 

speed.easy.anova     <- aov(easy.speed ~ easy.viz, data=df.speed.easy)
speed.easy.kruskal   <- kruskal.test(easy.speed ~ easy.viz, data = df.speed.easy)
speed.medium.anova   <- aov(medium.speed ~ medium.viz, data=df.speed.medium)
speed.medium.kruskal <- kruskal.test(medium.speed ~ medium.viz, data = df.speed.medium)
speed.hard.anova     <- aov(hard.speed ~ hard.viz, data=df.speed.hard)
speed.hard.kruskal   <- kruskal.test(hard.speed ~ hard.viz, data = df.speed.hard)

actions.easy.anova     <- aov(easy.actions ~ easy.viz, data=df.actions.easy)
actions.easy.kruskal   <- kruskal.test(easy.actions ~ easy.viz, data = df.actions.easy)
actions.medium.anova   <- aov(medium.actions ~ medium.viz, data=df.actions.medium)
actions.medium.kruskal <- kruskal.test(medium.actions ~ medium.viz, data = df.actions.medium)
actions.hard.anova     <- aov(hard.actions ~ hard.viz, data=df.actions.hard)
actions.hard.kruskal   <- kruskal.test(hard.actions ~ hard.viz, data = df.actions.hard)

accuracy.easy.anova     <- aov(easy.accuracy ~ easy.viz, data=df.accuracy.easy)
accuracy.easy.kruskal   <- kruskal.test(easy.accuracy ~ easy.viz, data = df.accuracy.easy)
accuracy.medium.anova   <- aov(medium.accuracy ~ medium.viz, data=df.accuracy.medium)
accuracy.medium.kruskal <- kruskal.test(medium.accuracy ~ medium.viz, data = df.accuracy.medium)
accuracy.hard.anova     <- aov(hard.accuracy ~ hard.viz, data=df.accuracy.hard)
accuracy.hard.kruskal   <- kruskal.test(hard.accuracy ~ hard.viz, data = df.accuracy.hard)

# T-test is also not viable, since we have 3 variables
# Mann-Whitney test... Not useful, since we have 3 variables. 
#speed.easy.whitney <- wilcox.test(speed ~ viz, data=speed.viz) 
# Bonferroni-Holm correction, useful...
# p.adjust(tukey.speed.easy$easy.viz[,4], method = "holm")
# p.adjust(tukey.speed.medium$medium.viz[,4], method = "holm")
# p.adjust(tukey.speed.hard$hard.viz[,4], method = "holm")
###
# Post Hoc Study...
###
tukey.speed.easy   <- TukeyHSD(speed.easy.anova)
tukey.speed.medium <- TukeyHSD(speed.medium.anova)
tukey.speed.hard   <- TukeyHSD(speed.hard.anova)

tukey.actions.easy   <- TukeyHSD(actions.easy.anova)
tukey.actions.medium <- TukeyHSD(actions.medium.anova)
tukey.actions.hard   <- TukeyHSD(actions.hard.anova)

tukey.accuracy.easy   <- TukeyHSD(accuracy.easy.anova)
tukey.accuracy.medium <- TukeyHSD(accuracy.medium.anova)
tukey.accuracy.hard   <- TukeyHSD(accuracy.hard.anova)

#par(bty='n', mfrow=c(1,1), las=1)
#plot(tukey)

#series.boxplot.medium$ls.speed   
#charts.boxplot.medium$ls.speed   
#dots.boxplot.medium$ls.speed   
    
#charts.boxplot.hard$ls.speed   
#series.boxplot.hard$ls.speed   
#dots.boxplot.hard$ls.speed   
        