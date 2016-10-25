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
        
###
speed <- c()
viz   <- c()
###
speed <- c(speed, charts.boxplot.easy$ls.speed)
viz   <- c(viz, as.vector(charts.boxplot.easy$ls.viz))
###
speed <- c(speed, series.boxplot.easy$ls.speed)
viz   <- c(viz, as.vector(series.boxplot.easy$ls.viz))
###
speed <- c(speed, dots.boxplot.easy$ls.speed)
viz   <- c(viz, as.vector(dots.boxplot.easy$ls.viz))

speed.viz <- data.frame(viz, speed) 

speed.model <- lm(speed ~ viz, data=speed.viz)
        
#series.boxplot.medium$ls.speed   
#charts.boxplot.medium$ls.speed   
#dots.boxplot.medium$ls.speed   
    
#charts.boxplot.hard$ls.speed   
#series.boxplot.hard$ls.speed   
#dots.boxplot.hard$ls.speed   
        