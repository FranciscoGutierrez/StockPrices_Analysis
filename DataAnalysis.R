study     <- read.csv("data/second_study.csv") 
qf.charts <- read.csv("data/qf_charts.csv") 
qf.dots   <- read.csv("data/qf_dots.csv") 
qf.series <- read.csv("data/qf_series.csv") 

# <Questions>
# Easy  : 1,4,7,10
# Medium: 2,5,8,11
# Hard  : 3,6,9,12
# Add missing "difficulty" parameter.

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

split.study <- split(study, study$viz) 

study.charts <- split.study$chart
study.dots   <- split.study$dots
study.series <- split.study$series

# <Charts>
# Easy   - (Accuracy, Speed, Actions) [Mean, Median] 
# Medium - (Accuracy, Speed, Actions) [Mean, Median]
# Hard   - (Accuracy, Speed, Actions) [Mean, Median]

split.study.charts <- split(study.charts, study.charts$userid)

df.easy   <- data.frame()
df.medium <- data.frame()
df.hard   <- data.frame()

ls.id       <- c()
ls.viz      <- c() 
ls.speed    <- c() 
ls.actions  <- c() 
ls.accuracy <- c() 

for (user in split.study.charts) {
    user.speed   <- 0
    user.actions <- 0
    user.correct <- 0
    user.id      <- user$userid[1]
    user.viz     <- user$viz[1]
    if ((nrow(user) > 11) & (nrow(user) < 15)) {
    for (i in 1:nrow(user)) {
        # Do the thing...
        user.speed    <- user$timespent[i] + user.speed
        user.actions  <- user$actions[i]   + user.actions
        if (user$correct[i] == "true") user.correct  <- user.correct + 1
    }
    
        ls.id        <- c(ls.id,       toString(user.id))
        ls.viz       <- c(ls.viz,      toString(user.viz))
        ls.speed     <- c(ls.speed,    (user.speed/1000)/12)
        ls.actions   <- c(ls.actions,  user.actions/12)
        ls.accuracy  <- c(ls.accuracy, user.correct)
    }
}

ls.accuracy <- ls.accuracy/12

charts.output <- data.frame(ls.id,            
                            ls.viz,            
                            ls.speed,
                            ls.actions,
                            ls.accuracy)

par(bty='n')
par(las=1)

# boxplot(charts.output$ls.speed, ylim = c(0, 150))
# points(1, c(mean(charts.output$ls.speed)), pch="+", cex = 2)

# boxplot(charts.output$ls.actions, ylim = c(0, 20))
# points(1, c(mean(charts.output$ls.actions)), pch="+", cex = 2)

# boxplot(charts.output$ls.accuracy, ylim = c(0, max(charts.output$ls.accuracy)))
# points(1, c(mean(charts.output$ls.accuracy)), pch="+", cex = 2)

# <Series>
# Easy   - (Accuracy, Speed, Actions) [Mean, Median] 
# Medium - (Accuracy, Speed, Actions) [Mean, Median]
# Hard   - (Accuracy, Speed, Actions) [Mean, Median]

split.study.series <- split(study.series, study.series$userid)

df.easy   <- data.frame()
df.medium <- data.frame()
df.hard   <- data.frame()

ls.id       <- c()
ls.viz      <- c() 
ls.speed    <- c() 
ls.actions  <- c() 
ls.accuracy <- c() 

for (user in split.study.series) {
    user.speed   <- 0
    user.actions <- 0
    user.correct <- 0
    user.id      <- user$userid[1]
    user.viz     <- user$viz[1]
    if ((nrow(user) > 11) & (nrow(user) < 15)) {
        for (i in 1:nrow(user)) {
            # Do the thing...
            user.speed    <- user$timespent[i] + user.speed
            user.actions  <- user$actions[i]   + user.actions
            if (user$correct[i] == "true") user.correct  <- user.correct + 1
        }
        
        ls.id        <- c(ls.id,       toString(user.id))
        ls.viz       <- c(ls.viz,      toString(user.viz))
        ls.speed     <- c(ls.speed,    (user.speed/1000)/12)
        ls.actions   <- c(ls.actions,  user.actions/12)
        ls.accuracy  <- c(ls.accuracy, user.correct)
    }
}

ls.accuracy <- ls.accuracy/12

series.output <- data.frame(ls.id,            
                            ls.viz,            
                            ls.speed,
                            ls.actions,
                            ls.accuracy)

par(bty='n')
par(las=1)
# 
# boxplot(series.output$ls.speed, ylim = c(0, 150))
# points(1, c(mean(series.output$ls.speed)), pch="+", cex = 2)
# 
# boxplot(series.output$ls.actions, ylim = c(0, 20))
# points(1, c(mean(series.output$ls.actions)), pch="+", cex = 2)
# 
# boxplot(series.output$ls.accuracy, ylim = c(0, max(series.output$ls.accuracy)))
# points(1, c(mean(series.output$ls.accuracy)), pch="+", cex = 2)

# <Dots>
# Easy   - (Accuracy, Speed, Actions) [Mean, Median] 
# Medium - (Accuracy, Speed, Actions) [Mean, Median]
# Hard   - (Accuracy, Speed, Actions) [Mean, Median]

split.study.dots <- split(study.dots, study.dots$userid)

df.easy   <- data.frame()
df.medium <- data.frame()
df.hard   <- data.frame()

ls.id       <- c()
ls.viz      <- c() 
ls.speed    <- c() 
ls.actions  <- c() 
ls.accuracy <- c() 

for (user in split.study.dots) {
    user.speed   <- 0
    user.actions <- 0
    user.correct <- 0
    user.id      <- user$userid[1]
    user.viz     <- user$viz[1]
    if ((nrow(user) > 11) & (nrow(user) < 15)) {
        for (i in 1:nrow(user)) {
            # Do the thing...
            user.speed    <- user$timespent[i] + user.speed
            user.actions  <- user$actions[i]   + user.actions
            if (user$correct[i] == "true") user.correct  <- user.correct + 1
        }
        if(((user.speed/1000)/12) < 200) {
            ls.id        <- c(ls.id,       toString(user.id))
            ls.viz       <- c(ls.viz,      toString(user.viz))
            ls.speed     <- c(ls.speed,    (user.speed/1000)/12)
            ls.actions   <- c(ls.actions,  user.actions/12)
            ls.accuracy  <- c(ls.accuracy, user.correct)
        }
    }
}

ls.accuracy <- ls.accuracy/12

dots.output <- data.frame(ls.id,            
                            ls.viz,            
                            ls.speed,
                            ls.actions,
                            ls.accuracy)

par(bty='n')
par(las=1)

boxplot(dots.output$ls.speed, charts.output$ls.speed, series.output$ls.speed, ylim = c(0, 150))
points(1:3, c(mean(dots.output$ls.speed),   mean(charts.output$ls.speed), mean(series.output$ls.speed)), pch="+", cex = 2)
 
boxplot(dots.output$ls.actions, charts.output$ls.actions, series.output$ls.actions, ylim = c(0, 20))
points(1:3, c(mean(dots.output$ls.actions), mean(charts.output$ls.actions), mean(series.output$ls.actions)), pch="+", cex = 2)

boxplot(dots.output$ls.accuracy, charts.output$ls.accuracy, series.output$ls.accuracy, ylim = c(0, max(series.output$ls.accuracy)))
points(1:3, c(mean(dots.output$ls.accuracy), mean(charts.output$ls.accuracy), mean(series.output$ls.accuracy)), pch="+", cex = 2)


