# References: 
## https://personality-project.org/r/r.guide.html#oneway
## http://www.gardenersown.co.uk/education/lectures/r/anova.htm
## http://www2.lv.psu.edu/jxm57/irp/chisquar.html


#### First Study
s1.study.dots   <- read.csv("data/s1_dots.csv")
s1.study.charts <- read.csv("data/s1_charts.csv")
s1.study.series <- read.csv("data/s1_maps.csv")
s1.qf.charts    <- read.csv("data/qf1_charts.csv")
s1.qf.dots      <- read.csv("data/qf1_dots.csv")
s1.qf.series    <- read.csv("data/qf1_maps.csv")

#### Second Study
s2.study.dots   <- read.csv("data/s2_dots.csv")
s2.study.charts <- read.csv("data/s2_charts.csv")
s2.study.series <- read.csv("data/s2_series.csv")
s2.qf.charts    <- read.csv("data/qf2_charts.csv")
s2.qf.dots      <- read.csv("data/qf2_dots.csv")
s2.qf.series    <- read.csv("data/qf2_series.csv")

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

s1.study.dots   <- filter.users(s1.study.dots,  s1.qf.charts)
s1.study.charts <- filter.users(s1.study.charts,s1.qf.dots  )
s1.study.series <- filter.users(s1.study.series,s1.qf.series)

s2.study.dots   <- filter.users(s2.study.dots,  s2.qf.charts)
s2.study.charts <- filter.users(s2.study.charts,s2.qf.dots  )
s2.study.series <- filter.users(s2.study.series,s2.qf.series)

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
s1.clean.study.charts <- add.difficulty(s1.study.charts)
s1.clean.study.dots   <- add.difficulty(s1.study.dots)
s1.clean.study.series <- add.difficulty(s1.study.series)
s1.charts.boxplot.easy   <- create.boxplot(s1.clean.study.charts, "easy")
s1.series.boxplot.easy   <- create.boxplot(s1.clean.study.series, "easy")
s1.dots.boxplot.easy     <- create.boxplot(s1.clean.study.dots,   "easy")
s1.series.boxplot.medium <- create.boxplot(s1.clean.study.series, "medium")
s1.charts.boxplot.medium <- create.boxplot(s1.clean.study.charts, "medium")
s1.dots.boxplot.medium   <- create.boxplot(s1.clean.study.dots,   "medium")
s1.charts.boxplot.hard   <- create.boxplot(s1.clean.study.charts, "hard")
s1.series.boxplot.hard   <- create.boxplot(s1.clean.study.series, "hard")
s1.dots.boxplot.hard     <- create.boxplot(s1.clean.study.dots,   "hard")

s2.clean.study.charts <- add.difficulty(s2.study.charts)
s2.clean.study.dots   <- add.difficulty(s2.study.dots)
s2.clean.study.series <- add.difficulty(s2.study.series)
s2.charts.boxplot.easy   <- create.boxplot(s2.clean.study.charts, "easy")
s2.series.boxplot.easy   <- create.boxplot(s2.clean.study.series, "easy")
s2.dots.boxplot.easy     <- create.boxplot(s2.clean.study.dots,   "easy")
s2.series.boxplot.medium <- create.boxplot(s2.clean.study.series, "medium")
s2.charts.boxplot.medium <- create.boxplot(s2.clean.study.charts, "medium")
s2.dots.boxplot.medium   <- create.boxplot(s2.clean.study.dots,   "medium")
s2.charts.boxplot.hard   <- create.boxplot(s2.clean.study.charts, "hard")
s2.series.boxplot.hard   <- create.boxplot(s2.clean.study.series, "hard")
s2.dots.boxplot.hard     <- create.boxplot(s2.clean.study.dots,   "hard")

#############################################################
s1.easy.speed <- c()
s1.easy.viz   <- c()
s1.easy.speed <- c(s1.easy.speed, s1.charts.boxplot.easy$ls.speed)
s1.easy.viz   <- c(s1.easy.viz,   as.vector(s1.charts.boxplot.easy$ls.viz))
s1.easy.speed <- c(s1.easy.speed, s1.series.boxplot.easy$ls.speed)
s1.easy.viz   <- c(s1.easy.viz,   as.vector(s1.series.boxplot.easy$ls.viz))
s1.easy.speed <- c(s1.easy.speed, s1.dots.boxplot.easy$ls.speed)
s1.easy.viz   <- c(s1.easy.viz,   as.vector(s1.dots.boxplot.easy$ls.viz))
#############################################################
s1.medium.speed <- c()
s1.medium.viz   <- c()
s1.medium.speed <- c(s1.medium.speed, s1.charts.boxplot.medium$ls.speed)
s1.medium.viz   <- c(s1.medium.viz,   as.vector(s1.charts.boxplot.medium$ls.viz))
s1.medium.speed <- c(s1.medium.speed, s1.series.boxplot.medium$ls.speed)
s1.medium.viz   <- c(s1.medium.viz,   as.vector(s1.series.boxplot.medium$ls.viz))
s1.medium.speed <- c(s1.medium.speed, s1.dots.boxplot.medium$ls.speed)
s1.medium.viz   <- c(s1.medium.viz,   as.vector(s1.dots.boxplot.medium$ls.viz))
#############################################################
s1.hard.speed <- c()
s1.hard.viz   <- c()
s1.hard.speed <- c(s1.hard.speed, s1.charts.boxplot.hard$ls.speed)
s1.hard.viz   <- c(s1.hard.viz,   as.vector(s1.charts.boxplot.hard$ls.viz))
s1.hard.speed <- c(s1.hard.speed, s1.series.boxplot.hard$ls.speed)
s1.hard.viz   <- c(s1.hard.viz,   as.vector(s1.series.boxplot.hard$ls.viz))
s1.hard.speed <- c(s1.hard.speed, s1.dots.boxplot.hard$ls.speed)
s1.hard.viz   <- c(s1.hard.viz,   as.vector(s1.dots.boxplot.hard$ls.viz))
#############################################################
s1.easy.actions <- c()
s1.easy.viz     <- c()
s1.easy.actions <- c(s1.easy.actions, s1.charts.boxplot.easy$ls.actions)
s1.easy.viz     <- c(s1.easy.viz,     as.vector(s1.charts.boxplot.easy$ls.viz))
s1.easy.actions <- c(s1.easy.actions, s1.series.boxplot.easy$ls.actions)
s1.easy.viz     <- c(s1.easy.viz,     as.vector(s1.series.boxplot.easy$ls.viz))
s1.easy.actions <- c(s1.easy.actions, s1.dots.boxplot.easy$ls.actions)
s1.easy.viz     <- c(s1.easy.viz,     as.vector(s1.dots.boxplot.easy$ls.viz))
#############################################################
s1.medium.actions <- c()
s1.medium.viz     <- c()
s1.medium.actions <- c(s1.medium.actions, s1.charts.boxplot.medium$ls.actions)
s1.medium.viz     <- c(s1.medium.viz,     as.vector(s1.charts.boxplot.medium$ls.viz))
s1.medium.actions <- c(s1.medium.actions, s1.series.boxplot.medium$ls.actions)
s1.medium.viz     <- c(s1.medium.viz,     as.vector(s1.series.boxplot.medium$ls.viz))
s1.medium.actions <- c(s1.medium.actions, s1.dots.boxplot.medium$ls.actions)
s1.medium.viz     <- c(s1.medium.viz,     as.vector(s1.dots.boxplot.medium$ls.viz))
#############################################################
s1.hard.actions <- c()
s1.hard.viz     <- c()
s1.hard.actions <- c(s1.hard.actions, s1.charts.boxplot.hard$ls.actions)
s1.hard.viz     <- c(s1.hard.viz,     as.vector(s1.charts.boxplot.hard$ls.viz))
s1.hard.actions <- c(s1.hard.actions, s1.series.boxplot.hard$ls.actions)
s1.hard.viz     <- c(s1.hard.viz,     as.vector(s1.series.boxplot.hard$ls.viz))
s1.hard.actions <- c(s1.hard.actions, s1.dots.boxplot.hard$ls.actions)
s1.hard.viz     <- c(s1.hard.viz,     as.vector(s1.dots.boxplot.hard$ls.viz))
#############################################################
s1.easy.accuracy <- c()
s1.easy.viz      <- c()
s1.easy.accuracy <- c(s1.easy.accuracy, s1.charts.boxplot.easy$ls.accuracy)
s1.easy.viz      <- c(s1.easy.viz,      as.vector(s1.charts.boxplot.easy$ls.viz))
s1.easy.accuracy <- c(s1.easy.accuracy, s1.series.boxplot.easy$ls.accuracy)
s1.easy.viz      <- c(s1.easy.viz,      as.vector(s1.series.boxplot.easy$ls.viz))
s1.easy.accuracy <- c(s1.easy.accuracy, s1.dots.boxplot.easy$ls.accuracy)
s1.easy.viz      <- c(s1.easy.viz,      as.vector(s1.dots.boxplot.easy$ls.viz))
#############################################################
s1.medium.accuracy <- c()
s1.medium.viz      <- c()
s1.medium.accuracy <- c(s1.medium.accuracy, s1.charts.boxplot.medium$ls.accuracy)
s1.medium.viz      <- c(s1.medium.viz,      as.vector(s1.charts.boxplot.medium$ls.viz))
s1.medium.accuracy <- c(s1.medium.accuracy, s1.series.boxplot.medium$ls.accuracy)
s1.medium.viz      <- c(s1.medium.viz,      as.vector(s1.series.boxplot.medium$ls.viz))
s1.medium.accuracy <- c(s1.medium.accuracy, s1.dots.boxplot.medium$ls.accuracy)
s1.medium.viz      <- c(s1.medium.viz,      as.vector(s1.dots.boxplot.medium$ls.viz))
#############################################################
s1.hard.accuracy <- c()
s1.hard.viz      <- c()
s1.hard.accuracy <- c(s1.hard.accuracy, s1.charts.boxplot.hard$ls.accuracy)
s1.hard.viz      <- c(s1.hard.viz,      as.vector(s1.charts.boxplot.hard$ls.viz))
s1.hard.accuracy <- c(s1.hard.accuracy, s1.series.boxplot.hard$ls.accuracy)
s1.hard.viz      <- c(s1.hard.viz,      as.vector(s1.series.boxplot.hard$ls.viz))
s1.hard.accuracy <- c(s1.hard.accuracy, s1.dots.boxplot.hard$ls.accuracy)
s1.hard.viz      <- c(s1.hard.viz,      as.vector(s1.dots.boxplot.hard$ls.viz))
#############################################################

#############################################################
s2.easy.speed <- c()
s2.easy.viz   <- c()
s2.easy.speed <- c(s2.easy.speed, s2.charts.boxplot.easy$ls.speed)
s2.easy.viz   <- c(s2.easy.viz,   as.vector(s2.charts.boxplot.easy$ls.viz))
s2.easy.speed <- c(s2.easy.speed, s2.series.boxplot.easy$ls.speed)
s2.easy.viz   <- c(s2.easy.viz,   as.vector(s2.series.boxplot.easy$ls.viz))
s2.easy.speed <- c(s2.easy.speed, s2.dots.boxplot.easy$ls.speed)
s2.easy.viz   <- c(s2.easy.viz,   as.vector(s2.dots.boxplot.easy$ls.viz))
#############################################################
s2.medium.speed <- c()
s2.medium.viz   <- c()
s2.medium.speed <- c(s2.medium.speed, s2.charts.boxplot.medium$ls.speed)
s2.medium.viz   <- c(s2.medium.viz,   as.vector(s2.charts.boxplot.medium$ls.viz))
s2.medium.speed <- c(s2.medium.speed, s2.series.boxplot.medium$ls.speed)
s2.medium.viz   <- c(s2.medium.viz,   as.vector(s2.series.boxplot.medium$ls.viz))
s2.medium.speed <- c(s2.medium.speed, s2.dots.boxplot.medium$ls.speed)
s2.medium.viz   <- c(s2.medium.viz,   as.vector(s2.dots.boxplot.medium$ls.viz))
#############################################################
s2.hard.speed <- c()
s2.hard.viz   <- c()
s2.hard.speed <- c(s2.hard.speed, s2.charts.boxplot.hard$ls.speed)
s2.hard.viz   <- c(s2.hard.viz,   as.vector(s2.charts.boxplot.hard$ls.viz))
s2.hard.speed <- c(s2.hard.speed, s2.series.boxplot.hard$ls.speed)
s2.hard.viz   <- c(s2.hard.viz,   as.vector(s2.series.boxplot.hard$ls.viz))
s2.hard.speed <- c(s2.hard.speed, s2.dots.boxplot.hard$ls.speed)
s2.hard.viz   <- c(s2.hard.viz,   as.vector(s2.dots.boxplot.hard$ls.viz))
#############################################################
s2.easy.actions <- c()
s2.easy.viz     <- c()
s2.easy.actions <- c(s2.easy.actions, s2.charts.boxplot.easy$ls.actions)
s2.easy.viz     <- c(s2.easy.viz,     as.vector(s2.charts.boxplot.easy$ls.viz))
s2.easy.actions <- c(s2.easy.actions, s2.series.boxplot.easy$ls.actions)
s2.easy.viz     <- c(s2.easy.viz,     as.vector(s2.series.boxplot.easy$ls.viz))
s2.easy.actions <- c(s2.easy.actions, s2.dots.boxplot.easy$ls.actions)
s2.easy.viz     <- c(s2.easy.viz,     as.vector(s2.dots.boxplot.easy$ls.viz))
#############################################################
s2.medium.actions <- c()
s2.medium.viz     <- c()
s2.medium.actions <- c(s2.medium.actions, s2.charts.boxplot.medium$ls.actions)
s2.medium.viz     <- c(s2.medium.viz,     as.vector(s2.charts.boxplot.medium$ls.viz))
s2.medium.actions <- c(s2.medium.actions, s2.series.boxplot.medium$ls.actions)
s2.medium.viz     <- c(s2.medium.viz,     as.vector(s2.series.boxplot.medium$ls.viz))
s2.medium.actions <- c(s2.medium.actions, s2.dots.boxplot.medium$ls.actions)
s2.medium.viz     <- c(s2.medium.viz,     as.vector(s2.dots.boxplot.medium$ls.viz))
#############################################################
s2.hard.actions <- c()
s2.hard.viz     <- c()
s2.hard.actions <- c(s2.hard.actions, s2.charts.boxplot.hard$ls.actions)
s2.hard.viz     <- c(s2.hard.viz,     as.vector(s2.charts.boxplot.hard$ls.viz))
s2.hard.actions <- c(s2.hard.actions, s2.series.boxplot.hard$ls.actions)
s2.hard.viz     <- c(s2.hard.viz,     as.vector(s2.series.boxplot.hard$ls.viz))
s2.hard.actions <- c(s2.hard.actions, s2.dots.boxplot.hard$ls.actions)
s2.hard.viz     <- c(s2.hard.viz,     as.vector(s2.dots.boxplot.hard$ls.viz))
#############################################################
s2.easy.accuracy <- c()
s2.easy.viz      <- c()
s2.easy.accuracy <- c(s2.easy.accuracy, s2.charts.boxplot.easy$ls.accuracy)
s2.easy.viz      <- c(s2.easy.viz,      as.vector(s2.charts.boxplot.easy$ls.viz))
s2.easy.accuracy <- c(s2.easy.accuracy, s2.series.boxplot.easy$ls.accuracy)
s2.easy.viz      <- c(s2.easy.viz,      as.vector(s2.series.boxplot.easy$ls.viz))
s2.easy.accuracy <- c(s2.easy.accuracy, s2.dots.boxplot.easy$ls.accuracy)
s2.easy.viz      <- c(s2.easy.viz,      as.vector(s2.dots.boxplot.easy$ls.viz))
#############################################################
s2.medium.accuracy <- c()
s2.medium.viz      <- c()
s2.medium.accuracy <- c(s2.medium.accuracy, s2.charts.boxplot.medium$ls.accuracy)
s2.medium.viz      <- c(s2.medium.viz,      as.vector(s2.charts.boxplot.medium$ls.viz))
s2.medium.accuracy <- c(s2.medium.accuracy, s2.series.boxplot.medium$ls.accuracy)
s2.medium.viz      <- c(s2.medium.viz,      as.vector(s2.series.boxplot.medium$ls.viz))
s2.medium.accuracy <- c(s2.medium.accuracy, s2.dots.boxplot.medium$ls.accuracy)
s2.medium.viz      <- c(s2.medium.viz,      as.vector(s2.dots.boxplot.medium$ls.viz))
#############################################################
s2.hard.accuracy <- c()
s2.hard.viz      <- c()
s2.hard.accuracy <- c(s2.hard.accuracy, s2.charts.boxplot.hard$ls.accuracy)
s2.hard.viz      <- c(s2.hard.viz,      as.vector(s2.charts.boxplot.hard$ls.viz))
s2.hard.accuracy <- c(s2.hard.accuracy, s2.series.boxplot.hard$ls.accuracy)
s2.hard.viz      <- c(s2.hard.viz,      as.vector(s2.series.boxplot.hard$ls.viz))
s2.hard.accuracy <- c(s2.hard.accuracy, s2.dots.boxplot.hard$ls.accuracy)
s2.hard.viz      <- c(s2.hard.viz,      as.vector(s2.dots.boxplot.hard$ls.viz))
#############################################################

s1.df.speed.easy      <- data.frame(s1.easy.viz,   s1.easy.speed) 
s1.df.speed.medium    <- data.frame(s1.medium.viz, s1.medium.speed) 
s1.df.speed.hard      <- data.frame(s1.hard.viz,   s1.hard.speed) 
s1.df.actions.easy    <- data.frame(s1.easy.viz,   s1.easy.actions) 
s1.df.actions.medium  <- data.frame(s1.medium.viz, s1.medium.actions) 
s1.df.actions.hard    <- data.frame(s1.hard.viz,   s1.hard.actions) 
s1.df.accuracy.easy   <- data.frame(s1.easy.viz,   s1.easy.accuracy) 
s1.df.accuracy.medium <- data.frame(s1.medium.viz, s1.medium.accuracy) 
s1.df.accuracy.hard   <- data.frame(s1.hard.viz,   s1.hard.accuracy) 

s2.df.speed.easy      <- data.frame(s2.easy.viz,   s2.easy.speed) 
s2.df.speed.medium    <- data.frame(s2.medium.viz, s2.medium.speed) 
s2.df.speed.hard      <- data.frame(s2.hard.viz,   s2.hard.speed) 
s2.df.actions.easy    <- data.frame(s2.easy.viz,   s2.easy.actions) 
s2.df.actions.medium  <- data.frame(s2.medium.viz, s2.medium.actions) 
s2.df.actions.hard    <- data.frame(s2.hard.viz,   s2.hard.actions) 
s2.df.accuracy.easy   <- data.frame(s2.easy.viz,   s2.easy.accuracy) 
s2.df.accuracy.medium <- data.frame(s2.medium.viz, s2.medium.accuracy) 
s2.df.accuracy.hard   <- data.frame(s2.hard.viz,   s2.hard.accuracy) 

s1.df.accuracy.easy["difficulty"]   <- rep(factor("easy"),   212)
s1.df.accuracy.medium["difficulty"] <- rep(factor("medium"), 211)
s1.df.accuracy.hard["difficulty"]   <- rep(factor("hard"),   212)
s1.df.actions.easy["difficulty"]    <- rep(factor("easy"),   212)
s1.df.actions.medium["difficulty"]  <- rep(factor("medium"), 211)
s1.df.actions.hard["difficulty"]    <- rep(factor("hard"),   212)
s1.df.speed.easy["difficulty"]      <- rep(factor("easy"),   212)
s1.df.speed.medium["difficulty"]    <- rep(factor("medium"), 211)
s1.df.speed.hard["difficulty"]      <- rep(factor("hard"),   212)

s1.df.accuracy.easy["study"]   <- rep(factor("s1"),212)
s1.df.accuracy.medium["study"] <- rep(factor("s1"),211)
s1.df.accuracy.hard["study"]   <- rep(factor("s1"),212)
s1.df.actions.easy["study"]    <- rep(factor("s1"),212)
s1.df.actions.medium["study"]  <- rep(factor("s1"),211)
s1.df.actions.hard["study"]    <- rep(factor("s1"),212)
s1.df.speed.easy["study"]      <- rep(factor("s1"),212)
s1.df.speed.medium["study"]    <- rep(factor("s1"),211)
s1.df.speed.hard["study"]      <- rep(factor("s1"),212)


s2.df.accuracy.easy["difficulty"]   <- rep(factor("easy"),   266)
s2.df.accuracy.medium["difficulty"] <- rep(factor("medium"), 267)
s2.df.accuracy.hard["difficulty"]   <- rep(factor("hard"),   266)
s2.df.actions.easy["difficulty"]    <- rep(factor("easy"),   266)
s2.df.actions.medium["difficulty"]  <- rep(factor("medium"), 267)
s2.df.actions.hard["difficulty"]    <- rep(factor("hard"),   266)
s2.df.speed.easy["difficulty"]      <- rep(factor("easy"),   266)
s2.df.speed.medium["difficulty"]    <- rep(factor("medium"), 267)
s2.df.speed.hard["difficulty"]      <- rep(factor("hard"),   266)

s2.df.accuracy.easy["study"]   <- rep(factor("s2"),266)
s2.df.accuracy.medium["study"] <- rep(factor("s2"),267)
s2.df.accuracy.hard["study"]   <- rep(factor("s2"),266)
s2.df.actions.easy["study"]    <- rep(factor("s2"),266)
s2.df.actions.medium["study"]  <- rep(factor("s2"),267)
s2.df.actions.hard["study"]    <- rep(factor("s2"),266)
s2.df.speed.easy["study"]      <- rep(factor("s2"),266)
s2.df.speed.medium["study"]    <- rep(factor("s2"),267)
s2.df.speed.hard["study"]      <- rep(factor("s2"),266)

colnames(s1.df.accuracy.easy)       <- c("viz", "accuracy", "difficulty", "study")
colnames(s1.df.accuracy.medium)     <- c("viz", "accuracy", "difficulty", "study")
colnames(s1.df.accuracy.hard)       <- c("viz", "accuracy", "difficulty", "study")
colnames(s1.df.actions.easy)        <- c("viz", "actions",  "difficulty", "study")
colnames(s1.df.actions.medium)      <- c("viz", "actions",  "difficulty", "study")
colnames(s1.df.actions.hard)        <- c("viz", "actions",  "difficulty", "study")
colnames(s1.df.speed.easy)          <- c("viz", "speed",    "difficulty", "study")
colnames(s1.df.speed.medium)        <- c("viz", "speed",    "difficulty", "study")
colnames(s1.df.speed.hard)          <- c("viz", "speed",    "difficulty", "study")

colnames(s2.df.accuracy.easy)       <- c("viz", "accuracy", "difficulty",  "study")
colnames(s2.df.accuracy.medium)     <- c("viz", "accuracy", "difficulty",  "study")
colnames(s2.df.accuracy.hard)       <- c("viz", "accuracy", "difficulty",  "study")
colnames(s2.df.actions.easy)        <- c("viz", "actions",  "difficulty",  "study")
colnames(s2.df.actions.medium)      <- c("viz", "actions",  "difficulty",  "study")
colnames(s2.df.actions.hard)        <- c("viz", "actions",  "difficulty",  "study")
colnames(s2.df.speed.easy)          <- c("viz", "speed",    "difficulty",  "study")
colnames(s2.df.speed.medium)        <- c("viz", "speed",    "difficulty",  "study")
colnames(s2.df.speed.hard)          <- c("viz", "speed",    "difficulty",  "study")

s1.df.accuracy.all <- rbind(s1.df.accuracy.easy, s1.df.accuracy.medium, s1.df.accuracy.hard)
s1.df.actions.all  <- rbind(s1.df.actions.easy,  s1.df.actions.medium,  s1.df.actions.hard)
s1.df.speed.all    <- rbind(s1.df.speed.easy,    s1.df.speed.medium,    s1.df.speed.hard)

s2.df.accuracy.all <- rbind(s2.df.accuracy.easy, s2.df.accuracy.medium, s2.df.accuracy.hard)
s2.df.actions.all  <- rbind(s2.df.actions.easy,  s2.df.actions.medium,  s2.df.actions.hard)
s2.df.speed.all    <- rbind(s2.df.speed.easy,    s2.df.speed.medium,    s2.df.speed.hard)

df.accuracy.all <- rbind(s1.df.accuracy.all, s2.df.accuracy.all)
df.actions.all  <- rbind(s1.df.actions.all,  s2.df.actions.all)
df.speed.all    <- rbind(s1.df.speed.all,    s2.df.speed.all)

# three.accuracy.out <- aov(accuracy~viz*difficulty*study, data = df.accuracy.all)
# TukeyHSD(three.accuracy.out, which="difficulty")
# TukeyHSD(three.accuracy.out, which="viz")
# TukeyHSD(three.accuracy.out, which="study")
# TukeyHSD(three.accuracy.out)

# s1.accuracy.out <- aov(accuracy ~ viz * difficulty, data=s1.df.accuracy.all)
# s1.actions.out  <- aov(actions  ~ viz * difficulty, data=s1.df.actions.all)
# s1.speed.out    <- aov(speed    ~ viz * difficulty, data=s1.df.speed.all)
# 
# s2.accuracy.out <- aov(accuracy ~ viz * difficulty, data=s2.df.accuracy.all)
# s2.actions.out  <- aov(actions  ~ viz * difficulty, data=s2.df.actions.all)
# s2.speed.out    <- aov(speed    ~ viz * difficulty, data=s2.df.speed.all)

# summary(actions.out)
# TukeyHSD(actions.out)
# summary(accuracy.out)
# TukeyHSD(accuracy.out)
# summary(speed.out)
# TukeyHSD(speed.out)


