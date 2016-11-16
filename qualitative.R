require(plyr)
require(devtools)
require(likert)

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



filter.users <- function(main.study, qf.study) {
split.study  <- split(main.study, main.study$userid)
dots.users   <- c() 

for (user in split.study) {
    dots.users <- c(dots.users, as.character(user$userid[1]))
}

#code   <- c()    
#age    <- c()
#gender <- c()   
vq1    <- c()
vq2    <- c()
vq3    <- c()
vq4    <- c()
vq5    <- c()
vq6    <- c()
vq7    <- c()
vq8    <- c()
vq9    <- c()
vq10   <- c() 
vq11   <- c() 
vq12   <- c() 
vq13   <- c() 
vq14   <- c() 
vq15   <- c() 
 
for (user in dots.users) {
    for (i in 1:nrow(qf.study)) {
        if(as.character(qf.study[i,2]) == user) {
            #code   <- c(code  , as.character(qf.study[i,2]))    
            #age    <- c(age   , qf.study[i,3])
            #gender <- c(gender, as.character(qf.study[i,4]))
            vq1    <- c(vq1   , qf.study[i,11])
            vq2    <- c(vq2   , qf.study[i,12])
            vq3    <- c(vq3   , qf.study[i,13])
            vq4    <- c(vq4   , qf.study[i,14])
            vq5    <- c(vq5   , qf.study[i,15])
            vq6    <- c(vq6   , qf.study[i,16])
            vq7    <- c(vq7   , qf.study[i,17])
            vq8    <- c(vq8   , qf.study[i,18])
            vq9    <- c(vq9   , qf.study[i,19])
            vq10   <- c(vq10  , qf.study[i,20]) 
            vq11   <- c(vq11  , qf.study[i,21]) 
            vq12   <- c(vq12  , qf.study[i,22]) 
            vq13   <- c(vq13  , qf.study[i,23]) 
            vq14   <- c(vq14  , qf.study[i,24]) 
            vq15   <- c(vq15  , qf.study[i,25]) 
            
            # vq1    <- c(vq1   , qf.study[i,5])
            # vq2    <- c(vq2   , qf.study[i,6])
            # vq3    <- c(vq3   , qf.study[i,7])
            # vq4    <- c(vq4   , qf.study[i,8])
            # vq5    <- c(vq5   , qf.study[i,9])
            # vq6    <- c(vq6   , qf.study[i,10])
            # vq7    <- c(vq7   , qf.study[i,11])
            # vq8    <- c(vq8   , qf.study[i,12])
            # vq9    <- c(vq9   , qf.study[i,13])
            # vq10   <- c(vq10  , qf.study[i,14]) 
            # vq11   <- c(vq11  , qf.study[i,15]) 
            # vq12   <- c(vq12  , qf.study[i,16]) 
            # vq13   <- c(vq13  , qf.study[i,17]) 
            # vq14   <- c(vq14  , qf.study[i,18]) 
            # vq15   <- c(vq15  , qf.study[i,19]) 
        }
    }
}

output <- data.frame(#code,
                     #age ,
                     #gender,
                     vq1 ,
                     vq2 ,
                     vq3 ,
                     #vq4 ,
                     #vq5 ,
                     vq6 ,
                     #vq7 ,
                     vq8 ,
                     #vq9 ,
                     #vq10,
                     #vq11,
                     #vq12,
                     #vq13,
                     #vq14,
                     vq15)
return(output)

}

filtered.compact    <- filter.users(study.dots  , qf.dots  )
filtered.detailed   <- filter.users(study.charts, qf.charts)
filtered.intuitive  <- filter.users(study.series, qf.series)


# vq1 ease of use    # vq2 understand   # vq3 vis. appeal    # vq4 engaging
# vq5 like           # vq6 trustworthy  # vq7 credible       # vq8 confidence
# vq9 match          # vq10 suitable    # vq11 easy to see   # vq12 clear
# vq13 ease of tasks # vq14 unc. factor # v15 expect

par(bty='n', mfrow=c(1,1), las=1)


likert.scale <- function(data) {
mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
data[data == 1 ] <- "Strongly Disagree"
data[data == 2 ] <- "Disagree"
data[data == 3 ] <- "Neither"
data[data == 4 ] <- "Agree"
data[data == 5 ] <- "Strongly Agree"
for(i in seq_along(data)) {
    data[,i] <- factor(data[,i], levels=mylevels)
}
data <- rename(data, c(vq1  = "Ease of Use",
                       vq2  = "Understanding", 
                       vq3  = "Visual Appeal",
                       vq4  = "Engament", 
                       vq5  = "Like it", 
                       vq6  = "Trustworthiness", 
                       vq7  = "Credible", 
                       vq8  = "Confidence", 
                       vq9  = "Operation", 
                       vq10 = "Suitability", 
                       vq11 = "Importance",
                       vq12 = "Comprehensible",
                       vq13 = "Easy Tasks",
                       vq14 = "Opacity",
                       vq15 = "Expectation"
                       ))
return(data)
}

likert.compact   <- likert.scale(filtered.compact)
likert.intuitive <- likert.scale(filtered.intuitive)
likert.detailed  <- likert.scale(filtered.detailed)


plot(likert(likert.detailed))
last_plot()
ggsave("s2.likert.detailed.png", width=6, height=3, dpi=300)

plot(likert(likert.compact))
last_plot()
ggsave("s2.likert.compact.png", width=6, height=3, dpi=300)

plot(likert(likert.intuitive))
last_plot()
ggsave("s2.likert.intuitive.png", width=6, height=3, dpi=300)

# Charts
# boxplot(qf.charts$vq8, qf.charts$vq1, qf.charts$vq2, qf.charts$vq3, qf.charts$vq6, qf.charts$vq14, qf.charts$vq15, ylim = c(0, max(qf.charts$vq8)))
# points(1:7, c(mean(qf.charts$vq8), mean(qf.charts$vq1), mean(qf.charts$vq2), mean(qf.charts$vq3), mean(qf.charts$vq6), mean(qf.charts$vq14), mean(qf.charts$vq15)), pch="+", cex = 2)

# Dots
# boxplot(qf.dots$vq8, qf.dots$vq1, qf.dots$vq2, qf.dots$vq3, qf.dots$vq6, qf.dots$vq14, qf.dots$vq15, ylim = c(0, max(qf.dots$vq8)))
# points(1:7, c(mean(qf.dots$vq8), mean(qf.dots$vq1), mean(qf.dots$vq2), mean(qf.dots$vq3), mean(qf.dots$vq6), mean(qf.dots$vq14), mean(qf.dots$vq15)), pch="+", cex = 2)

# Series
# boxplot(qf.series$vq8, qf.series$vq1, qf.series$vq2, qf.series$vq3, qf.series$vq6, qf.series$vq14, qf.series$vq15, ylim = c(0, max(qf.series$vq8)))
# points(1:7, c(mean(qf.series$vq8), mean(qf.series$vq1), mean(qf.series$vq2), mean(qf.series$vq3), mean(qf.series$vq6), mean(qf.series$vq14), mean(qf.series$vq15)), pch="+", cex = 2)