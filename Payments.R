data <- read.csv("output2.csv")

split.data <- split(data, data$userid)

ls.names     <- c()
ls.questions <- c()
ls.correct   <- c()
ls.viz       <- c()
ls.payment   <- c()
ls.timestamp <- c()
#  Check users and questions
for (user in split.data) {
    user.subset  <- subset(user,
                           select = c("userid"  ,
                                      "question",
                                      "correct" ,
                                      "viz"))
    user.ordered       <- user.subset[order(user.subset$question),]
    # Prepare output
    user.num.questions <- 0
    user.num.correct   <- 0
    user.payment       <- 0.5
    user.id            <- user$userid[1]
    user.viz           <- user$viz[1]
    user.timestamp     <- user$timestart[1]
    # Loop and count
    for (i in 1:nrow(user)) {
        user.num.questions <- user.num.questions + 1
        if (user$correct[i] == "true") user.num.correct  <- user.num.correct + 1
    }
    if (user.num.correct > 7 ) user.payment <- 1.5
    if (user.num.correct > 10) user.payment <- 3.0
    # output
    # checking if this is the latest study...
    if (user.timestamp > 1476000000000) {
        ls.names     <- c(ls.names,     toString(user.id))
        ls.viz       <- c(ls.viz,       toString(user.viz))
        ls.timestamp <- c(ls.timestamp, toString(user.timestamp))
        ls.questions <- c(ls.questions, user.num.questions)
        ls.correct   <- c(ls.correct,   user.num.correct)
        ls.payment   <- c(ls.payment,   user.payment)
    }
}

# create a dataframe for the output
output <- data.frame(ls.names,
                     ls.viz,
                     ls.questions,
                     ls.timestamp,
                     ls.correct,
                     ls.payment)

write.csv(output, file = "bonus_payments2.csv")
