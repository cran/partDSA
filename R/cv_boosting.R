
cv.boosting <- function(training.data,training.y,wt,control)
{
    boost.rounds <- control$boost.rounds
    n.training <- length(training.y)
    cv.div <- n.training%/%5
    cv.mod <- n.training%%5
    
 #Now run 5-fold cross-validation

    error5.list <- vector("list",5) 
    for(i in 1:5) error5.list[[i]] <- matrix(NA_real_,4,boost.rounds)

#    error51.cv <- matrix(NA_real_,4,boost.rounds)
#    error52.cv <- matrix(NA_real_,4,boost.rounds)
#    error53.cv <- matrix(NA_real_,4,boost.rounds)
#    error54.cv <- matrix(NA_real_,4,boost.rounds)
#    error55.cv <- matrix(NA_real_,4,boost.rounds)
  
    samp5.cv <- sample(1:n.training,replace=FALSE)
    samp5.n <- rep(cv.div,5)
    if(cv.mod>0) samp5.n[1:cv.mod] <- samp5.n[1:cv.mod]+1
    samp5.ends <- cumsum(samp5.n)
    samp5.starts <- c(1,samp5.ends[1:4]+1)
    samp5.list <- vector("list",5)
    for(i in 1:5) samp5.list[[i]] <- sort(samp5.cv[samp5.starts[i]:samp5.ends[i]])
#    samp51.cv <- sort(samp5.cv[1:cv.size])
#    samp52.cv <- sort(samp5.cv[(cv.size+1):(2*cv.size)])
#    samp53.cv <- sort(samp5.cv[(2*cv.size+1):(3*cv.size)])
#    samp54.cv <- sort(samp5.cv[(3*cv.size+1):(4*cv.size)])
#    samp55.cv <- sort(samp5.cv[(4*cv.size+1):(5*cv.size)])
    training5.list <- vector("list",5)
    test5.list <- vector("list",5)
    for(i in 1:5) training5.list[[i]] <- training.data[-samp5.list[[i]],]
    for(i in 1:5) test5.list[[i]] <- training.data[samp5.list[[i]],]
#    training51.data <- training.data[-samp51.cv,]
#    training52.data <- training.data[-samp52.cv,]
#    training53.data <- training.data[-samp53.cv,]
#    training54.data <- training.data[-samp54.cv,]
#    training55.data <- training.data[-samp55.cv,]
    trainingy5.list <- vector("list",5)
    testy5.list <- vector("list",5)
    for(i in 1:5) trainingy5.list[[i]] <- training.y[-samp5.list[[i]]]
    for(i in 1:5) testy5.list[[i]] <- training.y[samp5.list[[i]]]
#    training51.y <- training.y[-samp51.cv]
#    training52.y <- training.y[-samp52.cv]
#    training53.y <- training.y[-samp53.cv]
#    training54.y <- training.y[-samp54.cv]
#    training55.y <- training.y[-samp55.cv]
    wt5.list <- vector("list",5)
    for(i in 1:5) wt5.list[[i]] <- wt[-samp5.list[[i]]]

    for(i in 2:5)
        {
            print(paste(i,"Boosting Partitions"))
            control$cut.off.growth <- i
                                        #            control <- DSA.control(vfold=10, cut.off.growth = boost.part, loss.function = "default",MPD = mpd,boost=1,boost.rounds=boost.rounds)
            for(j in 1:5)
                {
                    pdfit <- run.boosting(x=training5.list[[j]],y=as.matrix(trainingy5.list[[j]]),wt=wt5.list[[j]],x.test=test5.list[[j]],y.test=testy5.list[[j]],control = control)
                    error5.list[[j]][i-1,] <- pdfit$Test.Set.Errors
#            pd51.cv <- partDSA(x=training51.data,y=as.matrix(training51.y),x.test=training.data[samp51.cv,],y.test=training.y[samp51.cv],control = control)
#            error51.cv[i-1,] <- pd51.cv$Test.Set.Error
#            pd52.cv <- partDSA(x=training52.data,y=as.matrix(training52.y),x.test=training.data[samp52.cv,],y.test=training.y[samp52.cv],control = control)
#            error52.cv[i-1,] <- pd52.cv$Test.Set.Error
#            pd53.cv <- partDSA(x=training53.data,y=as.matrix(training53.y),x.test=training.data[samp53.cv,],y.test=training.y[samp53.cv],control = control)
#            error53.cv[i-1,] <- pd53.cv$Test.Set.Error
#            pd54.cv <- partDSA(x=training54.data,y=as.matrix(training54.y),x.test=training.data[samp54.cv,],y.test=training.y[samp54.cv],control = control)
#            error54.cv[i-1,] <- pd54.cv$Test.Set.Error
#            pd55.cv <- partDSA(x=training55.data,y=as.matrix(training55.y),x.test=training.data[samp55.cv,],y.test=training.y[samp55.cv],control = control)
#            error55.cv[i-1,] <- pd55.cv$Test.Set.Error
                }
            }

    error.5cv <- error5.list[[1]]+error5.list[[2]]+error5.list[[3]]+error5.list[[4]]+error5.list[[5]]
#    error.5cv <- error51.cv+error52.cv+error53.cv+error54.cv+error55.cv    
    min.5partitions <- apply(error.5cv,1,min)
    order.5min.partitions <- order(min.5partitions)[1]
    boost.5part <- order.5min.partitions+1
    boost.5rounds <- order(error.5cv[order.5min.partitions,])[1]
    list(boost.5part=boost.5part,boost.5rounds=boost.5rounds)
#    print(paste("Real Boosting 5-cv with",boost.5part,"partitions and",boost.5rounds,"rounds"))
#    control <- DSA.control(vfold=10, cut.off.growth = boost.5part, loss.function = "default",MPD = mpd,boost=1,boost.rounds=boost.5rounds)
#    pd5 <- partDSA(x=training.data,y=as.matrix(training.y),x.test=test.data,y.test=test.y,control = control)
#    ErrorToTruthCurrent <- sum((pd5$Predicted.Test.Set.Values-test.y)^2)/n.test
#    list(ErrorToTruthCurrent=ErrorToTruthCurrent,boost.5part=boost.5part,boost.5rounds=boost.5rounds)
}







