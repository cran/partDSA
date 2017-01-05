run.boosting <- function(x=x, y=y, wt=wt, x.test=NULL,y.test=NULL,control=control)
{
    boost.rounds <- control$boost.rounds
    boost.out <- matrix(0,nrow=nrow(x),ncol=boost.rounds)
    boost.models<-vector("list",boost.rounds)
    resids.sum <- NULL
    resids<-y
    for(i in 1:boost.rounds){
        boost.models[[i]]<-rss.dsa(x=x, y=resids, wt=wt, minsplit=control$minsplit, minbuck=control$minbuck,
                                   cut.off.growth=control$cut.off.growth, MPD=control$MPD,missing=control$missing,
                                   loss.function=control$loss.function, control=control,
                                   wt.method=control$wt.method, brier.vec=control$brier.vec, cox.vec=control$cox.vec, IBS.wt=control$IBS.wt)
        boost.models[[i]]$pred.test.set.DSA <- predict(boost.models[[i]], x)
        boost.out[,i]<-boost.models[[i]]$pred.test.set.DSA[,control$cut.off.growth]
        resids <- (resids - boost.out[,i])
        resids.sum[i] <- sum(resids^2)
    }
### Predicted Values
    y.hat.train <- y - resids
    if(!is.null(x.test)){  # For future prediction
        y.hat.test<-rep(0,nrow=x.test)
        test.set.error <- NULL
        for(i in 1:boost.rounds){
            y.hat.test <- y.hat.test +  predict(boost.models[[i]], x.test)[,control$cut.off.growth]
            test.set.error[i]<-sum((y.test - y.hat.test)^2)
        }
    }
    results <- list(resids.sum,
                    boost.models,
                    y.hat.train,
                    y.hat.test,
                    test.set.error,
                    test.set.error[boost.rounds])

    names(results) <- list("Training.Set.Errors", "Training.Set.Models", 
                           "Predicted.Train.Set.Values", 
                           "Predicted.Test.Set.Values", "Test.Set.Errors", "Final.Test.Set.Error")
    class(results)<-('BoostDSA')
    results
}

