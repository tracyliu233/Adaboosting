#========= Q(1) Build adaboosting ==========

# generate data and initialize the weight
train <- data.frame(x = runif(40), y = rbinom(40, 1, 0.5))
train$w <- 1/nrow(train)
train <- train[order(train$x),]

Geni <- function(A, B){
# This function calculate the geni 
    # Geni A
    vote.A.0 <- sum(A$w[A$y == 0])
    vote.A.1 <- sum(A$w[A$y == 1])
    # ! the classification here need to consider w not count
    if (vote.A.0 > vote.A.1){
        A$c <- 0
    }else{
        A$c <- 1
    }
    g.A.0 <- vote.A.0 / (vote.A.0 + vote.A.1)
    g.A.1 <- vote.A.1 / (vote.A.0 + vote.A.1)   
    g.A <- g.A.0*(1-g.A.0) + g.A.1*(1-g.A.1)
    
    # Geni B
    vote.B.0 <- sum(B$w[B$y == 0])
    vote.B.1 <- sum(B$w[B$y == 1])
    if (vote.B.0 > vote.B.1){
        B$c <- 0
    }else{
        B$c <- 1
    }
    g.B.0 <- vote.B.0 / (vote.B.0 + vote.B.1)
    g.B.1 <- vote.B.1 / (vote.B.0 + vote.B.1)   
    g.B <- g.B.0*(1-g.B.0) + g.B.1*(1-g.B.1)
    
    # Geni
    g <- g.A*nrow(A)/nrow(train) + g.B*nrow(B)/nrow(train)
    list(A, B, g)
}

DT <- function(train) {
# Build decision tree
    max.g <- 0
    for (i in c(1:(nrow(train)-1))){
        A <- train[c(1:i),]
        B <- train[c((i+1):nrow(train)),]
        Geni.list <- Geni(A,B)
        A <- Geni.list[[1]]
        B <- Geni.list[[2]]
        g <- Geni.list[[3]]
        if (max.g < g){
            max.g <- g
            ind.g <- i
        }
    }
    # update the new classifier
    A <- train[c(1:ind.g),]
    B <- train[c((ind.g+1):nrow(train)),]
    Geni.list <- Geni(A,B)
    A <- Geni.list[[1]]
    B <- Geni.list[[2]]
    g <- Geni.list[[3]]
    f.count <- sum(A$y!=A$c) + sum(B$y!=B$c)
    err <- (sum(A$w[A$y!=A$c]) + sum(B$w[B$y!=B$c]))/sum(A$w + B$w)
    alpha <- log((1-err)/err)
    A$w[A$y!=A$c] <- A$w[A$y!=A$c]*exp(alpha)
    B$w[B$y!=B$c] <- B$w[B$y!=B$c]*exp(alpha)
    G <- rbind(A,B)
    G$alpha <- alpha
    G
}

# Adaboosting
Ada <- function(train, M){
    G.m <- train
    G.list <- list()
    for (i in c(1:M)){
        G.list[[i]] <- DT(G.m)
        G.m <- DT(G.m)
    }
    # error rate
    df <- data.frame(train$x)
    for (i in c(1:length(G.list))){
        result <- G.list[[i]]$c*G.list[[i]]$alpha
        df <- cbind(df, result)
    }
    ada.class <- apply(df[,c(2:ncol(df))], 1, function(x) if(sum(x)>0.5) 1 else 0)
    err <- sum(ada.class != train$y) / length(ada.class)
    err
}

#========== Q(2) ==========
#I don't understand this question

#========== Q(3) Optimal iteration ==========
Ada(train, 2)
Ada(train, 3)
Ada(train, 4)
Ada(train, 5)
# The result shows that after the first iteration the error rate has been stable.
# Since the dataset I generate only have one fearture X in order to simplify 
# building the decision tree. This result looks resonable.


#========== Q(4) Compare with built in package gbm ===========
library(gbm)
gbm1 <- gbm(y ~ x, data=train, n.trees=5, bag.fraction = 0.7)
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)

# Also this built-in packadge shows that at the first iteration it can already
# gives a optimal result.
# Note: this calling this build-in package on this dataset may generate some
# different results, but most of the time the optimal iteration will be one.


