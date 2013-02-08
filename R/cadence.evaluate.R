cadence.evaluate <-
function(x, W1, W2, hidden.fcn, distribution)
{
    if(!is.null(distribution$parameters.fixed)){
        colnames(W2) <- distribution$parameters
        W2[1:(nrow(W2)-1),distribution$parameters.fixed] <- 0.
    }
    x <- cbind(x, 1)
    h1 <- x %*% W1
    y1 <- hidden.fcn(h1)
    aug.y1 <- cbind(y1, 1)
    y2 <- aug.y1 %*% W2
    colnames(y2) <- distribution$parameters
    for(i in seq_along(distribution$parameters)){
        y2[,i] <- distribution$output.fcns[[i]](y2[,distribution$parameters[i]])
    }
    y2
}

