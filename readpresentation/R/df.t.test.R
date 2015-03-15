df.t.test <- function(df,group,col,indcol){
    t.test.helper <- function(x,col,indcol,group){
        tob <- t.test(x[,col] ~ x[,indcol])
        tmp <- data.frame(data = paste(col,"by",indcol),
                          group = x[1,group],
                          mean.group.1 = tob$estimate[1],
                          mean.group.2 = tob$estimate[2],
                          name.test.stat = tob$statistic,
                          conf.lower = tob$conf.int[1],
                          conf.upper = tob$conf.int[2],
                          pval = tob$p.value,
                          alternative = tob$alternative,
                          tob$method)
        names(tmp)[3:4] <- make.names(names(tob$estimate))
        row.names(tmp) <- x[1,group]
        tmp
    }
    df.l <- split(df[,c(col,indcol,group)],df[,group])
    Reduce(rbind,lapply(df.l,t.test.helper,col=col,indcol=indcol,group=group))}
