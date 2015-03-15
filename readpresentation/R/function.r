read.file <- function(file,skip,verbose=T){
    if(verbose) print(paste("read", file))
    tmp <- read.table(file,skip = skip,sep = "\t",
                      header=T,na.strings = c(" +",""),
                      fill=T)
    
    tmp <- tmp[!is.na(tmp$Subject),] 

    if(sum(!str_detect(tmp$Subject,"^0[012][0-9]_[1-8]$|^0[012][0-9]_test[12]$")))
        print(paste("id",tmp$Subject[1]))
    
    if(sum(tmp$Stim.Type %in% c("hit","incorrect"))==0) return(NULL)

    tmp <- lapply(tmp,function(x) {
        if( class(x) %in% c("character","factor") ){
            x <- factor(gsub(" ","",as.character(x)))
            return(x)}else{ return(x) }})
    
    tmp <- as.data.frame(tmp)
    
    pause <- which(tmp$Event.Type=="Picture" & tmp$Code=="Pause")
    if(length(pause)>0){
        drei <- which(tmp$Code==3 & !is.na(tmp$Code))
        drei <- drei[drei > pause][1:2]
        if(pause + 1 < drei[1]){
            tmp <- tmp[-(pause:drei[2]),]
        }}

    
    tmp <- tmp[!(tmp$Event.Type %in% c("Pause","Resume")), ]

    first.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) )) - 1
    tmp <- tmp[-(1:first.pic),]

    last.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) &
                              tmp$Code=="Fertig!" & !is.na(tmp$Code)))
    tmp <- tmp[-(last.pic:nrow(tmp)),]

    zeilen <- which(tmp$Event.Type %in% c("Response"))
    zeilen <- sort(unique(c(zeilen,zeilen-1)))
    zeilen <- zeilen[zeilen>0]
    tmp <- tmp[zeilen,]
    
    responses <- which(tmp$Code %in% c(1,2))
    events <- responses-1
    tmp$Type <- NA
    tmp$Type[responses] <- as.character(tmp$Event.Type[events])

    if(length(tmp$Type[responses])!=length(tmp$Event.Type[events])) { print(file)}
    tmp$Event.Code <- NA
    tmp$Event.Code[responses] <- as.character(tmp$Code[events])
    tmp$Time1 <- NA
    tmp$Time1[responses] <- tmp$Time[events]
    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp$Duration[responses] <- as.character(tmp$Duration[events])
    tmp$Uncertainty.1[responses] <- as.character(tmp$Uncertainty.1[events])
    tmp$ReqTime[responses] <- as.character(tmp$ReqTime[events])
    tmp$ReqDur[responses] <- as.character(tmp$ReqDur[events])
    tmp$Pair.Index[responses] <- as.character(tmp$Pair.Index[events])
    

    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp <- tmp[tmp$Event.Type=="Response" & !is.na(tmp$Type),]
    tmp <- tmp[tmp$Type=="Picture" & !is.na(tmp$Type),]
    return(tmp)
}

read.files <- function(filesdir,skip=3,recursive=F,pattern="."){
    files <- dir(filesdir,
                 full.names = T,
                 recursive = recursive,
                 pattern = pattern)
    Reduce(rbind,lapply(files,read.file,skip=skip))}
