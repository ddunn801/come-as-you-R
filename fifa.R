library(cluster)
library(fpc)
library(ggplot2)

# setwd("C:\\Users\\ddunn\\Dropbox\\DB Cloud-Only Files\\R\\fifa")
setwd("Z:\\LAD\\CMG Direct Marketing\\Analytics\\R\\Examples\\fifa\\")

sleep.time <- 0.01
pagecount <- 226
pc.ignore <- 0
name.gaplines <- 70
names.page <- 48
names.lastpage <- 24

pageSeq <- seq(from=1,to=pagecount,by=1)
urls.df <- data.frame(pageSeq)
for(i in 1:length(urls.df$pageSeq)){
  urls.df$url[i] <- paste0("http://www.futhead.com/14/players/?page=",
                           urls.df$pageSeq[i],
                           "&sort_direction=desc")
}

pages <- as.list("na")
for(j in 1:length(urls.df$pageSeq)){
  pages[[j]] <- urls.df$pageSeq[j]
  download.file(urls.df$url[j],destfile=paste0(urls.df$pageSeq[j],".txt"))
  Sys.sleep(sleep.time)
}

namSeq <- seq(from=1445,by=name.gaplines,length.out=names.page)
posSeq <- seq(from=1450,by=name.gaplines,length.out=names.page)
RATSeq <- seq(from=1455,by=name.gaplines,length.out=names.page)
PACSeq <- seq(from=1460,by=name.gaplines,length.out=names.page)
SHOSeq <- seq(from=1465,by=name.gaplines,length.out=names.page)
PASSeq <- seq(from=1470,by=name.gaplines,length.out=names.page)
DRISeq <- seq(from=1475,by=name.gaplines,length.out=names.page)
DEFSeq <- seq(from=1479,by=name.gaplines,length.out=names.page)
HEASeq <- seq(from=1482,by=name.gaplines,length.out=names.page)

attribs <- data.frame(matrix(nrow=names.page*225+names.lastpage,ncol=9))
colnames(attribs) <- c("Name","Position","RAT","PAC","SHO","PAS","DRI","DEF","HEA")

for(m in 1:(pagecount-pc.ignore)){
  page <- readLines(paste0(urls.df$pageSeq[m],".txt"))
  for(k in 1:names.page){
    n <- (m-1)*names.page+k
    attribs$Name[n] <- page[namSeq[k]]
    attribs$Position[n] <- page[posSeq[k]]
    attribs$RAT[n] <- page[RATSeq[k]]
    attribs$PAC[n] <- page[PACSeq[k]]
    attribs$SHO[n] <- page[SHOSeq[k]]
    attribs$PAS[n] <- page[PASSeq[k]]
    attribs$DRI[n] <- page[DRISeq[k]]
    attribs$DEF[n] <- page[DEFSeq[k]]
    attribs$HEA[n] <- page[HEASeq[k]]
  }
}

pagelast <- readLines(paste0(urls.df$pageSeq[pagecount],".txt"))
for(p in 1:names.lastpage){
  q <- (pagecount-1)*names.page+p
  attribs$Name[q] <- pagelast[namSeq[p]]
  attribs$Position[q] <- pagelast[posSeq[p]]
  attribs$RAT[q] <- pagelast[RATSeq[p]]
  attribs$PAC[q] <- pagelast[PACSeq[p]]
  attribs$SHO[q] <- pagelast[SHOSeq[p]]
  attribs$PAS[q] <- pagelast[PASSeq[p]]
  attribs$DRI[q] <- pagelast[DRISeq[p]]
  attribs$DEF[q] <- pagelast[DEFSeq[p]]
  attribs$HEA[q] <- pagelast[HEASeq[p]]
}

attribs$Name <- gsub("^.*<span class=\"name\">","",attribs$Name)
attribs$Name <- gsub("</span>.*$","",attribs$Name)
attribs$Position <- gsub("^ *","",attribs$Position)
attribs$RAT <- gsub("^ *","",attribs$RAT)
attribs$PAC <- gsub("^ *","",attribs$PAC)
attribs$SHO <- gsub("^ *","",attribs$SHO)
attribs$PAS <- gsub("^ *","",attribs$PAS)
attribs$DRI <- gsub("^ *","",attribs$DRI)
attribs$DEF <- gsub("^.*<span class=\"attribute\">","",attribs$DEF)
attribs$DEF <- gsub("</span>.*$","",attribs$DEF)
attribs$HEA <- gsub("^.*<span class=\"attribute\">","",attribs$HEA)
attribs$HEA <- gsub("</span>.*$","",attribs$HEA)

attribs$Position <- as.factor(attribs$Position)
attribs$RAT <- as.numeric(attribs$RAT)
attribs$PAC <- as.numeric(attribs$PAC)
attribs$SHO <- as.numeric(attribs$SHO)
attribs$PAS <- as.numeric(attribs$PAS)
attribs$DRI <- as.numeric(attribs$DRI)
attribs$DEF <- as.numeric(attribs$DEF)
attribs$HEA <- as.numeric(attribs$HEA)

attribs <- attribs[!duplicated(attribs$Name),]

attribs$Type[attribs$Position=="LB"] <- "Defense"
attribs$Type[attribs$Position=="RB"] <- "Defense"
attribs$Type[attribs$Position=="CB"] <- "Defense"
attribs$Type[attribs$Position=="LWB"] <- "Defense"
attribs$Type[attribs$Position=="RWB"] <- "Defense"
attribs$Type[attribs$Position=="LM"] <- "Midfield"
attribs$Type[attribs$Position=="RM"] <- "Midfield"
attribs$Type[attribs$Position=="CDM"] <- "Midfield"
attribs$Type[attribs$Position=="CM"] <- "Midfield"
attribs$Type[attribs$Position=="CAM"] <- "Midfield"
attribs$Type[attribs$Position=="LW"] <- "Midfield"
attribs$Type[attribs$Position=="RW"] <- "Midfield"
attribs$Type[attribs$Position=="CF"] <- "Forward"
attribs$Type[attribs$Position=="ST"] <- "Forward"


wss <- (nrow(attribs)-1)*sum(apply(attribs[,4:9],2,var))
for (w in 2:10){
  wss[w] <- sum(kmeans(attribs[,4:9],centers=w)$withinss)
}
plot(1:10,wss,type="b",xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit5 <- kmeans(attribs[,4:9],5)
aggregate(attribs[,4:9],by=list(fit5$cluster),FUN=mean)
attribs$kM5 <- fit5$cluster
plotcluster(attribs[,4:9],attribs$kM5)

ggplot(attribs,aes(x=reorder(Position,RAT,FUN=median),y=RAT)) +
  geom_boxplot(aes(fill=Position)) + geom_jitter() +
  xlab("Position") + ylab("Rating") +ggtitle("Player Ratings by Position")

par(mfrow=c(1,1))
def.lm <- lm(RAT~.,data=attribs[attribs$Type=="Defense",3:9])
summary(def.lm)
plot(attribs$RAT[attribs$Type=="Defense"],def.lm$fitted,cex=0.5,col="blue",pch=19,
     xlab="Actual Rating",ylab="Predicted Rating",main="Defense")
plot(def.lm$resid,xlab="Observation",ylab="Residual",main="Defense")
dotplot(def.lm$coefficients[2:7][order(def.lm$coefficients[2:7])],
        xlab="Weighting",main="Defense")

mid.lm <- lm(RAT~.,data=attribs[attribs$Type=="Midfield",3:9])
summary(mid.lm)
plot(attribs$RAT[attribs$Type=="Midfield"],mid.lm$fitted,cex=0.5,col="blue",pch=19,
     xlab="Actual Rating",ylab="Predicted Rating",main="Midfield")
plot(mid.lm$resid,xlab="Observation",ylab="Residual",main="Midfield")
dotplot(mid.lm$coefficients[2:7][order(mid.lm$coefficients[2:7])],
        xlab="Weighting",main="Midfield")

for.lm <- lm(RAT~.,data=attribs[attribs$Type=="Forward",3:9])
summary(for.lm)
plot(attribs$RAT[attribs$Type=="Forward"],for.lm$fitted,cex=0.5,col="blue",pch=19,
     xlab="Actual Rating",ylab="Predicted Rating",main="Forward")
plot(for.lm$resid,xlab="Observation",ylab="Residual",main="Forward")
dotplot(for.lm$coefficients[2:7][order(for.lm$coefficients[2:7])],
        xlab="Weighting",main="Forward")

attribs$Def.Pred <- predict(def.lm,newdata=attribs[,3:9])
attribs$Mid.Pred <- predict(mid.lm,newdata=attribs[,3:9])
attribs$For.Pred <- predict(for.lm,newdata=attribs[,3:9])

attribs$Best.Pred <- apply(attribs[,12:14],1,max)
attribs$Best <- names(attribs[,12:14])[max.col(attribs[,12:14])]
attribs$Best[attribs$Best=="Def.Pred"] <- "Defense"
attribs$Best[attribs$Best=="Mid.Pred"] <- "Midfield"
attribs$Best[attribs$Best=="For.Pred"] <- "Forward"
table(attribs$Type,attribs$Best)

head(attribs[attribs$Type=="Defense"&attribs$Best=="Forward",],3)  # Defense better as Forward
head(attribs[attribs$Type=="Defense"&attribs$Best=="Midfield",],3)  # Defense better as Midfield
head(attribs[attribs$Type=="Midfield"&attribs$Best=="Defense",],3)  # Midfield better as Defense
head(attribs[attribs$Type=="Midfield"&attribs$Best=="Forward",],3)  # Midfield better as Forward
head(attribs[attribs$Type=="Forward"&attribs$Best=="Defense",],3)  # Forward better as Defense
head(attribs[attribs$Type=="Forward"&attribs$Best=="Midfield",],3)  # Forward better as Midfield

attribs$Pos.Pred[attribs$Type=="Defense"] <- attribs$Def.Pred[attribs$Type=="Defense"]
attribs$Pos.Pred[attribs$Type=="Midfield"] <- attribs$Mid.Pred[attribs$Type=="Midfield"]
attribs$Pos.Pred[attribs$Type=="Forward"] <- attribs$For.Pred[attribs$Type=="Forward"]
attribs$Pos.xFactor <- attribs$Pos.Pred - attribs$RAT
attribs$xFactor <- attribs$Best.Pred - attribs$RAT

head(attribs[order(attribs$Pos.xFactor),c("Name","Position","RAT","Pos.Pred","xFactor")],3)  # worse than predicted in position
tail(attribs[order(attribs$Pos.xFactor),c("Name","Position","RAT","Pos.Pred","xFactor")],3)  # better than predicted in position

head(attribs[order(attribs$xFactor),c("Name","Position","RAT","Best.Pred","xFactor")],3)  # worse than predicted
tail(attribs[order(attribs$xFactor),c("Name","Position","RAT","Best.Pred","xFactor")],3)  # better than predicted

