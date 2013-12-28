###  Prepare  ####
library(cluster)
library(fpc)
library(ggplot2)
library(foreign)
library(ggdendro)
library(reshape2)

set.seed(4444)
setwd("C:\\Users\\ddunn\\Dropbox\\DB Cloud-Only Files\\R\\fifa")
#  setwd("Z:\\LAD\\CMG Direct Marketing\\Analytics\\R\\Examples\\fifa\\")


###  Control panel for screen-scraping  ####
sleep.time <- 0.01
pagecount <- 226
pc.ignore <- 0
names.page <- 48
names.lastpage <- 24
name.gaplines <- 70
namLine1 <- 1446
posLine1 <- 1451
RATLine1 <- 1456
PACLine1 <- 1461
SHOLine1 <- 1466
PASLine1 <- 1471
DRILine1 <- 1476
DEFLine1 <- 1480
HEALine1 <- 1483
TopRAT <- 87.5


###  Create custom urls to scrape  ####
pageSeq <- seq(from=1,to=pagecount,by=1)
urls.df <- data.frame(pageSeq)
for(i in 1:length(urls.df$pageSeq)){
  urls.df$url[i] <- paste0("http://www.futhead.com/14/players/?page=",
                           urls.df$pageSeq[i],
                           "&sort_direction=desc")
}


###  Scrape html from custom urls  ####
pages <- as.list("na")
for(j in 1:length(urls.df$pageSeq)){
  pages[[j]] <- urls.df$pageSeq[j]
  download.file(urls.df$url[j],destfile=paste0(urls.df$pageSeq[j],".txt"))
  Sys.sleep(sleep.time)
}


###  Identify which lines store player statistics  ####
namSeq <- seq(from=namLine1,by=name.gaplines,length.out=names.page)
posSeq <- seq(from=posLine1,by=name.gaplines,length.out=names.page)
RATSeq <- seq(from=RATLine1,by=name.gaplines,length.out=names.page)
PACSeq <- seq(from=PACLine1,by=name.gaplines,length.out=names.page)
SHOSeq <- seq(from=SHOLine1,by=name.gaplines,length.out=names.page)
PASSeq <- seq(from=PASLine1,by=name.gaplines,length.out=names.page)
DRISeq <- seq(from=DRILine1,by=name.gaplines,length.out=names.page)
DEFSeq <- seq(from=DEFLine1,by=name.gaplines,length.out=names.page)
HEASeq <- seq(from=HEALine1,by=name.gaplines,length.out=names.page)


###  Create empty dataframe for storing player stats
attribs <- data.frame(matrix(nrow=names.page*225+names.lastpage,ncol=9))
colnames(attribs) <- c("Name","Position","RAT","PAC","SHO","PAS","DRI","DEF","HEA")


###  Store lines from full pages containing player stats to dataframe  ####
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


###  Store lines from partial last page containing player stats to dataframe  ####
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


###  Remove html wrapped around player stats in each line  ####
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


###  Remove statisitcs from duplicated players  ####
attribs <- attribs[!duplicated(attribs$Name),]
rownames(attribs) <- NULL


###  Clean up foreign characters in names  ####
Encoding(attribs$Name) <- "UTF-8"
attribs$Name <- iconv(attribs$Name,"UTF-8","UTF-8",sub='')


###  Create general position type  ####
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


###  Change each stat to the appropriate data type  ####
attribs$Position <- as.factor(attribs$Position)
attribs$RAT <- as.numeric(attribs$RAT)
attribs$PAC <- as.numeric(attribs$PAC)
attribs$SHO <- as.numeric(attribs$SHO)
attribs$PAS <- as.numeric(attribs$PAS)
attribs$DRI <- as.numeric(attribs$DRI)
attribs$DEF <- as.numeric(attribs$DEF)
attribs$HEA <- as.numeric(attribs$HEA)
attribs$Type <- ordered(attribs$Type,levels=c("Defense","Midfield","Forward"))


###  Linear models for each type of position  ####
def.lm <- lm(RAT~.,data=attribs[attribs$Type=="Defense",3:9])
mid.lm <- lm(RAT~.,data=attribs[attribs$Type=="Midfield",3:9])
for.lm <- lm(RAT~.,data=attribs[attribs$Type=="Forward",3:9])
summary(def.lm)
summary(mid.lm)
summary(for.lm)

attribs$Def.Pred <- predict(def.lm,newdata=attribs[,3:9])
attribs$Mid.Pred <- predict(mid.lm,newdata=attribs[,3:9])
attribs$For.Pred <- predict(for.lm,newdata=attribs[,3:9])


###  Find best predicted type by rating  ####
attribs$Best.Pred <- apply(attribs[,11:13],1,max)
attribs$Best <- names(attribs[,11:13])[max.col(attribs[,11:13])]
attribs$Best[attribs$Best=="Def.Pred"] <- "Defense"
attribs$Best[attribs$Best=="Mid.Pred"] <- "Midfield"
attribs$Best[attribs$Best=="For.Pred"] <- "Forward"
attribs$Best <- ordered(attribs$Best,levels=c("Defense","Midfield","Forward"))
table(attribs$Type,attribs$Best)


###  Create attribute of predicted rating by actual type
attribs$Type.Pred[attribs$Type=="Defense"] <- attribs$Def.Pred[attribs$Type=="Defense"]
attribs$Type.Pred[attribs$Type=="Midfield"] <- attribs$Mid.Pred[attribs$Type=="Midfield"]
attribs$Type.Pred[attribs$Type=="Forward"] <- attribs$For.Pred[attribs$Type=="Forward"]
attribs$Type.xFactor <- attribs$Type.Pred - attribs$RAT
attribs$xFactor <- attribs$Best.Pred - attribs$RAT


###  Create row number by Type  ####
attribs$TypeRow[attribs$Type=="Defense"] <- seq(length=nrow(attribs[attribs$Type=="Defense",]))
attribs$TypeRow[attribs$Type=="Midfield"] <- seq(length=nrow(attribs[attribs$Type=="Midfield",]))
attribs$TypeRow[attribs$Type=="Forward"] <- seq(length=nrow(attribs[attribs$Type=="Forward",]))


###  Find players who would be ranked higher in a different type  ####
head(attribs[attribs$Type=="Defense"&attribs$Best=="Forward",],3)  # D better as F
head(attribs[attribs$Type=="Defense"&attribs$Best=="Midfield",],3)  # D better as M
head(attribs[attribs$Type=="Midfield"&attribs$Best=="Defense",],3)  # M better as D
head(attribs[attribs$Type=="Midfield"&attribs$Best=="Forward",],3)  # M better as F
head(attribs[attribs$Type=="Forward"&attribs$Best=="Defense",],3)  # F better as D
head(attribs[attribs$Type=="Forward"&attribs$Best=="Midfield",],3)  # F better as M


###  Find outlier predicted players by 
head(attribs[order(attribs$Type.xFactor),
             c("Name","Position","RAT","Type.Pred","xFactor")],3)  # < pred in position
tail(attribs[order(attribs$Type.xFactor),
             c("Name","Position","RAT","Type.Pred","xFactor")],3)  # > pred in position
head(attribs[order(attribs$xFactor),
             c("Name","Position","RAT","Best.Pred","xFactor")],3)  # < pred
tail(attribs[order(attribs$xFactor),
             c("Name","Position","RAT","Best.Pred","xFactor")],3)  # > pred


###  Prediction and residual charts for each linear model  ####
ggplot(attribs) + 
  aes(x=RAT,y=Type.Pred) + 
  geom_point(aes(color=Type),size=3,alpha=0.3) + 
  facet_grid(Type~.) + 
  theme(legend.position="none") + 
  xlab("Actual Rating") + ylab("Predicted Rating") + ggtitle("Model Performance")
ggplot(attribs) + 
  aes(x=Type.xFactor) + 
  geom_density(aes(color=Type,fill=Type),alpha=0.3) + 
  xlab("Residual") + ylab("Density") + ggtitle("Model Residuals") + 
  geom_vline(xintercept=0,color="red",size=1,linetype=2) + 
  coord_cartesian(xlim = c(-15,15))


###  Dotplots of coefficient weights  ####
ggplot() + 
  aes(x=def.lm$coefficients[2:7],
                y=reorder(names(def.lm$coefficients[2:7]),def.lm$coefficients[2:7])) + 
  geom_point(color="red",size=10) + 
  theme(panel.background=element_rect(fill="darkgray")) + 
  theme(legend.position="none") + 
  xlab("Weight") + ylab("Attribute") + ggtitle("Attribute Weighting for Defense") + 
  geom_vline(xintercept=0,color="gold",size=2,linetype=2) + 
  coord_cartesian(xlim = c(-0.05,0.8))
ggplot() + 
  aes(x=mid.lm$coefficients[2:7],
                y=reorder(names(mid.lm$coefficients[2:7]),mid.lm$coefficients[2:7])) + 
  geom_point(color="green",size=10) + 
  theme(panel.background=element_rect(fill="darkgray")) + 
  theme(legend.position="none") + 
  xlab("Weight") + ylab("Attribute") + ggtitle("Attribute Weighting for Midfield") + 
  geom_vline(xintercept=0,color="gold",size=2,linetype=2) + 
  coord_cartesian(xlim = c(-0.05,0.8))
ggplot() + 
  aes(x=for.lm$coefficients[2:7],
                y=reorder(names(for.lm$coefficients[2:7]),for.lm$coefficients[2:7])) + 
  geom_point(color="blue",size=10) + 
  theme(panel.background=element_rect(fill="darkgray")) + 
  theme(legend.position="none") + 
  xlab("Weight") + ylab("Attribute") + ggtitle("Attribute Weighting for Forward") + 
  geom_vline(xintercept=0,color="gold",size=2,linetype=2) + 
  coord_cartesian(xlim = c(-0.05,0.8))


###  Boxcharts of player ratings by type and position  ####
ggplot(attribs) + 
  aes(x=Type,y=RAT) + 
  geom_boxplot(aes(fill=Type)) + geom_jitter(size=0.5) + 
  theme(legend.position="none") + 
  ylab("Rating") +ggtitle("Player Ratings by Type")
ggplot(attribs) + 
  aes(x=reorder(Position,RAT,FUN=median),y=RAT) + 
  geom_boxplot(aes(fill=Position)) + geom_jitter(size=0.5) + 
  theme(legend.position="none") + 
  ylab("Rating") +ggtitle("Player Ratings by Position")


###  k-means clustering  ####
wss <- (nrow(attribs)-1)*sum(apply(attribs[,4:9],2,var))
for (w in 2:9){
  wss[w] <- sum(kmeans(attribs[,4:9],centers=w)$withinss)
}
clusterNums <- seq(from=1,to=9,by=1)
ggplot() + 
  aes(x=clusterNums,y=wss) +
  geom_point(color="brown",size=10) + 
  coord_cartesian(xlim = c(0,10)) + 
  scale_x_continuous(breaks=1:9) + 
  ylab("Weighted Sum of Squares") + xlab("Number of Clusters") + 
  ggtitle("WSS by Cluster Count")
fit3 <- kmeans(attribs[,4:9],3)
attribs$kM3 <- fit3$cluster


###  Hierarchical clustering  ####
Top <- attribs[attribs$RAT>TopRAT,4:9]
rownames(Top) <- attribs$Name[attribs$RAT>TopRAT]
ggd.dist <- dist(Top,method="euclidean")
ggd.hc <- hclust(ggd.dist,method="ward")
ggd.dhc <- as.dendrogram(ggd.hc)
ggd.data <- dendro_data(ggd.dhc,type="rectangle")
labs <- attribs[attribs$RAT>TopRAT,"Name"]
pos <- attribs[attribs$RAT>TopRAT,"Type"]
ggplot(segment(ggd.data)) + 
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) + 
  geom_text(data=label(ggd.data),aes(x=x,y=y,label=labs,hjust=0,color=pos)) + 
  coord_flip() + scale_y_reverse(expand=c(0.2,0)) + 
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank()) + 
  theme(legend.justification=c(0,0), legend.position=c(0.25,0.6)) + 
  ggtitle("Player Tree")

