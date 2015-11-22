#load libraries

library(jsonlite)
library(dplyr)
library(caret)
library(rvest)
library(broom)
library(mlearning)

#what attributes best predict a well-rated restaurant on yelp?

reviews <- readRDS("reviews.rds")
biz <- readRDS("biz.rds")

#join biz & grouped reviews, remove discounted columns, filter out non-restaurants

reviewgroup <- reviews %>% group_by(business_id) %>% 
  summarise(avgstars=mean(stars),allvf=sum(votes.funny),
            allvu=sum(votes.useful),allc=sum(votes.cool))

grnb <- inner_join(biz,reviewgroup,by="business_id")

grnb <- filter(grnb,grepl("Restaurants",categories))

grnb <- grnb[,-c(2,3,8,14:27,86:93)]

#add urban column

citypage <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population")
cities <- citypage %>% html_nodes("table") %>% .[[4]] %>%  html_table()
citylist <- as.vector(cities[,2])
citylist <- gsub("\\[[0-9]\\]","",citylist)
citylist <- gsub("\\[[0-9][0-9]\\]","",citylist)

grnb <-  mutate(grnb, Urban = ifelse(city %in% citylist , 1, 0))

#filter out rows without enough reviews; minimum of 5 reviews (will trim set by 15%)

grnb <- filter(grnb,review_count>=5)


#delete columns with too many null values; too many = more than 15% null

disregard <- c()
stuff <- c()

for (i in 1:ncol(grnb)) {
  
  ifelse(sum(is.na(grnb[,i]))>(.15*nrow(grnb)),disregard <- c(disregard,i),stuff <- c(stuff,i))
  
}

grnb <- grnb[,-disregard]


#change attribution values from booleans to binaries (false = 0, true = 1)

logiclist <- c()

for (i in 1:ncol(grnb)) {
  
  ifelse(class(grnb[,i])=="logical",logiclist <- c(logiclist,i),c(stuff,i))
  
}       


for (i in 1:length(logiclist)) {
  
grnb[,logiclist[i]] <- gsub("FALSE",0,grnb[,logiclist[i]])
grnb[,logiclist[i]] <- gsub("TRUE",1,grnb[,logiclist[i]])
  
}

#filter out NAs, convert credit card column

grnb[,11] <- gsub("FALSE",0,grnb[,11])
grnb[,11] <- gsub("TRUE",1,grnb[,11])
grnb[,11] <- gsub("NULL","",grnb[,11])

#convert alcohol column to numeric

grnb[,16] <- gsub("full_bar",2,grnb[,16])
grnb[,16] <- gsub("none",0,grnb[,16])
grnb[,16] <- gsub("beer_and_wine",1,grnb[,16])

#convert attire column to numeric

grnb[,18] <- gsub("casual",0,grnb[,18])
grnb[,18] <- gsub("dressy",1,grnb[,18])
grnb[,18] <- gsub("formal",2,grnb[,18])


#convert attire and alcohol attributes to numeric

grnbcomp <- na.omit(grnb)

for (i in 11:33) {
  
  grnbcomp[,i] <- as.numeric(grnbcomp[,i])
  
}

grnbcomp <- na.omit(grnbcomp)

#remove duplicate columns

grnbcomp <- select(grnbcomp,-stars)

##split set

gsplit1 <- createDataPartition(y=grnbcomp$avgstars,p=.4,list=FALSE)
d1<-grnbcomp[gsplit1,]
d2<-grnbcomp[-gsplit1,]

#create validation and test set 

gsplit11 <- createDataPartition(y=d2$avgstars,p=.33,list=FALSE)
d1v<-d2[gsplit11,]
d1t<-d2[-gsplit11,]

#remove attributes with a correlation of greater than 0.9

d1mat <- d1[,c(4,6,8,10:37)]
d1cormat <- cor(d1mat)
d1cormatf <- findCorrelation(d1cormat, cutoff = .90, verbose = TRUE)

d1uncor <- d1mat[,-c(1,28,30)]

#conduct linear regression and identify influential attributes

d1lmset <- d1[,c(4,6,8,10:33,35,37)]
d1lm <- lm(data=d1lmset,avgstars~.)
d1lmvars <- tidy(d1lm)
d1lmkeyvars <- filter(d1lmvars,p.value<=.05)

#build another model with only the attributes that have low p-values 

set2 <- d1lmkeyvars$term
set22 <- gsub("`","",set2)
set22 <- set22[-1]
d1lmset1 <- cbind(d1$avgstars,d1[,set22])
d1lmset1 <- rename(d1lmset1,avgstars = `d1$avgstars`)

d1lm1 <- lm(data=d1lmset1,avgstars~.)


######RE-RUN BUT WITH DIFFERENT COMPLETENESS REQUIREMENT (turn into function later)

grnb2 <- inner_join(biz,reviewgroup,by="business_id")

grnb2 <- filter(grnb2,grepl("Restaurants",categories))

grnb2 <- grnb2[,-c(2,3,8,14:27,86:93)]

#add urban column

grnb2 <-  mutate(grnb2, Urban = ifelse(city %in% citylist , 1, 0))

#filter out rows without enough reviews; minimum of 5 reviews (will trim set by 15%)

grnb2 <- filter(grnb2,review_count>=5)


#delete columns with too many null values; too many = more than 50% null

disregard <- c()
stuff <- c()

for (i in 1:ncol(grnb2)) {
  
  ifelse(sum(is.na(grnb2[,i]))>(.5*nrow(grnb2)),disregard <- c(disregard,i),stuff <- c(stuff,i))
  
}

grnb2 <- grnb2[,-disregard]


#change attribution values from booleans to binaries (false = 0, true = 1)

logiclist <- c()

for (i in 1:ncol(grnb2)) {
  
  ifelse(class(grnb2[,i])=="logical",logiclist <- c(logiclist,i),c(stuff,i))
  
}       


for (i in 1:length(logiclist)) {
  
  grnb2[,logiclist[i]] <- gsub("FALSE",0,grnb2[,logiclist[i]])
  grnb2[,logiclist[i]] <- gsub("TRUE",1,grnb2[,logiclist[i]])
  
}

#filter out NAs, convert credit card column

grnb2$`attributes.Accepts Credit Cards` <- gsub("FALSE",0,grnb2$`attributes.Accepts Credit Cards`)
grnb2$`attributes.Accepts Credit Cards` <- gsub("TRUE",1,grnb2$`attributes.Accepts Credit Cards`)
grnb2$`attributes.Accepts Credit Cards` <- gsub("NULL","",grnb2$`attributes.Accepts Credit Cards`)

#convert alcohol column to numeric

grnb2$attributes.Alcohol <- gsub("full_bar",2,grnb2$attributes.Alcohol)
grnb2$attributes.Alcohol <- gsub("none",0,grnb2$attributes.Alcohol)
grnb2$attributes.Alcohol <- gsub("beer_and_wine",1,grnb2$attributes.Alcohol)

#convert attire column to numeric

grnb2$attributes.Attire <- gsub("casual",0,grnb2$attributes.Attire)
grnb2$attributes.Attire <- gsub("dressy",1,grnb2$attributes.Attire)
grnb2$attributes.Attire <- gsub("formal",2,grnb2$attributes.Attire)

#convert noise level attributes to numeric

grnb2$`attributes.Noise Level` <- gsub("quiet",0,grnb2$`attributes.Noise Level`)
grnb2$`attributes.Noise Level` <- gsub("average",1,grnb2$`attributes.Noise Level`)
grnb2$`attributes.Noise Level` <- gsub("loud",2,grnb2$`attributes.Noise Level`)
grnb2$`attributes.Noise Level` <- gsub("very_loud",3,grnb2$`attributes.Noise Level`)

#convert Wi-Fi attributes to numeric

grnb2$`attributes.Wi-Fi` <- gsub("no",0,grnb2$`attributes.Wi-Fi`)
grnb2$`attributes.Wi-Fi` <- gsub("paid",1,grnb2$`attributes.Wi-Fi`)
grnb2$`attributes.Wi-Fi` <- gsub("free",2,grnb2$`attributes.Wi-Fi`)

#remove duplicate columns

grnb2 <- select(grnb2,-stars)


grnbcomp2 <- na.omit(grnb2)


for (i in 10:50) {
  
  grnbcomp2[,i] <- as.numeric(grnbcomp2[,i])
  
}

grnbcomp2 <- na.omit(grnbcomp2)


##split  set

gsplit2 <- createDataPartition(y=grnbcomp2$avgstars,p=.4,list=FALSE)
d21<-grnbcomp2[gsplit2,]
d22<-grnbcomp2[-gsplit2,]

#create validation and test set 

gsplit22 <- createDataPartition(y=d22$avgstars,p=.33,list=FALSE)
d2v<-d22[gsplit22,]
d2t<-d22[-gsplit22,]


#remove attributes with a correlation of greater than 0.9

d2mat <- d21[,c(4,6,8,10:50)]
d2cormat <- cor(d2mat)
d2cormatf <- findCorrelation(d2cormat, cutoff = .90, verbose = TRUE)

d21uncor <- d21[,-c(48,49)]

#conduct linear regression to identify influential attributes

d2lmset <- d21[,c(4,6,8,10:46)]
d2lm <- lm(data=d2lmset,avgstars~.)
d2lmvars <- tidy(d2lm)
d2lmkeyvars <- filter(d2lmvars,p.value<=.05)

#build another model with only the attributes that have low p-values 

set2a <- d2lmkeyvars$term
set22b <- gsub("`","",set2a)
set22b <- set22b[-1]
d1lmset2 <- cbind(d21$avgstars,d21[,set22b])
d1lmset2 <- rename(d1lmset2,avgstars = `d21$avgstars`)

d1lm2 <- lm(data=d1lmset2,avgstars~.)

#test out two models against a validation set

mod1 <- train(avgstars~.,method="lm",data=d1lmset1)
mod2 <- train(avgstars~.,method="lm",data=d1lmset2)

pmod1 <- predict(mod1,newdata=d1v)
pmod2 <- predict(mod2,newdata=d2v)

#evaluate factors on 

d1lmsetv <- d1v[,c(4,6,8,10:33,35,37)]
d2lmsetv <- d2v[,c(4,6,8,10:46)]

p1lm <- lm(data=d1lmsetv,avgstars~.)
p2lm <- lm(data=d2lmsetv,avgstars~.)


