#load libraries

library(jsonlite)
library(dplyr)
library(caret)
library(reshape2)
library(tm)
library(wordcloud)
library(slam)

#save paths to specific json files

bizfile <- "./R/capstone/yelp/yelp2/yelp_academic_dataset_business.json"
checkfile <- "./R/capstone/yelp/yelp2/yelp_academic_dataset_checkin.json"
reviewfile <- "./R/capstone/yelp/yelp2/yelp_academic_dataset_review.json"
tipfile <-"./R/capstone/yelp/yelp2/yelp_academic_dataset_tip.json"
userfile <- "./R/capstone/yelp/yelp2/yelp_academic_dataset_user.json"

#read json files into R

bizfile1 <- fromJSON(sprintf("[%s]", paste(readLines(bizfile), collapse=",")))
checkfile1 <- fromJSON(sprintf("[%s]", paste(readLines(checkfile), collapse=",")))
tipfile1 <- fromJSON(sprintf("[%s]", paste(readLines(tipfile), collapse=",")))

userfile1 <- stream_in(file(userfile),pagesize = 10000)
reviewfile1 <- stream_in(file(reviewfile),pagesize = 10000)


#saveRDS files

saveRDS(userfile1,"uf1.rds")
saveRDS(reviewfile1,"rf1.rds")
saveRDS(checkfile1,"cf1.rds")
saveRDS(bizfile1,"bf1.rds")

#flatten files

biz <- flatten(bizfile1)
checks <- flatten(checkfile1)
tips <- flatten(tipfile1)
users <- flatten(userfile1)
reviews <- flatten(reviewfile1)

#split users file to prepare for testing

split1 <- createDataPartition(y=users$votes.funny,p=.9,list=FALSE)
explore1<-users[split1,]
explore2<-users[-split1,]

#conduct exploratory plotting

plot1 <- ggplot(data=explore2,aes(x=fans,y=votes.funny),size=friends,shape=average_stars,color=review_count) + geom_point()

#what is the relationship between funny compliments and fans?

explore3 <- explore2[,c(6,15)]
explore3 <- na.omit(explore3)
plot3 <- ggplot(data=explore3,aes(x=fans,y=compliments.funny)) + geom_point()

#what is the relationship between funny votes and fans?

explore4 <- explore2[,c(6,10)]
explore4 <- na.omit(explore4)
plot4 <- ggplot(data=explore4,aes(x=fans,y=votes.funny)) + geom_point()

quantile(explore4$fans,.99) #99% of users in explore4 have less than 25 fans; let's 
#re-fit based on that

explore5 <- filter(explore4,fans<=25)
plot5 <- ggplot(data=explore5,aes(x=fans,y=votes.funny)) + geom_point() + stat_smooth()
model1 <- lm(data=explore5,fans~votes.funny)

#what types of votes are most predictive of fan counts?

##create sample set for this question

explore6 <- explore2[,c(6,10,11,12)]
explore6 <- na.omit(explore6)

##filter out outliers after applying quantile function to 99th percentile

explore6 <- filter(explore6,fans<=25)
explore6 <- filter(explore6,votes.useful<=872)
explore6 <- filter(explore6,votes.cool<=515)

explore6_melt <- melt(explore6,id="fans")
plot6 <- ggplot(data=explore6_melt,aes(x=value,y=fans,color=variable)) + geom_point() + stat_smooth()

model2 <- lm(data=explore6,fans~.)

#is there a relationship between the types of votes a business gets and its average rating?

##group, melt, and plot data

reviews_groupbiz <- reviews %>% group_by(business_id) %>% 
  summarise(avgstars=mean(stars),allvf=sum(votes.funny),allvu=sum(votes.useful),allc=sum(votes.cool))

reviews_gb_melt <- melt(reviews_groupbiz,id=c("business_id","avgstars"))
                        
plot7 <- ggplot(data=reviews_gb_melt,aes(x=value,y=avgstars,color=variable)) + geom_point() + stat_smooth()

model3 <- lm(data=reviews_groupbiz,avgstars~allvf+allvu+allc)
model4 <- lm(data=reviews_groupbiz,avgstars~allvf)
model5 <- lm(data=reviews_groupbiz,avgstars~allvu)
model6 <- lm(data=reviews_groupbiz,avgstars~allc)

#what are words that can predict whether a business will have good reviews? funny reviews? etc.

##split reviews set

rsplit1 <- createDataPartition(y=reviews$stars,p=.9,list=FALSE)
explore7<-reviews[rsplit1,]
explore8<-reviews[-rsplit1,]

##draw up word cloud

reviewtext <- Corpus(VectorSource(explore8$text))        

reviewtext <- tm_map(reviewtext, content_transformer(tolower))
reviewtext <- tm_map(reviewtext, stripWhitespace)
reviewtext <- tm_map(reviewtext, stemDocument)
reviewtext <- tm_map(reviewtext, removeWords, stopwords("english"))

###saveRDS on reviewtext

saveRDS(reviewtext,"reviewtext.rds")

##

plot8 <- wordcloud(reviewtext, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


##conduct frequency count

dtm <- DocumentTermMatrix(reviewtext)

saveRDS(dtm,"dtm.rds")

frequency <- col_sum(dtm)

#how long is the average useful post?

explore8 <- mutate(explore8,char=nchar(explore8$text))
plot9 <- ggplot(data=explore8,aes(x=nchar,y=votes.useful)) + geom_point() + stat_smooth()
model7 <- lm(data=explore8,votes.useful~char)

#is there a relationship between the number of reviews a user 
#has and the number of fans they have?

plot10 <- ggplot(data=users,aes(x=review_count,y=fans)) + geom_point() + stat_smooth()

#join biz & reviews, split on biz

rnb <- left_join(explore8,biz,by="business_id")

#join biz & grouped reviews

rgroup <- explore8 %>% group_by(business_id) %>% 
          summarise(avgstars=mean(stars),allvf=sum(votes.funny),
                    allvu=sum(votes.useful),allc=sum(votes.cool))

grnb <- left_join(rgroup,biz,by="business_id")

#filter out businesses that aren't restaurants

rnb <- filter(rnb,grepl("Restaurants",categories))
grnb <- filter(grnb,grepl("Restaurants",categories))



#what attributes best predict a well-rated restaurant on yelp with lots of fans?
#do attributes differ meaningfully based on location/region in the country?
#what attributes generate the most useful reviews?


reviews <- readRDS("reviews.rds")
biz <- readRDS("biz.rds")

