# Recommender System Assignment by gaurav sachdeva

library(dplyr)
library(ggplot2)
library(recommenderlab)

setwd("C:/PGDDS/Recommender Systems assignment")
beer_rev_df<-read.csv("beer_data.csv")


View(beer_rev_df)

str(beer_rev_df) #beer_beeridid & review_overall are numeric, review_profilename(name of reviewer) is factor

summary(beer_rev_df) #review_profilename have missing values, reviews are in range 0-5

dim(beer_rev_df) #475984 no of user ratings & 3 columns

sum(is.na(beer_rev_df$review_profilename))# No NA

nrow(beer_rev_df[beer_rev_df$review_profilename=="", ])# 100 Blank Enrties

#Updating beer datset removing empty review_profilename
beer_rev_df<-beer_rev_df[!(beer_rev_df$review_profilename==""), ]


summary(beer_rev_df) 

dim(beer_rev_df) #475884 no of user ratings & 3 columns

#Removing duplicates where both beer & user are duplicated, this will ensure that no 2 reviews from single user to same beer are counted.
beer_rev_df<-distinct(beer_rev_df,beer_beerid,review_profilename,.keep_all = TRUE)

summary(beer_rev_df) 

dim(beer_rev_df)

#######################################################################################################
#1. Data Prepration
#1. Choose only those beers that have at least N number of reviews

beer_reviews_cnt <- beer_rev_df %>% group_by(beer_beerid) %>% summarise(beer_reviews_total=n())
dim(beer_reviews_cnt) #Total 40304 distinct beers

summary(beer_reviews_cnt) 

beer_reviews_cnt<-arrange(beer_reviews_cnt, desc(beer_reviews_total))

View(beer_reviews_cnt)


# Arranging beers with the overall mean rating
beer_rating_df<-group_by(beer_rev_df,beer_beerid)

beer_rating_df<- beer_rating_df%>% summarise(mean_rating=mean(review_overall))

beer_rating_df<-arrange(beer_rating_df, desc(mean_rating))

View(beer_rating_df)

# merging the dataset with reviews with that of rating

beer_rating_rev_df<-merge(beer_reviews_cnt,beer_rating_df,by.x="beer_beerid",by.y="beer_beerid")

View(beer_rating_rev_df)

# we wcan now eliminate beers with rating less than 3

beer_rating_rev_top_df<-beer_rating_rev_df%>% subset(mean_rating>=3)

dim(beer_rating_rev_top_df)

beer_rating_rev_top_df<-beer_rating_rev_df

beer_rating_rev_top_df_temp<-arrange(beer_rating_rev_top_df, desc(beer_reviews_total))

View(beer_rating_rev_top_df_temp)

#Beers have reviews count in range of 1 to 977, beer 2093 have highest number of reviews(977)

# Searching beer with single review

beer_rating_rev_top_df_temp %>% subset(beer_reviews_total==1) %>% dim() #So 18080 beers have only 1 review, lets group by total reviews & find count for each

#Dataframe with frequency of count of reviews as pivot

rev_freq<-beer_rating_rev_top_df_temp %>% group_by(beer_reviews_total) %>% summarise(no_of_cnt_freq=n())

rev_freq 

ggplot(rev_freq,aes(x=beer_reviews_total,y=no_of_cnt_freq)) + geom_point()

summary(rev_freq$no_of_cnt_freq)

# Mean is 94 but that does not make much sense

#From the plot ,We see that the beers having reviews less than 25 are most in the dataset

#we will eliminate such peripheral cases and hence we assume N=25

beer_rating_rev_final_df<-beer_rating_rev_top_df_temp%>% subset(beer_reviews_total>25)

dim(beer_rating_rev_final_df)

summary(beer_rating_rev_final_df) 

#Lets also filter beer dataset based on users who have reviewed minimum some quantity of beers

usr_rev_cnt<- beer_rev_df %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) 

usr_rev_cnt<-arrange(usr_rev_cnt, desc(total_user_reviews))

dim(usr_rev_cnt)

View(usr_rev_cnt)

#Checking on  full dataset of beers
avg_usr_ratings_total<-usr_rev_cnt %>% group_by(review_profilename) %>% summarise(average_rating=mean(total_user_reviews))

summary(avg_user_ratings_all$average_rating)

#So on average each user gives 21 reviews so we limit the data to users giving 21 or more reviews

usr_rev_cnt_subset<-subset(usr_rev_cnt,usr_rev_cnt$total_user_reviews>=21)

dim(user_reviews_count_subset)

#Now lets filter original dataset using above calculated datasets

final_beers<-merge(beer_rev_df,beer_rating_rev_final_df,by.x="beer_beerid",by.y="beer_beerid")
final_beers<-merge(final_beers,usr_rev_cnt_subset,by.x="review_profilename",by.y="review_profilename")

summary(final_beers) 

dim(final_beers)
View(final_beers)

#This beer subset have substantial reviews for both users & beers 

#2. Convert this data frame to a "realratingMatrix" before analyzing through collaborative filtering models
beers_rrmatrix <- as(final_beers[,c(1,2,3)], "realRatingMatrix")
class(beers_rrmatrix)


#Coerce the matrix to a dataframe
beers_df_final <- as(beers_rrmatrix, "data.frame")
str(beers_df_final)
summary(beers_df_final)


################################################################################################
#2. Data Exploration:

#1. Determine how similar the first ten users are with each other and visualise it

#How similar are the first ten users are with each other
usr_similar<- similarity(beers_rrmatrix[1:10,],method = "cosine",which = "users") 

#Similarity matrix
as.matrix(usr_similar)

#Visualise similarity matrix
image(as.matrix(usr_similar), main = "Similarity among 1st 10 Users")


#2. Compute and visualise the similarity between the first 10 beers
#How similar are the first ten beers are with each other
beers_similar <- similarity(beers_rrmatrix[,1:10],method = "cosine",which = "items")

#Similarity matrix
as.matrix(beers_similar)

#Visualise similarity matrix
image(as.matrix(beers_similar), main = "Similarity among 1st 10 Beers")

#3. What are the unique values of ratings?

beers_df_final %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% nrow() 
# 9 distinct ratings are there ranging from 1.0 to 5.0

beers_df_final  %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% View()
#Rating 4.0 & 4.5 have the most frequency, 1.0 & 1.5 have the least frequency.ppl write a review when like something

#4. Visualise the rating values and notice:

#(i) The average beer ratings
avg_beer_rating<-beers_df_final %>% group_by(item) %>% summarise(average_rating=mean(rating))

ggplot(avg_beer_rating,aes(x=average_rating)) + geom_histogram() + labs(x="Avg Rating", y="# of Beers")

summary(avg_beer_rating$average_rating)

#Average beer ratings(Mean)=3.777 & Median=3.833, almost normal, slightly skewed to the left

#(ii) The average user ratings

avg_user_rating<-beers_df_final %>% group_by(user) %>% summarise(average_rating=mean(rating))

ggplot(avg_user_rating,aes(x=average_rating)) + geom_histogram() + labs(x="Avg Rating", y="# of Users")

summary(avg_user_rating$average_rating)

#Average beer ratings(Mean)=3.862 & Median=3.888, slightly left skewed & uneven distribution

#(iii) The average number of ratings given to the beers

avg_beer_review<-final_beers %>% group_by(beer_beerid) %>% summarise(average_reviews=mean(beer_reviews_total))

ggplot(avg_beer_review,aes(x=average_reviews)) + geom_histogram() + labs(x="Avg Rating", y="No. of Beers")

summary(avg_beer_reviews$average_reviews)

#So on average each beer gets ~99 reviews from chosen subset

#(iv) The average number of ratings given by the users

avg_user_review<-final_beers %>% group_by(review_profilename) %>% summarise(average_reviews=mean(total_user_reviews))

ggplot(avg_user_review,aes(x=average_reviews)) + geom_histogram()

summary(avg_user_review$average_reviews)

#So on average each user gives ~101 reviews

#######################################################################################################
#3. Recommendation Models:

#1. Divide your data into training and testing datasets, Experiment with 'split' and 'cross-validation' evaluation schemes

#1. 1st Scheme with train/test(90/10) split without cross validation & goodRating as 4

scheme_split <- evaluationScheme(beers_rrmatrix, method = "split", train = .9,k = 1, given = -1, goodRating = 4)
scheme_split

#ii) 2nd Scheme using cross-validation without cross validation(5 folds) & goodRating as 4 
scheme_cross <- evaluationScheme(beers_rrmatrix, method = "cross-validation",k = 5, given = -1, goodRating = 4)
scheme_cross


#2. Building IBCF and UBCF models with below hyperparameters
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

#Evaluating algorithms & predicting next n beers
results_split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_split)

results_cross <- evaluate(scheme_cross, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_cross)

#3. Comparing the performance of the two models and suggest the one that should be deployed
#Drawing ROC curve

plot(results_split, annotate = 1:4, legend="topleft")
plot(results_cross, annotate = 1:4, legend="topleft")

#So UBCF seems to get better then IBCF especially with higher values of n

#4. Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
#Making Recomendations using UBCF

rec <- Recommender(beers_rrmatrix, method = "UBCF") 
rec

rec_cokes <- predict(rec, beers_rrmatrix['cokes'], n=5)

as(rec_cokes, "list")
#recommendation for cokes: "7971" "582"  "599"  "2041" "1346" 

rec_genog <- predict(rec, beers_rrmatrix['genog'], n=5)
as(rec_genog, "list")
#recommendation for geong: "57908" "19960" "1445"  "34420" "1160"  

rec_giblet <- predict(rec, beers_rrmatrix['giblet'], n=5)
as(rec_giblet, "list")
#recommendation for giblet:  "141"  "459"  "34"   "2671" "7971"


