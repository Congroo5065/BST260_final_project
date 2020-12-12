install.packages('caret')
library(tidyverse)
library(caret)
library(pROC)
setwd('/Users/jiesun/Documents/nBox/TO TRANSFER/260 Project')
setwd('C:/Users/ephsunj/Desktop/nBox/TO TRANSFER/260 Project')

d1 = readRDS('data/Data_info_00to20.RDS')
d2 = readRDS('data/Data_people_00to20.RDS')

d3 <- d2 %>% dplyr::select(-imdb_ID) %>% bind_cols(d1)


#0. remove rows of NAs in cast, director, worldwide boxoffice, rating_all, genres
d3 <- d3 %>% filter(!is.na(cast) & !is.na(director) & !is.na(cumulative_worldwide_gross) & !is.na(rating_all) & !is.na(genres))

#1. remove NA in rating. 
d3$award_win = replace_na(d3$award_win,0)
d3$award_nom = replace_na(d3$award_nom,0)
d3$winner_oscar = replace_na(d3$winner_oscar,0)
d3$nominee_oscar = replace_na(d3$nominee_oscar,0)


#2. create variables: 
# 1) win_nom_tf: if the movie has earned any awards or nominations (yes = 1, no = 0)
# 2) outcome: y variable. 1 = success (rating > 7, or won or nominated by an award), 0 = not success
d3.1 = d3 %>% 
  rowwise %>%
  mutate(win_nom = sum(award_win, award_nom, winner_oscar, nominee_oscar)) %>%
  mutate(win_nom_tf = (win_nom > 0)) %>%
  mutate(good_rate = rating_all > 7) %>%
  mutate(outcome = as.numeric(good_rate | win_nom_tf))


#3. process x variables

#x1(categorical): the primary genre of each movie (the 1st genre listed). has NA

#collection of genres: categorical variable 
genr_collect = na.omit(unique(unlist(d3.1$genres))) 

x1 = c()
for (i in 1:nrow(d3.1)){
  x1[i]<-d3.1$genres[[i]][1]
}

x1 <- data.frame(x1)
d3.1 <- bind_cols(d3.1,x1)


#=========================
#x2: director's score (ignore co-director) (numeric) avg by the number of movies of the same director in database

#accumulated score for director (Basic) @could revise the criteria further: win oscar can +10
# if movie has an award or nomination: +1
# if movie rating > 7: +1

dir_score = c() #score of director by movie (0,1,2)
dir_lst = c() #main director by movie
for (i in 1:nrow(d3.1)){
  dir_score[i] = as.numeric(d3.1$win_nom_tf[i]) + as.numeric(d3.1$good_rate[i])
  dir_lst[i] = d3.1$director[[i]][1]
}

d3.2 = cbind(d3.1,dir_score,dir_lst)
#remove irrelevant columns
d3.3 <- d3.2 %>% group_by(dir_lst) %>% mutate(x2 = mean(dir_score)) %>%
  dplyr::select(year, imdb_ID, title, rating_all,vote_all,cumulative_worldwide_gross,dir_score,dir_lst,cast,outcome,x1,x2)

# unique director, score @
dscore = cbind(dir_lst,d3.3$x2)
dscore <- dscore[!duplicated(dscore[,'dir_lst']),]

#=========================
#x3: cast score (numeric): weighted sum from actors' performance in other movies

#1. separate database for actors: actor_score(unique actor, adjusted score)

hist(d3.3$vote_all,bin = 20) #looks like a power distribution
quantile(d3.3$vote_all) #very broad range

d3.4 <- d3.3 %>% arrange(desc(vote_all)) #arrange the dataset by descending order of votes

k <- rep(1:NROW(d3.4),times = sapply(d3.4$cast, length))
tmp <- d3.4[k, c("imdb_ID","title", "rating_all")] %>%
  group_by(title)%>%
  mutate(z = 1:n())%>%
  ungroup()%>%
  mutate(w = ifelse(z>2, 0.1, 1),
         actor = unlist(d3.4$cast))

actor_score <- tmp %>% rowwise() %>%
  mutate(s = rating_all * w) %>%
  ungroup() %>% 
  group_by(actor) %>%
  summarise(as = sum(s)) %>% 
  arrange(desc(as)) # Actor_adjusted score

#2. x3: each film, cast score
# weighted sum of all actor scores based on their position in the movie
# 2 main actor: full score. Supporting: 0.1

tmp2 = tmp %>% left_join(actor_score,by = 'actor') %>%
  rowwise()%>%
  mutate(q = as * w) #the score this actor is contributing to the cast of this movie
tmp3 <- tmp2 %>% 
  group_by(title) %>%
  summarise(cast_score = sum(q)) %>%
  ungroup() #ID, cast_score

d3.5 <- d3.4 %>% left_join(tmp3,by = 'title') #outcome, x1,x2, x3(cast_score)






#=========================
#x3.2: cast score (numeric): weighted sum from actors' performance in other movies
# we change the allocation of score: first 4 main actors get full, the rest get 20%

#1. separate database for actors: actor_score(unique actor, adjusted score)

hist(d3.3$vote_all) #looks like a power distribution
quantile(d3.3$vote_all) #very broad range

d3.3 %>% ggplot(aes(vote_all))+
  geom_histogram()+
  #scale_x_log10()+
  scale_y_log10() #check binwidth@

d3.4 <- d3.3 %>% arrange(desc(cumulative_worldwide_gross))#arrange the dataset by descending order of worldwise box-office


k <- rep(1:NROW(d3.4),times = sapply(d3.4$cast, length))
tmp <- d3.4[k, c("imdb_ID","title", "rating_all")] %>%
  group_by(title)%>%
  mutate(z = 1:n())%>%
  ungroup()%>%
  mutate(w = ifelse(z>4, 0.2, 1),
         actor = unlist(d3.4$cast))

actor_score2 <- tmp %>% rowwise() %>%
  mutate(s = rating_all * w) %>%
  ungroup() %>% 
  group_by(actor) %>%
  summarise(as = sum(s)) %>% 
  arrange(desc(as)) # Actor_adjusted score

#2. x3: each film, cast score
# weighted sum of all actor scores based on their position in the movie
# 2 main actor: full score. Supporting: 0.2

tmp2 = tmp %>% left_join(actor_score,by = 'actor') %>%
  rowwise()%>%
  mutate(q = as * w) #the score this actor is contributing to the cast of this movie
tmp3 <- tmp2 %>% 
  group_by(title) %>%
  summarise(cast_score2 = sum(q)) %>%
  ungroup() #ID, cast_score2

d3.6 <- d3.5 %>% left_join(tmp3,by = 'title')#outcome, x1,x2, x3.2(cast_score but version 2)
#=========================
#outcome2: y2. only box-office. If it's at the top 10% of the year, then it's a success(1)
#controlling P(y2 = 1) to be 0.1
d3.6 <- d3.6 %>% group_by(year) %>%
  mutate(top10 = quantile(cumulative_worldwide_gross,0.9))%>%
  ungroup() %>%
  rowwise() %>%
  mutate(outcome2 = as.numeric(cumulative_worldwide_gross > top10)) #outcome, x1,x2, x3.2,outcome2

#I moved this up, so d3.6 becomes d3.1

#d3.6 is so far the most complete df: outcome, x1,x2,x3(cast_score), x3.2(cast_score2),outcome2
save(d3.3,d3.4,d3.5,d3.6,
     actor_score,actor_score2,dscore,
     genr_collect,file='output/0812_2Models.Rdata')


#4. Partition the test/training data
set.seed(100)
n_test <- round(nrow(d3.6)/10) #1300 tests, 10%
test_indices <- sample(1: nrow(d3.6),n_test,replace = FALSE)




#5. Modelling: 

#take only the necessary variables
d3.7 <- d3.6 %>% dplyr::select(outcome,outcome2,x1,x2,cast_score,cast_score2)
d_test <- d3.7[test_indices,]
d_train <- d3.7[-test_indices,]

#Check for multicollinearity between director rating & cast rating
cor(d3.7$x2,d3.7$cast_score) #0.25
#how many success do we have in this dataset: 
sum(d3.7$outcome > 0)/nrow(d3.7) #0.7040769


## Mod1 - Linear

#outcome ~ genre + dir score + cast score (d3.5)

lm_mod1 = glm(outcome ~ x1 + x2 + cast_score,data = d_train,family = 'binomial')
lm1_summary = summary(lm_mod1)
lm1_coef = lm1_summary$coefficients[,1] #estimates
#result: success needs: drama, good director. cast score doesn't seem very useful

#see how well it performs on test

d_test_pred = predict(lm_mod1,d_test,type = 'response')
plot(density(d_test_pred[!is.na(d_test_pred)])) #plot the density of the probability

cm1 <-confusionMatrix(data = as.factor(as.numeric(d_test_pred>0.5)),reference = as.factor(d_test$outcome))
cm1
#result: accurarcy 0.923. sensitivity 0.83, specificity 0.95

#plot ROC
roc_lm1 <- roc(d_test$outcome,d_test_pred)
roc_lm1
plot(roc_lm1) #looks pretty impressive. 


## Mod1.2: how sensitive is the model if we change the cast score allocation
#outcome ~ genre + dir score + cast score2
lm_mod1.2 = glm(outcome ~ x1 + x2 + cast_score2,data = d_train,family = 'binomial')
summary(lm_mod1.2)
d_test_pred1.2 = predict(lm_mod1.2,d_test,type = 'response')
plot(density(d_test_pred1.2[!is.na(d_test_pred1.2)])) #plot the density of the probability
cm1.2 <-confusionMatrix(data = as.factor(as.numeric(d_test_pred1.2>0.5)),reference = as.factor(d_test$outcome))
cm1.2
#result: accurarcy 0.9238. sensitivity 0.83, specificity 0.96
#Not much difference from lm1! So cast_score classification doesn't really matter in this case

#plot ROC
roc_lm1.2 <- roc(d_test$outcome,d_test_pred1.2)
roc_lm1.2
plot(roc_lm1.2) #looks pretty impressive. 
auc(roc_lm1.2) #same conclusion as above: lm1 and lm1.2 not much difference 

## Mod2
#outcome2 ~ genre + dir score + cast score2
lm_mod2 = glm(outcome2 ~ x1 + x2 + cast_score2,data = d_train, family = 'binomial')
summary(lm_mod2)
#result: more genre (animation, biography, comedy, crime, drama), dir, cast

d_test_pred2 = predict(lm_mod2,d_test,type = 'response')
plot(density(d_test_pred2[!is.na(d_test_pred2)])) #plot the density of the probability

cm2 <- confusionMatrix(data = as.factor(as.numeric(d_test_pred2>0.5)),reference = as.factor(d_test$outcome2))
cm2
#result: accurarcy 0.9176. sensitivity 0.97, specificity 0.41

#plot ROC
roc_lm2 <- roc(d_test$outcome2,d_test_pred2)
roc_lm2
plot(roc_lm2) #looks pretty impressive. 


## Mod2.2
#outcome2 ~ genre + dir score + cast score
lm_mod2.2 = glm(outcome2 ~ x1 + x2 + cast_score,data = d_train, family = 'binomial')
summary(lm_mod2.2)
#result: more genre (animation, biography, comedy, crime, drama), dir, cast

d_test_pred2.2 = predict(lm_mod2.2,d_test,type = 'response')
plot(density(d_test_pred2.2[!is.na(d_test_pred2.2)])) #plot the density of the probability

cm2.2 <- confusionMatrix(data = as.factor(as.numeric(d_test_pred2.2>0.5)),reference = as.factor(d_test$outcome2))
cm2.2
#result: accurarcy 0.9138. sensitivity 0.98, specificity 0.37

#plot ROC
roc_lm2.2 <- roc(d_test$outcome2,d_test_pred2.2)
roc_lm2.2
plot(roc_lm2.2) 

#Compare Mod1 and Mod2:
compare_cm = 
  data.frame(lm1 = c(cm1$byClass[1:2]),
             lm1.2 = c(cm1.2$byClass[1:2]),
             lm2 = c(cm2$byClass[1:2]),
             lm2.2 = c(cm2.2$byClass[1:2]))

compare_cm
#result: Mod2 has much lower specificity(if a movie is actually a failure, mod2 is less likely to pick it up) than lm1,
# higher sensitivity (if the movie is gonna be a success, mod2 is more like to pick it up). 
#overall it seems that mod1 is more balanced

plot(roc_lm1,col=4)
plot(roc_lm1.2,col=5, add = TRUE)
plot(roc_lm2,col=6, add = TRUE)
plot(roc_lm2.2,col='orange', add = TRUE)
legend(0.2,0.8,legend = c('lm1','lm1.2','lm2','lm2.2'),col = c(4,5,6,'orange'),lty = 1)
auc(roc_lm1)
auc(roc_lm1.2)
auc(roc_lm2)
auc(roc_lm2.2)
#result:looks like lm1 is better, with higher auc.

#More thoughts: the reason could be the way we defined the y: we forced it to be 10% based on year
# but year was not taken into account in the explanatory variable. To fix it, we can add back the year



