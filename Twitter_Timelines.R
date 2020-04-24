# Gather History Twitter


setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

library(dplyr)
library(tidyr)

distress1 <- read.csv("Psych_distress_07010714.csv")
distress2 <- read.csv("Psych_distress_07140723.csv")
distress3 <- read.csv("Psych_distress_07230729.csv")

distress.all <- bind_rows(list(distress1, distress2, distress3))

distress.all <- mutate(distress.all, Data_Source = "Twitter API")

# Create Distress Groups

toMatch <- c("diagnosed with depression", "diagnosed with anxiety", "I have depression", "I have anxiety")

distress.cl <- distress.all %>% 
  filter(grepl(paste(toMatch,collapse="|"), text))

distress.text <- distress.cl %>% 
  select(user_id, status_id, created_at, screen_name, text)

write.csv(distress.text, file = "distress_tweet_text_raw.csv")

distress.test <- read.csv("distress_tweet_text_raw_200.csv")

table(distress.test$Label)

toRemove <- c("anxiety about", "anxious to", "haha", "lol", "LOL")

distress.cl2 <- distress.test %>% 
  filter(!grepl(paste(toRemove,collapse="|"), text))

table(distress.cl2$Label)

write.csv(distress.cl2, file = "distress_tweet_text_raw_labs.csv")

distress.test2 <- read.csv("distress_tweet_text_raw_labsv3.csv")

table(distress.test2$Label)

distress.test.cl <- distress.test2 %>% 
  filter(Label == 1) %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x")

distress.users <- unique(distress.test.cl$ID)

## Collect history for users 
## Open Twitter
library(rtweet)


api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""



create_token(
  app = "BCU_Data_Viz",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret)

test.tweets <- get_timeline("244738515", n = 100, retryonratelimit = TRUE)

Users <- distress.users
distress.cur <- read.csv("tweets_history_distress.csv")

# Keep find the IDs which have already had history pulled 

# Keep Those 

distress.keep <- distress.cur %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x") %>% 
  filter(ID %in% Users)

# Get list of new ids
distress.users.keep <- unique(distress.keep$ID)

distress.new.keep <- Users[- which(Users %in% distress.users.keep)]

library(glue)

get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  tweets.all <<- tweets
}




Users <- distress.new.keep

get_timeline_unlimited(Users, 3000)

tweets.all <- mutate(tweets.all, Distress = "Yes")

tweets.all.cl <- tweets.all %>% 
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, is_retweet, is_quote, 
         favorite_count, retweet_count, lang, place_name, place_type, country, country_code,  
         location, url, protected, followers_count, friends_count, statuses_count, 
         favourites_count, account_created_at)

write_as_csv(tweets.all, 'tweets_history_distress2.csv')

# Get list of those still needed round 2
distress.users.new <- unique(tweets.all$user_id)
distress.new.keep2 <- Users[- which(Users %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep2, 3000)

tweets.all.cl <- tweets.all %>% 
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, is_retweet, is_quote, 
         favorite_count, retweet_count, lang, place_name, place_type, country, country_code,  
         location, url, protected, followers_count, friends_count, statuses_count, 
         favourites_count, account_created_at)

write_as_csv(tweets.all, 'tweets_history_distress3.csv')

# Round 3 

distress.users.new <- unique(tweets.all$user_id)
distress.new.keep3 <- distress.new.keep2[- which(distress.new.keep2 %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep3, 3000)

distress.users.new <- unique(tweets.all$user_id)


tweets.all.cl <- tweets.all %>% 
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, is_retweet, is_quote, 
         favorite_count, retweet_count, lang, place_name, place_type, country, country_code,  
         location, url, protected, followers_count, friends_count, statuses_count, 
         favourites_count, account_created_at)

write_as_csv(tweets.all, 'tweets_history_distress4.csv')

# Round 4
distress.new.keep4 <- distress.new.keep3[- which(distress.new.keep3 %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep4, 3000)

distress.users.new <- unique(tweets.all$user_id)

tweets.all.cl <- tweets.all %>% 
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, is_retweet, is_quote, 
         favorite_count, retweet_count, lang, place_name, place_type, country, country_code,  
         location, url, protected, followers_count, friends_count, statuses_count, 
         favourites_count, account_created_at)

write_as_csv(tweets.all, 'tweets_history_distress5.csv')

# Round 5 
distress.new.keep5 <- distress.new.keep4[- which(distress.new.keep4 %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep5, 3000)

distress.users.new <- unique(tweets.all$user_id)

write_as_csv(tweets.all, 'tweets_history_distress6.csv')

# Round 6
distress.new.keep6 <- distress.new.keep5[- which(distress.new.keep5 %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep6, 3000)

distress.users.new <- unique(tweets.all$user_id)

write_as_csv(tweets.all, 'tweets_history_distress7.csv')

# Round 7 
distress.new.keep7 <- distress.new.keep6[- which(distress.new.keep6 %in% distress.users.new)]

get_timeline_unlimited(distress.new.keep7, 3000)

distress.users.new <- unique(tweets.all$user_id)
write_as_csv(tweets.all, 'tweets_history_distress8.csv')


# Round 8 
distress.new.keep8 <- distress.new.keep7[- which(distress.new.keep7 %in% distress.users.new)]





# Control Group

get_timeline_unlimited(users.control.id.cl, 3000)

users.control.id.unique <- unique(tweets.all$user_id)
write_as_csv(tweets.all, 'tweets_history_control2.csv')

tweets.all.peek <- tail(tweets.all, 100)
