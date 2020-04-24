
#
#
# Install Packages and load data
#
#
#

#install.packages('rtweet')
#install.packages('igraph')
#install.packages('tidytext')

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


#### For whatever time period you are interested in repeat this for consecutive days within the period 
since <- '2019-08-20'
until <- '2019-08-21'

# Get Tweets 

#Brexit Tweets 

brexit.tweets <- search_tweets(q = "#Brexit", n=20000, lang = "en", since = since, until = until, 
                               retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)


setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")
write_as_csv(brexit.tweets, 'brexit_tweets_0821822.csv')

####

### Distress Tweets Keywords 

distress.tweets.have <- search_tweets(q = "\"I have depression\" OR 
                                      \"I have anxiety\"", n=10000, lang = "en", since = since, until = until, 
                                      retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

distress.tweets.am <- search_tweets(q = "\"I am depressed\" OR 
                                    \"I am anxious\"", n=10000, lang = "en", since = since, until = until, 
                                    retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

distress.tweets.im <- search_tweets(q = "\"I\'m depressed\" OR 
                                    \"I\'m anxious\"", n=10000, lang = "en", since = since, until = until, 
                                    retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.diagnosed <- search_tweets(q = "\"diagnosed with depression\"", n=10000, lang = "en", since = since, until = until, 
                                             retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

anxiety.tweets.diagnosed <- search_tweets(q = "\"diagnosed with anxiety\"", n=10000, lang = "en", since = since, until = until, 
                                          retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.related <- search_tweets(q = "\"I feel alone\" OR \"I feel numb\" OR \"I have no friends\"
                                           OR \"nobody to talk to\"", n=10000, lang = "en", since = since, until = until, 
                                           retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.related3 <- search_tweets(q = "\"hurt myself\" OR \"cut myself\"", n=10000, lang = "en", since = since, until = until, 
                                            retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.related2 <- search_tweets(q = "\"I dont want to be here\" OR \"I dont want to live\"
                                            OR \"I want to end my life\" OR \"kill myself\" OR \"nobody will miss me\" OR 
                                            \"nobody will care if Im gone\"", n=10000, lang = "en", since = since, until = until, 
                                            retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.related4 <- search_tweets(q = "\"I cant leave my house\" OR \"I cant get out of bed\"", n=10000, lang = "en", since = since, until = until, 
                                            retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.negative.self <- search_tweets(q = "\"I hate myself\"", n=10000, lang = "en", since = since, until = until, 
                                                 retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.help <- search_tweets(q = "\"please help me\" OR \"I need help\"", n=10000, lang = "en", since = since, until = until, 
                                        retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.feel <- search_tweets(q = "\"I feel sad\" OR \"I dont want to feel like this\"", n=10000, lang = "en", since = since, until = until, 
                                        retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.abuse <- search_tweets(q = "\"Why wont be stop\" OR \"why won\'t he stop\" OR \"I am being abused\" OR \"abusing me\"", n=10000, lang = "en", since = since, until = until, 
                                         retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.self.worth <- search_tweets(q = "\"Im not good enough\" OR \"I\'m not good enough\"", n=10000, lang = "en", since = since, until = until, 
                                              retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

depression.tweets.care <- search_tweets(q = "\"nobody likes me\" OR \"nobody cares about me\"", n=10000, lang = "en", since = since, until = until, 
                                        retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)


# Save data
setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

library(dplyr)
library(tidyr)

psych.distress.master <- bind_rows(list(depression.tweets.care, depression.tweets.feel, depression.tweets.help, depression.tweets.negative.self, 
                                        depression.tweets.related, depression.tweets.related2, depression.tweets.related3, depression.tweets.related4, depression.tweets.self.worth, 
                                        distress.tweets.have, distress.tweets.am, distress.tweets.im))

write_as_csv(psych.distress.master, 'Psych_distress_07230729.csv')

distress.brexit <- psych.distress.master %>% 
  filter(grepl('Brexit | brexit | Europe', text))

brexit.distress <- brexit.tweets %>% 
  filter(grepl('depressed | anxious | anxiety | depression', text))

# All Tweets for User 

users.depressed <- as.character(as.vector(unique(distress.tweets.have[1:25,1])))

test.tweets <- get_timeline("3283098062", n = 100)




# Search for Control users 

world$trend[1]

users.control3 <- search_users(q = "is", n=1000)
users.control2 <- search_users(q = "the", n=1000)
users.control <- search_users(q = "and", n=1000)

users.control <- search_tweets(q = "and", n=10000, lang = "en", 
                               retryonratelimit = TRUE, type = 'mixed', include_rts = FALSE)

users.control.id <- unique(users.control$user_id)

users.control.id.cl <- users.control.id[6001:12000]

