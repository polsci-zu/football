badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)
source("model/00_function.R")

###########

load("model/data.Rdata")

# Flo this season 


season17 <- llply(1:34,
                  function(i) read_json(
                    paste0("https://www.openligadb.de/api/getmatchdata/bl1/2017/",
                           i)))

fitlist <- list()
predictionlist <- list()
sampslist <- list()

for (m in 11:15){
  
  data = model_preparation(team_ranking,
                           seasons,
                           whichseasons = c("season16","season17"),
                           maxmatchday = c(m))
  names(data)
  #--------------------------------------------------------------------------
  
  fit <- stan("model/model_pois_0.04.stan", 
              data=data, 
              chains=4, 
              #warmup = ,
              iter = 4000,
              #cores = 4,              
              refresh = 100)
  
  
  #fitlist[[m]] <- fit
  
  empty <- extract_info_pre(season17[[m]])
  samps <- as.data.frame(fit) %>% {.[(nrow(.)/2+1):nrow(.),]} # only lower half of df
  
  # Prediction 
  
  predictionlist[[m]] <-  simulate_matchday(empty,samps)
  sampslist [[m]] <- samps
}

## 

# Merge predictions und results: 

all_predictions <- 
  predictionlist %>% 
  flatten_dfr() %>%
  rename(predicted1 = goals1,
         predicted2 = goals2) %>% 
  mutate(predDiff1 = predicted1 - predicted2,
         predDiff2 = predicted2 - predicted1)

names(all_predictions)


merged <- 
  seasons %>% 
  full_join(all_predictions,
            by = "MatchID") %>% 
  na.omit()






# Leverkusen gewinnt zu 42% auswärts in münchen (hää)
a = merged %>% filter(MatchID == 45437)
a %>% group_by(predDiff1 < 0) %>% summarise(sum(n))








# Rest
x = sampslist[[1]] %>% select(contains("skill")) %>% head(50) %>% summarise_all(funs(mean(.)))
names(x) = team_ranking
x



