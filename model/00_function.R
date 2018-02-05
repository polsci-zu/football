# Functions ------------------------------------------------------------------

footballr <- function(sart,end){
  seasons <- start$season:end$season
  matchdays <- list()
  for(i in seq_along(seasons)){
    matchdays[[i]] <- 1:34
  }
  names(matchdays) <- paste0("season",seasons)
  matchdays[[1]] <- start$matchday:34
  matchdays[[length(matchdays)]] <- 1:end$matchday
  
  dwld <- list()
  #i <- 1
  #ii <- 1
  for(i in seq_along(seasons)){
    dwld[[i]] <- list()
    for(ii in matchdays[[i]]){
      path <- sprintf("https://www.openligadb.de/api/getmatchdata/bl1/20%2.0f/%.0f",
                      seasons[i],ii)
      dwld[[i]][[ii]] <- read_json(path)
    }
  }
  #dwld[[1]][[2]]
  names(dwld) <- paste0("season",seasons)
  dwld
}


extract_info <- function(x){
  laply(x,function(x){ 
    c(
      "MatchDateTime" = x[["MatchDateTime"]],
      "MatchID" = x[["MatchID"]],
      "LeagueName"= x[["LeagueName"]],
      "MatchDay" = x[["Group"]][["GroupName"]],
      "Team1" = x[["Team1"]][["TeamName"]],
      "Team2" = x[["Team2"]][["TeamName"]],
      "Goals1" = x[["MatchResults"]][[2]][["PointsTeam1"]],
      "Goals2" = x[["MatchResults"]][[2]][["PointsTeam2"]]
    )
  }) %>% as_tibble()
}

extractor <- function(X) purrr::map(X,extract_info) %>% bind_rows()

compute_table <- function(season){
  season <- within(season,{
    Goals1 %<>% as.numeric()
    Goals2 %<>% as.numeric()
    Points2 <- (Goals1 < Goals2)*3 + (Goals1 == Goals2)*1
    Points1 <- (Goals1 > Goals2)*3 + (Goals1 == Goals2)*1
    MatchDay %<>% str_extract("\\d{1,2}") %>% as.numeric()
    diff1 <- Goals1-Goals2
    diff2 <- Goals2-Goals1
  })
  
  long <- cbind(
    season %>% select(Team1,Team2) %>% gather(),
    season %>% select(Points1,Points2) %>% gather()
  )[,c(2,4)]
  
  names(long) <- c("Team","Points")
  long %>% group_by(Team) %>% summarise(Points = sum(Points)) %>%
    arrange(desc(Points))
}


#End Functions------------------------------------------------------------------
