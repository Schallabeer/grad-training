get_data<-function(username,n_obs){
  user_tweets<- rtweet::get_timeline(username,n = n_obs)%>%
    as.tibble()%>%
    select(
      created=created_at,
      status=text,
      coords_coords
    )%>%
    mutate(
      longitude = map_dbl(coords_coords,1),
      latitude = map_dbl(coords_coords,2)
    )%>%
    select(-coords_coords)
}

tokenise<-function(clean_data){
  tokens<-clean_data%>%
    unnest_tokens(word,status)%>%
    anti_join(stop_words)
}

P_N<-function(tokens){
  P_N<-tk%>%
    inner_join(get_sentiments("bing"))%>%
    count(sentiment,sort=TRUE)%>%
    mutate(prop=n/sum(n))
}

sentiments<-function(tokens){
  
}

term_matrix<-function(tokens){
  
}

