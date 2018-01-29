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
  P_N<-tokens%>%
    inner_join(get_sentiments("bing"))%>%
    count(sentiment,sort=TRUE)%>%
    mutate(prop=n/sum(n))
}

sentiments<-function(tokens){
  sentiments<-tokens%>%
    inner_join(get_sentiments("nrc"))%>%
    count(sentiment,sort=TRUE)%>%
    filter(!sentiment %in% c("positive","negative"))
}

term_matrix<-function(tokens){
  term_matrix<-tokens%>%
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment,sort=TRUE)%>%
    spread(sentiment,n,fill = 0)%>%
    remove_rownames()%>%
    column_to_rownames(var = "word")
}

