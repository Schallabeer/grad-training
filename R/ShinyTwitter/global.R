library(httr)
library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(leaflet)
library(plotly)

# set_config(
#   use_proxy(
#     url="proxy.za.deloitte.com", 
#     port=8080, username=rstudioapi::askForPassword(prompt = "username"),
#     password=rstudioapi::askForPassword(prompt = "password")
#   )
# )
twitter_token <- readRDS("twitter_token.rds")
