library(tidyverse)
library(plotly)

dt<-tibble(
  id=rep(paste("account",1:500,sep = "-"),each=10),
  segment=rep(LETTERS[1:5],each=1000),
  cf=runif(5000,min=-15,max=85)
)%>%
  group_by(id)%>%
  mutate(
    balance=runif(1,5000,6000)-cumsum(cf),
    time_on_book=1:n(),
    arrears=pmin(cumsum(cf<0),3)
  )

dt%>%rio::export("Data/tranition_data.csv")

rr<-dt%>%
  group_by(id)%>%
  mutate(lead_arrears=lead(arrears,1))%>%
  # filter(!is.na(lead_arrears))%>%
  group_by(segment,arrears,lead_arrears)%>%
  summarise(
    total=sum(balance),
    n=n()
  )%>%
  mutate(
    prop=total/sum(total),
    prop.n=n/sum(n)
  )


tr<-rr%>%
  select(segment,arrears,lead_arrears,prop)%>%
  spread(lead_arrears,prop)


