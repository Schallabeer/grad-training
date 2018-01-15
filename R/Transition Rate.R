
# Data generation

PD<-dlnorm(seq(0.05,3,by = 0.05), meanlog = 0, sdlog = 1, log = FALSE)/6


generate_account<-function(id,PD){
  
  id<-paste("account",id,sep = "-")
  segment<-sample(LETTERS[1:3],1)
  start_balance<-runif(1,5000,6000)
  interest_rate<-runif(1,0.05,0.20)/12
  n<-60
  
  pmt<-start_balance*(interest_rate*(1+interest_rate)^n)/((1+interest_rate)^n-1)
  
  for(i in 1:n){
    if(i==1){
      balance_contractual<-start_balance*(1+interest_rate)-pmt
      
      made_payment<-rbinom(1,1,1-PD[i])
      balance_actual<-start_balance*(1+interest_rate)-pmt*made_payment[i]
      
    }else{
      balance_contractual<-c(balance_contractual,balance_contractual[i-1]*(1+interest_rate)-pmt)
      
      made_payment<-c(made_payment,rbinom(1,1,1-PD[i]))
      balance_actual<-c(balance_actual,balance_actual[i-1]*(1+interest_rate)-pmt*made_payment[i])
    }
  }
  
  
  data<-data.frame(
    id=id,
    segment=segment,
    start_balance=start_balance,
    interest_rate=interest_rate,
    n=n,
    age=1:n,
    pmt=pmt,
    balance_contractual=balance_contractual,
    made_payment=made_payment,
    balance_actual=balance_actual,
    arrears=balance_contractual-balance_actual,
    cd_bucket=-ceiling((balance_contractual-balance_actual)/pmt)
  )
}



account<-lapply(
  1:10000,
  generate_account,
  PD=PD
)

data<-dplyr::bind_rows(account)


# Export
data%>%rio::export("Data/tranition_data.csv")

library(tidyverse)
library(sparklyr)

spark_home_set("Spark/spark-2.2.1-bin-hadoop2.7")
sc<-spark_connect(master="local") # Create a connection to spark

data<-spark_read_csv(sc,"loans_data","Data/tranition_data.csv")

# Transition rates
rr<-data%>%
  mutate(
    arrears=pmin(cd_bucket,3)
  )%>%
  arrange(id,age)%>%
  group_by(id)%>%
  mutate(lead_arrears=lead(arrears,1))%>%
  filter(!is.na(lead_arrears))%>%
  group_by(segment,arrears,lead_arrears)%>%
  summarise(
    total=sum(balance_actual),
    n=n()
  )%>%
  mutate(
    prop=total/sum(total),
    prop.n=n/sum(n)
  )


tr<-rr%>%
  select(segment,arrears,lead_arrears,prop)%>%
  collect()%>%
  spread(lead_arrears,prop)


# Hazard Rate calculation
hazard<-data%>%
  arrange(id,age)%>%
  group_by(id)%>%
  mutate(lead_flag=lead(made_payment,1))%>%
  filter(!is.na(lead_flag) & made_payment==1)%>%
  group_by(age,made_payment,lead_flag)%>%
  summarise(
    total=sum(balance_actual),
    n=n()
  )%>%
  mutate(
    prop=total/sum(total),
    prop.n=n/sum(n)
  )%>%
  filter(lead_flag==0)%>%
  collect()


#Actual vs Expected
hazard%>%
  plotly::plot_ly()%>%
  plotly::add_markers(
    x=~age,
    y=~prop,
    name="Simulated"
  )%>%
  plotly::add_lines(
    x=1:60,
    y=~PD,
    name="Actual"
  )


# Exposure over time
data%>%
  group_by(age)%>%
  summarise(
    contractual_total=sum(balance_contractual),
    actual_total=sum(balance_actual)
  )%>%
  plotly::plot_ly(x=~age)%>%
  plotly::add_lines(y=~contractual_total,name="Contractual")%>%
  plotly::add_lines(y=~actual_total,name="Actual")
