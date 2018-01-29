
library(tidyverse)
library(sparklyr)

spark_home_set("C:/Spark/spark-2.2.1-bin-hadoop2.7")
sc<-spark_connect(master="local") # Create a connection to spark

data<-spark_read_csv(sc,"loans_data","Data/tranition_data.csv",memory = FALSE)


# Exposure over time
data%>%
  group_by(age)%>%
  summarise(
    contractual_total=sum(balance_contractual),
    actual_total=sum(balance_actual)
  )%>%
  collect()%>%
  plotly::plot_ly(x=~age)%>%
  plotly::add_lines(y=~contractual_total,name="Contractual")%>%
  plotly::add_lines(y=~actual_total,name="Actual")




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

PD<-dlnorm(seq(0.05,3,by = 0.05), meanlog = 0, sdlog = 1, log = FALSE)/6

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





  
