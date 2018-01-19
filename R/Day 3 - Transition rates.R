

library(tidyverse)
library(sparklyr)

spark_home_set("C:/Spark/spark-2.2.1-bin-hadoop2.7")
sc<-spark_connect(master="local") # Create a connection to spark

data<-spark_read_csv(sc,"loans_data","Data/tranition_data.csv",memory = FALSE)

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




###############################################################################################
#' the code is the same and the source of the data is in different locations, but the same code still works
#' Note that Spark changes columns of the form xxx.yyy to xxx_yyy


# Local R data
iris%>%
  group_by(Species)%>%
  summarise(
    n=n(),
    ave=mean(Sepal.Length)
  )


# database data
library(DBI)
library(RSQLite)

db<-dbConnect(RSQLite::SQLite(),dbname=":memory:")

iris_db<-copy_to(db,iris)

iris_db%>%
  group_by(Species)%>%
  summarise(
    n=n(),
    ave=mean(Sepal.Length)
  )



# Spark data
library(sparklyr)

spark_home_set("C:/Spark/spark-2.2.1-bin-hadoop2.7")
sc<-spark_connect(master="local") # Create a connection to spark

iris_sc<-copy_to(sc,iris)

iris_sc%>%
  group_by(Species)%>%
  summarise(
    n=n(),
    ave=mean(Sepal_Length)
  )





