library(sparklyr)
library(tidyverse)

spark_home_set("Spark/spark-2.2.1-bin-hadoop2.7/")
sc <- spark_connect(master = "local") 

import_iris <- copy_to(sc, iris, "spark_iris", overwrite = TRUE)

partition_iris <- sdf_partition(import_iris,training=0.5, testing=0.5)

sdf_register(partition_iris,c("spark_iris_training","spark_iris_test")) 

tidy_iris <- tbl(sc,"spark_iris_training") %>%
  select(Species, Petal_Length, Petal_Width) 

model_iris <- tidy_iris %>%
  ml_decision_tree(
    response="Species",
    features=c("Petal_Length","Petal_Width")
  )


test_iris <- tbl(sc,"spark_iris_test") 

pred_iris <- sdf_predict(model_iris, test_iris) %>% collect

pred_iris %>%
  inner_join(
    data.frame(
      prediction=0:2,
      lab=model_iris$model.parameters$labels
    )
  ) %>%
  ggplot(aes(Petal_Length, Petal_Width, col=lab)) +
  geom_point() 

spark_disconnect(sc)
