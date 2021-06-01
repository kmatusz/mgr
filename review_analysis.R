library(tidyverse)

library(readr)
review_reduction <- read_csv("review_reduction.csv")


review_reduction %>% 
  select(-X1) -> review_reduction

library(plotly)
review_reduction %>%
  ggplot(aes(x = x1, y = x2, label=message_en)) +
  geom_point() ->p
  ggplotly(p)

kmeans(review_reduction %>% select(x1,x2), centers=5) -> k

review_reduction$cluster =k$cluster

