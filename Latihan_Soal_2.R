tugas_2 = read.csv("dataset_superstore_simple.csv")
library(dplyr)
jawaban_nomor_2 = tugas_2%>%
  group_by(category,sub_category)%>%
  summarise(total_profit = sum(profit))%>%
  filter(category == ("Office Supplies"))

