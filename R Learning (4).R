dataset = read.csv('dataset_superstore_simple.csv')

summary (dataset)

head (dataset)
head (dataset,7)

NROW(dataset)

NCOL(dataset)

library(dplyr)
glimpse(dataset)

select(dataset,category)
distinct(dataset,customer_id)
select(dataset, c(order_id,segment,category))
select(dataset, -c(segment, category))

filter(dataset, sub_category == 'Paper')
filter(dataset, category == 'Furniture', profit>20.000)
orstatement = filter(dataset, category == 'Furniture'|profit>20.000)
head (orstatement,10)
filter(dataset, category == 'Furniture' & profit>100.000)
filter(dataset, segment != 'Consumer')

mutate(dataset, avg_price = sales/quantity) #ada kolom baru dan semua kolom diperlihatkan
transmute(dataset, avg_price = sales/quantity) #hanya kolom baru yang diperlihatkan

dataset%>%filter(segment == 'Home Office')%>%mutate(avg_price = sales/quantity)%>%select(-c(profit))

dataset%>%group_by(segment)%>%summarise(total_profit = sum(profit))
hasilakhir = dataset%>%group_by(segment,category)%>%summarise(total_sales = sum(sales), 
                                        avg_price = sales/quantity, 
                                        avg_sales = mean(sales),
                                        min_profit = min(profit),
                                        max_quantity = max(quantity),
                                        n_order = n())

data_a = dataset%>%filter(sub_category=='Art')%>%select (c('order_id','category','segment','sub_category','profit','sales','quantity'))%>%head(10)
data_b = dataset%>%filter(segment == 'Consumer')%>%select(c('order_id','category','segment','sub_category','profit','sales','quantity'))%>%head(10)
intersect(data_a,data_b)
union(data_a,data_b)
bind_rows(data_a,data_b)
setdiff(data_b,data_a)
setdiff(data_a,data_b)

data_c = select(data_a, c('order_id','segment','sub_category','sales','profit'))
data_d = select(data_b, c('order_id','segment','category','sales','quantity'))
bind_cols(data_c,data_d)
inner_join(data_c,data_d)
full_join(data_c,data_d)
left_join(data_c,data_d)
left_join(data_c,data_d)

library(ggplot2)

ggplot(dataset, aes(x = sales, y = profit)) + geom_point(colour = 'Green')
ggplot(dataset, aes(x = profit)) + geom_histogram(fill = 'Maroon', bins = 50, binwidth = 750)
ggplot(dataset, aes(x = category, y = quantity, fill = segment)) + geom_bar(stat = 'identity', width = 0.5)

quantity_per_category= dataset%>%group_by(category)%>%summarise(total_quantity = sum(quantity))
ggplot(quantity_per_category, aes(x="", y=total_quantity,fill = category)) + geom_bar(stat = 'identity', width = 1) + coord_polar("y", start = 0)

dataset$order_date = as.Date(dataset$order_date)
dataset$order_month = as.Date(cut(dataset$order_date, breaks = 'month'))       
ggplot(dataset, aes(x = order_month, y = sales)) + stat_summary(fun.y = sum, geom ='line')

monthly_sales = dataset%>%group_by(order_month)%>%summarise(total_sales = sum(sales))
ggplot(monthly_sales, aes(x = order_month, y = total_sales)) + geom_line() + geom_point(colour = 'Red')

library(dplyr)
library(ggplot2)
dataset = read.csv('dataset_superstore_simple.csv')
Linear_Regression_from_scatterplot = ggplot(dataset, aes(x = sales, y = profit)) + geom_point(aes(color = category), size = 2, shape = 15) + 
  geom_smooth(method = 'lm', color = 'Black', linetype = 'dotted', size = 1) + 
  labs(title = 'Scatter Plot Sales Vs Profit', 
       subtitle = 'Based on Dataset Superstore', 
       caption = 'R Language Tutorial') + 
  theme(plot.title = element_text(color = 'Red', size = 17, face = 'bold'), 
        plot.subtitle = element_text(color = 'Blue', size =13, face = 'italic')) +
  theme(legend.position = c(0.8,0.2), 
        legend.title = element_text(color = 'dark green', size = 11, face = 'bold'),
        legend.text = element_text(color = 'brown', face = 'bold')) +
  xlab("order_sales") + ylab('order_profit') +
  xlim(c(0,7500)) + ylim(c(-2500,2500))

ggsave('Scatter_Plot_Sales_Vs_Profit.png', Linear_Regression_from_scatterplot)

Linear_Regression_from_scatterplot 
