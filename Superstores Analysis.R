#Data Processing

#Loading essential libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

df = read.csv('Superstore.csv', encoding = "latin1")
View(df)

#Data Cleaning
str(df)
dim(df)
sum(is.na(df))
sum(duplicated(df))


#there were no missing values nor duplicated values

#change date columns to datetime format
df$Order.Date <- as.POSIXct(df$Order.Date, format = "%m/%d/%Y")

#Descriptive Analysis
summary(df)



#Sales Analysis



#Histogram of Sales
ggplot(df,aes(x=Sales))+ geom_histogram(binwidth = 100, color ='blue')+
  labs(x= "Sales", y= "Frequency",title = "Distribution of Sales")
#majority of sales are between 0 to 2000




#### Sales Trend over the years
df$year <- year(df$Order.Date)
Sales_by_year = df %>% group_by(year) %>% summarise(Total.Sales = sum(Sales))
ggplot(Sales_by_year, aes(x = year, y = Total.Sales)) +
  geom_line(color = 'blue') +
  labs(x = 'Year', y = 'Sales', title = 'Sales Trends over the Years')
#In 2015, sales reached their lowest point, but subsequently exhibited a consistent upward trend
 



#Monthly Sales trend
monthly_sales = df %>%
  group_by(Month = format(Order.Date, "%b")) %>%
  summarise(Total.Sales = sum(Sales))

month_order = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_sales$Month = factor(monthly_sales$Month, levels = month_order)
ggplot(monthly_sales, aes(x = Month, y = Total.Sales, group = 1)) +
  geom_line(color = 'blue') +
  labs(x = 'Months', y = 'Total Sales', title = 'Monthly Sales Trend')
#The Monthly sales trend reveals a pattern. In January and February, they're a bit slow after the holidays. In March and April, sales go up, possibly because of different reasons like better economic conditions or promotions. May and June stay pretty steady, and August sees a small increase. The most interesting part is later in the year. In September, sales shoot up, and they're even higher in November and December during the holiday season. So, it looks like people tend to spend more around holidays. 




# Top Selling Categories, Subcategories and Products


#Top Selling Categories
top_categories = df %>% count (Category)
ggplot(top_categories, aes(x= reorder(Category,-n), y = n))+
  geom_bar(stat='identity',fill='purple')+
  labs(x= 'Categories', y= 'Counts', title = 'Top Selling Categories')


#Top Selling SubCategories
top_Subcategories = df %>% count (Sub.Category) %>%  top_n(5,n)
ggplot(top_Subcategories, aes(x= reorder(Sub.Category,-n), y = n))+
  geom_bar(stat='identity',fill='purple')+
  labs(x= 'SubCategories', y= 'Counts', title = 'Top Selling Categories')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))


# Top Selling product
top_product = df %>% count(Product.Name) %>%  top_n(5,n)
ggplot(top_product, aes(x= reorder(Product.Name, -n), y =n))+
  geom_bar(stat = 'identity', fill = 'purple')  +
  labs(x ='Product', y= 'Count', title = 'Top Selling Products')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))

#From the above, in terms of total number of sales, Office supplies emerged as the top-selling category in the product range. Within the office supplies category, the subcategory of binders and papers stood out as the highest-performing subcategory.The top selling products in the inventory include "Staple Envelope," "Easy-Staple Paper," and "Staples." 




#Sales by Product Catgory
category_sales = df %>% group_by(Category) %>% summarise(Total.sales = sum(Sales))
ggplot(category_sales, aes(x = "", y = Total.sales, fill = Category)) +
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Sales by Category") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightcoral"))+
  geom_text(aes(label = scales::percent(Total.sales / sum(Total.sales))),
            position = position_stack(vjust = 0.5))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
#While "Office Supplies" stands out as the top-selling product category, the chart above reveals that the "Technology" category commands the highest sales figures among all product categories.




#Sales by Region
region_sales <- df %>% group_by(Region) %>% summarise(Total.sales = sum(Sales))
ggplot(region_sales, aes(x = "", y = Total.sales, fill = Region)) +
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Sales by Region") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightcoral", "yellow"))+
  geom_text(aes(label = scales::percent(Total.sales / sum(Total.sales))),
            position = position_stack(vjust = 0.5))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
#The Western region is leading in terms of sales performance, followed by the Eastern and Central regions. The Southern region records the lowest sales figures. 




#Sales by State
State_sales = df %>% group_by(State) %>% summarise(Total.sales = sum(Sales)) %>% 
  arrange(desc(Total.sales)) %>%top_n(10)

ggplot(State_sales, aes(x= reorder(State, -Total.sales), y =Total.sales))+
  geom_bar(stat = 'identity', fill = 'blue')  +
  labs(x ='State', y= 'Sales', title = 'Sales by State')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#California has the highest sale figure, followed by New York, then Texas




#Sales by City
City_sales = df %>% group_by(City) %>% summarise(Total.sales = sum(Sales)) %>% 
  arrange(desc(Total.sales)) %>%top_n(10)

ggplot(City_sales, aes(x= reorder(City, -Total.sales), y =Total.sales))+
  geom_bar(stat = 'identity', fill = 'blue')  +
  labs(x ='City', y= 'Sales', title = 'Sales by City')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#New York city is the highest in terms of sales figure, followed by Los Angeles and Seattle 




# Correlation Analysis


correlation = cor(df[c('Sales', 'Profit', 'Quantity', 'Discount')])
heatmap(correlation, main = 'Correlation Heatmap',col = colorRampPalette(c("blue", "white", "red"))(50), 
        scale = "none",cexCol = 1, cexRow = 1,margins = c(5, 5))
#In the correlation analysis, notably, a moderate positive correlation is observed between Sales and Profit, suggesting that as Sales increase, Profit tends to rise as well, which can be a critical insight for enhancing profitability. Additionally, Sales and Quantity exhibit a weaker positive correlation, indicating a subtle tendency for Quantity to increase with Sales. Conversely, the correlation between Profit and Quantity is minimal, implying that other factors beyond Quantity have a more substantial influence on Profit.





# Profit Analysis



# Profit Trends over Time
profit_by_year = df %>% group_by(year) %>% summarise(Total.Profit = sum(Profit))
ggplot(profit_by_year, aes(x = year, y = Total.Profit)) +
  geom_line(color = 'darkgreen') +
  labs(x = 'Year', y = 'Profit', title = 'Profit Trends over Time')
#Profit tends to increase over the years




#Average pofit margin by category
df$Profit.Margin = df$Profit / df$Sales
category_profit_margin = df %>% group_by(Category) %>% summarise(Avg.Profit.Margin = mean(Profit.Margin))

ggplot(category_profit_margin, aes(x= reorder(Category, -Avg.Profit.Margin), y =Avg.Profit.Margin))+
  geom_bar(stat = 'identity', fill = 'darkgreen')  +
  labs(x ='Category', y= 'Profit', title = 'Average Profit Margin by Category')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#Examining the profitability across different product categories, Technology category has the highest profit margins, followed by Office Supplies, and lastly, Furniture, which exhibits comparatively lower profit margins.




#Profit Magin by region
region_profit_margin = df %>% group_by(Region) %>% summarise(Total.Profit.Margin = sum(Profit.Margin))

ggplot(region_profit_margin, aes(x= reorder(Region, -Total.Profit.Margin), y =Total.Profit.Margin))+
  geom_bar(stat = 'identity', fill = 'darkgreen')  +
  labs(x ='Region', y= 'Profit', title = 'Profit Margin by Region')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#The analysis of profit margins across different regions reveals varying profitability levels. The West region exhibits the highest profitability, followed by the East and South regions, while the Central region shows a lower and negative profitability level. These regional differences in profitability suggest potential opportunities for strategic adjustments and resource allocation to enhance overall profitability.




#Profit Margin by State
State_profit_margin = df %>% group_by(State) %>% summarise(Total.Profit.Margin = sum(Profit.Margin))%>% arrange(desc(Total.Profit.Margin)) %>%top_n(10)

ggplot(State_profit_margin, aes(x= reorder(State, -Total.Profit.Margin), y =Total.Profit.Margin))+
  geom_bar(stat = 'identity', fill = 'darkgreen')  +
  labs(x ='State', y= 'Profit', title = 'Profit Margin by State')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#California brings the highest profit, followed by New York and Washington




#Profit Margin by City
City_profit_margin = df %>% group_by(City) %>% summarise(Total.Profit.Margin = sum(Profit.Margin))%>% arrange(desc(Total.Profit.Margin)) %>%top_n(10)

ggplot(City_profit_margin, aes(x= reorder(City, -Total.Profit.Margin), y =Total.Profit.Margin))+
  geom_bar(stat = 'identity', fill = 'darkgreen')  +
  labs(x ='City', y= 'Profit', title = 'Profit Margin by City')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#New York City brings the highest profit, followed by Los Angeles and San Francisco




# Profit by customer segment
segment_profit <- df %>% group_by(Segment) %>% summarise(Total.Profit = sum(Profit))

ggplot(segment_profit, aes(x= reorder(Segment, -Total.Profit), y =Total.Profit))+
  geom_bar(stat = 'identity', fill = 'darkgreen')  +
  labs(x ='Segment', y= 'Profit', title = 'Profit Margin by Segment')+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
#Across different customer segments, the Consumer segment has the highest total profit, followed by the Corporate and Home Office segments




# Customer Analysis



#Top purchasing segment
customer_segments <- df %>% count(Segment)
ggplot(customer_segments, aes(x = "", y = n, fill = Segment)) +
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = NULL, fill = NULL, title = 'Customer Segmentation') +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "yellow"))+
  geom_text(aes(label = scales::percent(n / sum(n))),
            position = position_stack(vjust = 0.5))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
#Consumers are the highest purchasing customer segment




#Customer Churn Analysis



# Calculate the latest order date in the dataset
latest_order_date = max(df$Order.Date)


# Define a churn threshold
churn_threshold = latest_order_date -months(12)


# Create a new column 'Churned' to identify churned customers
df = df %>%
  mutate(Churned = ifelse(Order.Date <= churn_threshold, "Churned", "Retained"))


# Create a summary table of churned vs. retained customers
churn_summary = df %>%
  group_by(Churned) %>%
  summarise(CustomerCount = n())


# Create a pie chart
ggplot(churn_summary, aes(x = "", y = CustomerCount, fill = Churned)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y")+
  labs(title = "Churned vs. Retained Customers", fill = "Customer Status")  +
  scale_fill_manual(values = c("Churned" = "red", "Retained" = "green"))+
  geom_text(aes(label = scales::percent(CustomerCount / sum(CustomerCount))),
            position = position_stack(vjust = 0.5))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

#customer churn was determined by considering customers who had not made any purchases in the last 12 months as churned customers. This identify and distinguish customers who continue to patronize from those who disengaged.
#The results of the churn analysis indicate that 33% of the customer base was retained and 67% of customers were categorized as churned.

