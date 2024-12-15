#########################################
### *** FAMILIARIZE WITH DATA SET *** ###
#########################################

## Read data into R ##
getwd()
stores_data<- read.csv("/Users/liliecatania/desktop/store_CA.csv", header = TRUE)
View(stores_data)
## check field classes ##
class(stores_data)

#######################################################################
#### ***SEARCH FOR AREAS IN DATA SET THAT MAY NEED TO BE CLEANED*** ###
#######################################################################
library(tidyverse)
## Check missing values ##
sum(is.na(stores_data))

## Check duplicate records ##
sum(duplicated(stores_data))

## Check range constraints of Employee Efficiency Field ##
sum(stores_data$EmployeeEfficiency < 0 & stores_data$EmployeeEfficiency > 100)

## Check for inconsistency within categorical variables ##
Location_var_check <- stores_data %>%
  count(StoreLocation)
View(Location_var_check)
Category_var_check<- stores_data %>%
  count(StoreCategory)
View(Category_var_check)


########################################
### *** VARIABLE TRANSFORMATIONS *** ###
########################################

## MarketingSpend*1000 for standardized units ##
stores_data2<- stores_data %>%
  mutate(MarketingExp = ifelse(MarketingSpend < 100, MarketingSpend*1000, 0))

## Check for successful field modification ##
Marketing_spend_before_vs_after<- stores_data2 %>%
  select(MarketingSpend,MarketingExp)
View(Marketing_spend_before_vs_after)


## Removed original MarketingSpend column to only keep its mutation ##
stores_data3<- stores_data2 %>%
  select(- MarketingSpend)
## Check for Successful Removal ##
head(stores_data3)

## MonthlySalesRevenue*1000 for standardized units ##
stores_data5<- stores_data3 %>%
  mutate(MonthlySales = ifelse(MonthlySalesRevenue < 999, MonthlySalesRevenue*1000, 0))

## Check for successful field modification ##
Sales_before_vs_after<- stores_data5 %>%
  select(MonthlySalesRevenue, MonthlySales)
View(Sales_before_vs_after)

## Removed original MonthlySalesRevenue column to only keep its mutation ##
stores_data6<- stores_data5 %>%
  select(-MonthlySalesRevenue)
head(stores_data6)

## Renaming columns for clarity purposes ##
col_rename_stores_data <- stores_data6 %>%
  rename(
    Product_Variety = ProductVariety,
    Traffic = CustomerFootfall,
    Store_Size = StoreSize,
    Staff_Efficiency = EmployeeEfficiency,
    Store_Age = StoreAge,
    Competitor_Distance = CompetitorDistance,
    Monthly_Promotions = PromotionsCount,
    Economic_Indicator = EconomicIndicator,
    Location = StoreLocation,
    Store_Type = StoreCategory,
    Marketing_Expense = MarketingExp,
    Monthly_Sales = MonthlySales)

head(col_rename_stores_data, 2)

## Finalized TIDY data set by reordering so characters come first, numerics are last ##
final_stores_data <- col_rename_stores_data[,c(9,10,1,2,3,4,5,6,7,8,11,12)]
head(final_stores_data,4)

##############################
### *** SQL TABLE JOIN *** ###
##############################

###CREATE A DATAFRAME WITH SALES PERFORMANCE METRICS###
######## TO BE JOINED WITH ORIGINAL DATA FRAME########
df2<- final_stores_data %>%
  mutate(Cost_Per_Customer = Marketing_Expense/ Traffic)
head(df2)
df3<- df2 %>%
  mutate(Sales_Per_Customer = Monthly_Sales / Traffic)
head(df3)  
### DATA FRAME WITH SALES PER CUSTOMER AND MARKETING SPEND PER CUSTOMER FOR EACH STORE ###
stores_data_table2<- df3 %>%
  select(Location, Cost_Per_Customer, Sales_Per_Customer)
class(stores_data_table2)
head(stores_data_table2)
dim(stores_data_table2)

library(sqldf)

### USE A WHILE LOOP TO CREATE A VECTOR THAT LABELS OBSERVATION NUMBERS IN DATA TABLES ####
x<- 0
n<-1
while (n<=1650) {
  x[n]<-n
  n <- n +1
}
x
#### ADD VECTOR WITH OBSERVATION NUMBERS TO DATA FRAME AS A NEW COLUMN TO BOTH TABLES ###
fsd_x<- cbind(final_stores_data, data.frame(x)) ##fsd_x is final_stores_data with observation number column ##
head(fsd_x)

fsd2_x<- cbind(stores_data_table2, data.frame(x)) ##fsd2_x is stores_data_table2 with observation number column ##
head(fsd2_x)

### USE SQL TO JOIN TABLES WITH OBSERVATION NUMBER COLUMN AS PRIMARY KEY ###
joinquery<- "SELECT fsd_x.Location, fsd_x.Store_Type, fsd_x.Product_Variety, fsd_x.Traffic, fsd_x.Store_size, fsd_x.Staff_Efficiency, fsd_x.Store_Age, fsd_x.Competitor_Distance, fsd_x.Monthly_Promotions, fsd_x.Economic_Indicator, fsd_x.Marketing_Expense, fsd_x.Monthly_sales, fsd2_x.Location, fsd2_x.Cost_Per_Customer, fsd2_x.Sales_Per_Customer, fsd2_x.x, fsd_x.x 
FROM fsd_x
FULL JOIN fsd2_x
USING (x)"
fsd_joined<- sqldf(joinquery)
head(fsd_joined)
dim(fsd_joined)
View(fsd_joined)

colnames(fsd_x)
dim(fsd_x)
colnames(fsd2_x)
dim(fsd2_x)
colnames(fsd_joined)
dim(fsd_joined)

######################################
### *** VARIABLE DISTRIBUTION *** ###
######################################

## Histogram Product Variety ##
prod_var_hist<- hist(final_stores_data$Product_Variety, main = "Product Variety", 
                     xlab = "No.of Unique Products", 
                     col = "lightblue", border = "black")
prod_var_hist

## Histogram Traffic ##
traffic_hist<- hist(final_stores_data$Traffic, main = "Traffic", 
                     xlab = "No.of Customers", 
                     col = "lightblue", border = "black")
traffic_hist

## Histogram Store Size ##
size_hist<- hist(final_stores_data$Store_Size, main = "Store Size", 
                    xlab = "No.of Square Meters", 
                    col = "lightblue", border = "black")
size_hist

## Histogram Staff Efficiency ##
staff_hist<- hist(final_stores_data$Staff_Efficiency, main = "Staff Efficiency Scores", 
                 xlab = "Scores", 
                 col = "lightblue", border = "black")
staff_hist

## Histogram Store Age ##
age_hist<- hist(final_stores_data$Store_Age, main = "Store Age", 
                  xlab = "No. of Years", 
                  col = "lightblue", border = "black")
age_hist

## Histogram Competitor Distance  ##
competdist_hist<- hist(final_stores_data$Competitor_Distance, main = "Competitor Distance", 
                xlab = "No. of Kilometers", 
                col = "lightblue", border = "black")
competdist_hist

## Histogram Store Age ##
age_hist<- hist(final_stores_data$Store_Age, main = "Store Age", 
                xlab = "No. of Years", 
                col = "lightblue", border = "black")
age_hist

## Histogram Monthly Promotions  ##
promo_hist<- hist(final_stores_data$Monthly_Promotions, main = "Monthly Promotions", 
                       xlab = "No. of Promotions", 
                       col = "lightblue", border = "black")
promo_hist

## Histogram Economic Indicator  ##
econ_hist<- hist(final_stores_data$Economic_Indicator, main = "Economic Indicators", 
                  xlab = "Index Score", 
                  col = "lightblue", border = "black")
econ_hist

## Histogram Marketing Expenses  ##
mktg_hist<- hist(final_stores_data$Marketing_Expense, main = "Monthly Marketing Expenses", 
                 xlab = "$ USD Spent on Marketing", 
                 col = "lightblue", border = "black")
mktg_hist

## Histogram Monthly Sales ##
sales_hist<- hist(final_stores_data$Monthly_Sales, main = "Monthly Sales Revenues", 
                 xlab = "$ USD in Revenues", 
                 col = "lightblue", border = "black")
sales_hist

####################################
### *** EXPLORATORY ANALYSIS *** ###
####################################

##################################################
##########*** COVARIANCE ANALYSIS *** ############
##################################################

#Relationship between monthly promotions and monthly sales
promo_sales_cov <- cov(final_stores_data$Monthly_Sales, final_stores_data$Monthly_Promotions)
print(promo_sales_cov)

#Relationship between age and monthly promotions
promo_age_cov <- cov(final_stores_data$Store_Age, final_stores_data$Monthly_Promotions)
print(promo_age_cov)

#Relationship between staff efficiency and store size
size_score_cov <- cov(final_stores_data$Store_Size, final_stores_data$Staff_Efficiency)
print(size_score_cov)

#Relationship between customer foot traffic and competitor distance 
traffic_competition_cov <- cov(final_stores_data$Traffic, final_stores_data$Competitor_Distance)
print(traffic_competition_cov)

# Relationship between foot traffic and product variety
traffic_variety_cov <- cov(final_stores_data$Product_Variety, final_stores_data$Traffic)
print(traffic_variety_cov)

# Relationship between foot traffic and promotions
traffic_promo_cov <- cov(final_stores_data$Traffic, final_stores_data$Monthly_Promotions)
print(traffic_promo_cov)

# Relationship between foot traffic and marketing expense
traffic_marketing_cov <- cov(final_stores_data$Traffic, final_stores_data$Marketing_Expense)
print(traffic_marketing_cov)

###################################################
##########*** CORRELATION ANALYSIS *** ############
###################################################

### Correlation Heat Map ###
# Use DPLYR to only get numeric variables #
stores_data_numeric <- final_stores_data %>%
  select(-Store_Type, -Location)
# round correlation data to 2 decimal places) #
correlation_matrix<- round(cor(stores_data_numeric), 2)
head(correlation_matrix)
# re-ordering correlation matrix by coefficient value #
install.packages("reshape2")
library(reshape2)
distance<- as.dist((1-correlation_matrix)/2)
# cluster hierarchically #
clustered<- hclust(distance)
correlation_matrix<- correlation_matrix[clustered$order, clustered$order]
# melt correlation matrix #
melted_correlation_matrix <- melt(correlation_matrix)
melted_correlation_matrix
# plot heat map using ggplot #
correlation_heatmap<- ggplot(melted_correlation_matrix, aes(Var1,Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
  theme(axis.text.x = element_text(angle = 90))
correlation_heatmap

# Correlation heatmap Electronic Stores #
library(sqldf)
Qhmap_electronic<-"SELECT *
      FROM final_stores_data
      WHERE Store_Type LIKE 'Elec%'"
Electronic_Store_Query<- sqldf(Qhmap_electronic)

electronic_data_numeric <- Electronic_Store_Query %>%
  select(-Store_Type, -Location)
elec_correl_matrix<- round(cor(electronic_data_numeric), 2)
head(elec_correl_matrix)
dist<- as.dist((1-elec_correl_matrix)/2)
clust<- hclust(dist)
elec_correl_matrix<- elec_correl_matrix[clust$order, clust$order]
melted_elec_correl_matrix <- melt(elec_correl_matrix)
melted_elec_correl_matrix
elec_correl_heatmap<- ggplot(melted_elec_correl_matrix, aes(Var1,Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
  theme(axis.text.x = element_text(angle = 90))
elec_correl_heatmap

# Correlation heatmap Grocery Stores #
Qhmap_grocery<-"SELECT *
      FROM final_stores_data
      WHERE Store_Type LIKE 'Groc%'"
grocery_store_query<- sqldf(Qhmap_grocery)

grocery_data_numeric <- grocery_store_query %>%
  select(-Store_Type, -Location)
groc_correl_matrix<- round(cor(grocery_data_numeric), 2)
head(groc_correl_matrix)
dist<- as.dist((1-groc_correl_matrix)/2)
clust<- hclust(dist)
groc_correl_matrix<- groc_correl_matrix[clust$order, clust$order]
melted_groc_correl_matrix <- melt(groc_correl_matrix)
melted_groc_correl_matrix
groc_correl_heatmap<- ggplot(melted_groc_correl_matrix, aes(Var1,Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#1F968BFF", high ="#BDE29F", guide = "colorbar") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 90))
groc_correl_heatmap

# Correlation heatmap Clothing Stores #
Qhmap_clothing<-"SELECT *
      FROM final_stores_data
      WHERE Store_Type LIKE 'Clo%'"
clothing_store_query<- sqldf(Qhmap_clothing)

clothing_data_numeric <- clothing_store_query %>%
  select(-Store_Type, -Location)
cloth_correl_matrix<- round(cor(clothing_data_numeric), 2)
head(cloth_correl_matrix)
dist<- as.dist((1-cloth_correl_matrix)/2)
clust<- hclust(dist)
cloth_correl_matrix<- cloth_correl_matrix[clust$order, clust$order]
melted_cloth_correl_matrix <- melt(cloth_correl_matrix)
melted_cloth_correl_matrix
cloth_correl_heatmap<- ggplot(melted_cloth_correl_matrix, aes(Var1,Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#a53e76", high ="#eec4dc", guide = "colorbar") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 90))
cloth_correl_heatmap

##########################
#####*** QUERIES ***######
#########################

library(sqldf)
#### *** QUERY 1 ***###
# Sorting records in descending order to find top
# performers in monthly sales
query1 <- "SELECT *
          FROM final_stores_data
          ORDER BY Monthly_Sales DESC
          LIMIT 5"
sqldf(query1)

#### *** QUERY 2 ***###
# Find average marketing expense, average traffic, and average monthly sales
# for each store type
query2 <-"SELECT Store_Type, AVG(Marketing_Expense) AS AVG_Marketing_Expense, 
          AVG(Traffic) AS AVG_Traffic,
          AVG(Monthly_Sales) AS AVG_Monthly_Sales
          FROM final_stores_data
          GROUP BY Store_Type
          ORDER BY AVG_Monthly_Sales DESC"
sqldf(query2)
# Create a working data frame to generate a bar chart
avg_foottraffic <- data.frame(Store_Type = c("Grocery", "Clothing", "Electronics"),
                              Traffic = c(2024.532, 2022.424, 2012.723))
summary(avg_foottraffic)
as_factor(avg_foottraffic$Store_Type)
class(avg_foottraffic$Traffic)
class(avg_foottraffic$Store_Type)
# Plot the bar chart to visualize the effect of store type on foot traffic
ggplot(avg_foottraffic, aes(x = Store_Type, y = Traffic, fill = Store_Type)) + 
  geom_col(stat = "identity", color = "black") + 
  labs(title = "The Effect of Store Type on Foot Traffic",
       x = "Store Type", 
       y = "Total Number of Customers Visiting per Month 
       (on average)") + 
  coord_cartesian(ylim=c(2000, 2025)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("Grocery" = "lightgreen", 
                               "Electronics" = "lightblue", 
                               "Clothing" = "pink"), 
                    name = "Store Type")

#### *** QUERY 3 ***###
# Sort records in descending order to find top performer
# in average monthly sales by location
query3 <-"SELECT Location, AVG(Marketing_Expense) AS AVG_Marketing_Expense,
          AVG(Monthly_Sales) AS AVG_Monthly_Sales
          FROM final_stores_data
          GROUP BY Location
          ORDER BY AVG_Monthly_Sales DESC"
sqldf(query3)

#### *** QUERY 4 ***###
# Explore level of staff efficiency by store type
query4 <-"SELECT Store_Type, AVG(Staff_Efficiency) AS AVG_Staff_Efficiency
          FROM final_stores_data
          GROUP BY Store_Type
          ORDER BY AVG(Staff_Efficiency) DESC"

## Pie chart to visualize average staff efficiency by store type#
pie_data <- sqldf(query4)
labels <- paste(pie_data$Store_Type, round(pie_data$AVG_Staff_Efficiency, 2), "%")
colors <- c("lightblue", "pink", "lightgreen")
pie(pie_data$AVG_Staff_Efficiency, labels = labels, col = colors, edges = 250, 
    radius = 1 , clockwise = TRUE, main = "Average Staff Efficiency by Store Type")
summary(final_stores_data)

#### *** QUERY 5 ***###
### CREATING A BINARY VARIABLE TO MEASURE STAFF EFFICIENCY
### RELATIVE TO THE AVERAGE

#Use DPLYR to create a field where "YES" means staff efficiency is above the average
#across all stores for that particular store, and "NO" means staff is below average
fsd_w_YN_staff<- final_stores_data %>%
  mutate(SE_aboveavg = ifelse(Staff_Efficiency< mean(Staff_Efficiency), "NO", "YES"))
head(fsd_w_YN_staff)

#Use sql to transform into a binary field for aggregation & analysis
query5<- "SELECT *,
CASE
WHEN SE_aboveavg = 'YES' THEN 1
WHEN SE_aboveavg = 'NO' THEN 0
END AS staff_effic_aboveavg
FROM fsd_w_YN_staff"
fsd_w_binary_staff_effic<- sqldf(query5)
head(fsd_w_binary_staff_effic)

#### *** QUERY 6 ***###
query6 <- "SELECT Location, Store_Type, COUNT(*) AS stores_above_avg
        FROM fsd_w_binary_staff_effic
        WHERE staff_effic_aboveavg = 1
        GROUP BY Location, Store_Type
        ORDER BY Location, Store_Type"
count_above_each <- sqldf(query6)
count_above_each

## column chart, clustered by location and filled by store type #
ggplot(count_above_each, aes(x = Location, y = stores_above_avg, fill = Store_Type)) + 
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  scale_fill_manual(
    values = c("Grocery" = "lightgreen", "Clothing" = "pink", "Electronics" = "lightblue")) +
  geom_text( aes(label = stores_above_avg),position = position_dodge(width = 0.8), 
             vjust = -0.5, size = 4) +
  labs(
    title = "Store Counts Above Average Staff Efficiency by Location",
    x = "Location",
    y = "Stores Above Average",
    fill = "Store Type") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5,  size = 16),
    legend.position = "top")

#### *** QUERY 7 ***###
query7 <-"SELECT Location, Economic_Indicator, 
          Monthly_Sales/Traffic AS Traffic_Sales
          FROM final_stores_data
          ORDER BY Traffic_Sales DESC"
econ_spend<- sqldf(query7)
econ_spend

# Generate a scatter plot to explore
ggplot(econ_spend, aes(x = Economic_Indicator, y = Traffic_Sales, 
                       shape = Location, color = Location)) + 
  geom_point(alpha = 0.6) + 
  labs(
    title = "Analyzing Customer Spend in Economic Conditions Through Location",
    x = "Economic Indicator",
    y =  "Customer Spend ($)") + 
  theme_classic() + 
  theme(
    legend.background = element_rect(size = 0.5, color = "black" ))

#### *** QUERY 8 ***###
query8 <- "SELECT Store_Age, Store_Type, AVG(Monthly_Sales) AS Average_Monthly_Sales
            FROM final_stores_data
            GROUP BY Store_Age, Store_Type
            ORDER BY Store_Age, Store_Type;"
query8df <- sqldf(query8)
head(query8df)

#Generate smooth line graph showing effect of a store's age on their average monthly sales
ggplot(query8df, aes(x = Store_Age, y = Average_Monthly_Sales, color = Store_Type)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Examining the Impact of Store Age on \nMonthly Revenue Across Store Types",
    x = "Store Age (Years)",
    y = "Average Monthly Sales Revenue ($)",
    color = "Store Type") +
  scale_color_manual(values = c("Grocery" = "lightgreen", 
                                "Electronics" = "lightblue", 
                                "Clothing" = "pink")) +
  theme_classic() +                          
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)) 

#### *** EXTENSIONS OF QUERY 8 ***###
#query 8.1
query8_clothingvar <- "SELECT Store_Age, Store_Type, 
AVG(Monthly_Sales) AS Average_Monthly_Sales
            FROM final_stores_data
            WHERE Store_Type = 'Clothing'
            GROUP BY Store_Age, Store_Type 
            ORDER BY Store_Age, Store_Type"
query8_clothinggraph <- sqldf(query8_clothingvar)

#line graph showing effect of a clothing store's age on their average monthly sales
ggplot(query8_clothinggraph, aes(x = Store_Age, y = Average_Monthly_Sales, color = Store_Type)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Examining the Impact of Store Age on \nMonthly Revenue of Clothing Stores",
    x = "Store Age (Years)",
    y = "Average Monthly Sales Revenue ($)",
    color = "Store Type") +
  scale_color_manual(values = c("Clothing" = "pink")) +
  theme_classic() +                          
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

#query 8.2
query8_groceryvar <- "SELECT Store_Age, Store_Type, 
AVG(Monthly_Sales) AS Average_Monthly_Sales
            FROM final_stores_data
            WHERE Store_Type = 'Grocery'
            GROUP BY Store_Age, Store_Type 
            ORDER BY Store_Age, Store_Type"
query8_grocerygraph <- sqldf(query8_groceryvar)

#line graph showing effect of a grocery store's age on their average monthly sales
ggplot(query8_grocerygraph, aes(x = Store_Age, y = Average_Monthly_Sales, color = Store_Type)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Examining the Impact of Store Age on \nMonthly Revenue Across Grocery Stores",
    x = "Store Age (Years)",
    y = "Average Monthly Sales Revenue ($)",
    color = "Store Type") +
  scale_color_manual(values = c("Grocery" = "lightgreen")) +
  theme_classic() +                          
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

#query 8.3
query8_electronicvar <- "SELECT Store_Age, Store_Type, 
AVG(Monthly_Sales) AS Average_Monthly_Sales
            FROM final_stores_data
            WHERE Store_Type = 'Electronics'
            GROUP BY Store_Age, Store_Type 
            ORDER BY Store_Age, Store_Type"
query8_electronicgraph <- sqldf(query8_electronicvar)

#line graph showing effect of a electronic store's age on their average monthly sales
ggplot(query8_electronicgraph, aes(x = Store_Age, y = Average_Monthly_Sales, color = Store_Type)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Examining the Impact of Store Age on \nMonthly Revenue Across Electronic Stores",
    x = "Store Age (Years)",
    y = "Average Monthly Sales Revenue ($)",
    color = "Store Type") +
  scale_color_manual(values = c("Electronics" = "lightblue")) +
  theme_classic() +                          
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

#### *** QUERY 9 ***###
#Query data for the selected fields to be arranged for the top 10 highest product varieties
query9top10 <- "SELECT Store_Size, Monthly_Sales, Product_Variety, Store_Type
            FROM final_stores_data
            ORDER BY Product_Variety DESC
            LIMIT 10"
graphtop10 <- sqldf(query9top10)
#Query data for the selected fields to be arranged for the top 10 lowest product varieties
query9bottom10 <- "SELECT Store_Size, Monthly_Sales, Product_Variety, Store_Type
            FROM final_stores_data
            ORDER BY Product_Variety 
            LIMIT 10"
graphbottom10 <- sqldf(query9bottom10)
query9_combined_data <- rbind(graphtop10, graphbottom10)
query9_combined_data

# Plot points to show magnitude of product variety in relation to sales
#Plot points to position of store size in relation to monthly sales 
ggplot(query9_combined_data, aes(x = Store_Size, y = Monthly_Sales, size = Product_Variety, 
                                 colour = Store_Type)) + 
  geom_point(alpha = 1) + 
  scale_size_continuous(name = "Product Variety") + 
  scale_color_manual(values = c("Grocery" = "lightgreen", 
                                "Electronics" = "lightblue", 
                                "Clothing" = "pink")) + 
  labs(title = "The Effect of Store Size and Product Variability \non Monthly Revenue",
       x = "Store Size (square ft.)",
       y = "Monthly Sales Revenue ($)",
       color = "Store Type") + 
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right")

# Scatter plot with trend line exploring how a store's product variety
# explains variation in monthly sales
ggplot(final_stores_data, aes(x = Product_Variety, y = Monthly_Sales)) +
  geom_point(size = 2, color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "violet") +
  labs(title = "Product Variety vs. Monthly Sales",
       x = "Product Variety",
       y = "Monthly Sales") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
## Linear equation output ##
lmss<- lm(Monthly_Sales~Product_Variety, final_stores_data)
summary(lmss)

#### *** QUERY 10 ***###
# Query data table for product variety & store type
Q10prodvar_st<- "SELECT product_variety, Store_Type
                FROM final_stores_data"
prodvar_st<- sqldf(Q10prodvar_st)

# use df from query to map variables
plot_variet<-ggplot(prodvar_st,aes(x=Product_Variety, fill = Store_Type)) +
  theme_classic()
#Add density plot geom layers to aesthetic mapping, use position arguments for clarity
prodvar_storetype<-plot_variet +
  geom_density(aes(y=..density..), position = "jitter", alpha = 0.5) +
  scale_fill_manual(values = c("Grocery" = "lightgreen", "Electronics" = "lightblue", 
                               "Clothing" = "pink")) +
  labs(title = "Density of Product Variety Across Store Types", 
       x = "Number of Unique Products", fill = "Store Type", y = "Density")
prodvar_storetype

#### *** QUERY 11 ***###
# Query data table for monthly sales & store type fields
Q11sales_st<- "SELECT Store_Type, Monthly_sales
              FROM final_stores_data"
sales_st<- sqldf(Q11sales_st)

### Generate Box plot of monthly sales distribution by store type ###
bxplt_sales_by_storetype<- ggplot(sales_st, aes(Monthly_Sales, fill = Store_Type)) +
  geom_boxplot() +
  facet_wrap(~Store_Type, ncol=1) +
  scale_fill_manual(values = c("Grocery" = "lightgreen", "Electronics" = "lightblue", 
                               "Clothing" = "pink"), name = "Store Type") +
  labs(title = "Summary of Monthly Sales by Store Type", x = "Monthly Sales") +
  theme_classic() +  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        plot.title = element_text(hjust = 0.5))
bxplt_sales_by_storetype

#normality tests
mean_grocery<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Grocery")

mean_clothing<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Clothing")

mean_electronics<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Electronics")
mean(mean_grocery$Product_Variety)
library(tseries)
jbg<- jarque.bera.test(mean_grocery$Product_Variety)
jbg
jbc<- jarque.bera.test(mean_clothing$Product_Variety)
jbc
jbe<- jarque.bera.test(mean_electronics$Product_Variety)
jbe

#### *** QUERY 12 ***###
fsd_joined<- fsd_joined %>%
  select(Sales_Per_Customer, Staff_Efficiency)
# Query data table for Sales Per Customer and Employee Efficiency ###
Q12spc_ee<- "SELECT Sales_Per_Customer, Staff_Efficiency
                FROM fsd_joined"
spc_eedf<- sqldf(Q12spc_ee)
# Add a column where the staff efficiency scores are labeled by quartile number ###
spc_eedf$Quartile <- with(spc_eedf, cut(Staff_Efficiency, 
                      breaks=quantile(Staff_Efficiency, probs=seq(0,1, by=0.25), 
                      na.rm=TRUE), include.lowest=TRUE))
head(spc_eedf,10)
#Query to create column labeling percentiles
Q12.2<- "SELECT *,
          CASE
WHEN Quartile = '(83.1,94.9]' THEN '100th'
WHEN Quartile = '(72.1,83.1]' THEN '75th'
WHEN Quartile = '(61,72.1]' THEN '50th'
WHEN Quartile = '[50,61]' THEN '25th'
END AS Percentile
FROM spc_eedf"
ee_percentiles<- sqldf(Q12.2)
View(ee_percentiles)

# Plot violin plots for each quartile 
Emp_eff_SPC<- ggplot(ee_percentiles, aes(as.factor(Quartile), Sales_Per_Customer, fill = Quartile)) +
  geom_violin(alpha = 0.5)+
  labs(title = "Employee Efficiency's Effect on Sales per Customer",
       x = "Percentiles of Employee Efficiency", 
       y = "Sales Per Customer", fill = "Percentile") + 
  scale_x_discrete(labels = c("25th", "50th", "75th", "100th"))+
  scale_fill_discrete(labels = c("25th", "50th", "75th", "100th"))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
Emp_eff_SPC

#### *** QUERY 13 ***###
query13 <-"SELECT Location, Store_Type, AVG(Marketing_Expense) AS AVG_Marketing_Expense,
          AVG(Monthly_Sales) AS AVG_Monthly_Sales
          FROM final_stores_data
          GROUP BY Location, Store_type
          ORDER BY Location, Store_type"
sqldf(query13)
query13df <- sqldf(query13)
head(query13df)
#ggplot
ggplot(query13df, aes(x = AVG_Marketing_Expense,y = AVG_Monthly_Sales,color = Store_Type,
  shape = Location)) + geom_point(size = 4) + 
  labs(title = "Analyzing Ad Spend and Revenue\nAcross Store Types and Cities",
       x = "Average Monthly Marketing Expense ($)",
       y = "Average Monthly Sales ($)",
       color = "Store Type",
       shape = "City") +
  scale_color_manual(values = c("Grocery" = "lightgreen", "Electronics" = "lightblue", 
                                "Clothing" = "pink"), name = "Store Type") +
  theme_classic() + 
  theme( text = element_text(size = 12), legend.position = "right", 
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(color = "black", size = 0.5))


########################################################
#### Additional code used for further exploration #####
#######################################################
library(sqldf)
library(ggplot2)


mean(final_stores_data$Monthly_Sales)
q333<- "SELECT AVG(Monthly_Sales) AS Av_sales, Store_Type
FROM final_stores_data
GROUP BY Store_Type"
avgsal<- sqldf(q333)
avgsal

q334<- "SELECT MEDIAN(Monthly_Sales) AS Med_sales, Store_Type
FROM final_stores_data
GROUP BY Store_Type"
medsal<- sqldf(q333)
medsal

lmsales_prodvar<-lm(Monthly_Sales~Product_Variety, final_stores_data)
summary(lmsales_prodvar)
plot(final_stores_data$Monthly_Sales~final_stores_data$Product_Variety)
abline(lmsales_prodvar$coefficients[1], lmsales_prodvar$coefficients[2], col='red', lwd=3)
plot(lmsales_prodvar$fitted.values~final_stores_data$Product_Variety, add=TRUE, col = "blue")

lmsales_prodvar<-lm(Monthly_Sales~Product_Variety, final_stores_data)
summary(lmsales_prodvar)
plot(final_stores_data$Monthly_Sales~final_stores_data$Product_Variety)
abline(lmsales_prodvar$coefficients[1], lmsales_prodvar$coefficients[2], col='red', lwd=2)
lmsales_prodvarsqft<-lm(Monthly_Sales~Product_Variety + Store_Size, final_stores_data)
summary(lmsales_prodvarsqft)
plot(final_stores_data$Monthly_Sales~Product_Variety, final_stores_data)
abline(lmsales_prodvarsqft$coefficients[1], lmsales_prodvarsqft$coefficients[2], lmsales_prodvarsqft$coefficients[3],col='blue', lwd=2)

lmsqft<- lm(Monthly_Sales~Store_Size, final_stores_data)
summary(lmsqft)
plot(final_stores_data$Monthly_Sales)
abline(lmsales_prodvar$coefficients[1], lmsales_prodvar$coefficients[2], col='red', lwd=2)
abline(lmsqft$coefficients[1], lmsqft$coefficients[2], col='green', lwd=2)
abline(lmsales_prodvarsqft$coefficients[1], lmsales_prodvarsqft$coefficients[2], lmsales_prodvarsqft$coefficients[3],col='blue', lwd=2)
legend("topleft", legend=c("Regression Store Seize", "Regression Product Variety", "Regression Store Size + Product Vairety"),
       col=c("red", "green", "blue"), lty=1:2, cex=0.6,
       box.lty=2, box.lwd=2, box.col="black")
plotvrsq<- ggplot(varsqmord, aes(x=varsqm, Monthly_Sales)) +
  geom_violin()
plotvrsq
mean_grocery<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Grocery")
mean_clothing<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Clothing")
mean_electronics<- final_stores_data %>%
  select(Store_Type, Product_Variety) %>%
  filter(Store_Type == "Electronics")
mean(mean_grocery$Product_Variety)
library(tseries)












