# __Retail Analytics__

### __With Proof of Concept Shiny App__

================================================================================

### __Project Goal__
Create a 3 part retail analytics solution for an online retailer. 

#### __Customer Lifetime Value__ 
This helps the marketing team with customer analysis by using machine learning to 
understanding/prediction the probability of a customer making a purchase
within a certain time frame as well as how much the customer might spend. RFM 
(Recency, Frequency and Monetary) features were extracted and used as predictors.
Additionally, This analysis focuses on a particular cohort of customers, i.e the 
customers with a first purchase date in Q1 2010. This represents the largest 
cohort of customers in terms of first purchase dates. 


<div style="display:flex; justify-content:center;">
<img img src="/Users/BachataLu/Desktop/School/2023_projects/retail_analytics/png/customer_red.png" style="width:40%;margin-right:10px;">
<img img src="/Users/BachataLu/Desktop/School/2023_projects/retail_analytics/png/customer_green.png" alt="image2" style="width:40%;margin-right:10px;">
</div>



In examples above, we can analysis for 2 customers. For the customer on the left,
the model predicted a high probability of making a purchase in the next 90 days (92%). However 
while this customer did make a purchase, the amount spent was over $3K less than 
what the model predicted ($733 actual vs $3216 predicted). This customer will be
a prime candidate to reach out to and recommend additional products to purchase (see product recommender tab).


For the customer on the right however, we can see that this customer also had a 
high probability of making a purchase and this customer did spend $2K higher than
the model predicted ($4759 actual vs $2671 predicted). This customer will be a prime candidate
to further analyze, understand their spending habits and use that to analyze other customers. 

#### __Product Recommender__
Builds on the CLV Analysis by using Collaborative Filtering to recommend products to 
customers to customers based on what similar customers have purchased in the prior
90 days. For example we see that customer 15125 purchased $3K less than predicted. We can 
recommend new products to this customer based on what similar customers have purchased.

![](png/pr_recommend.png)

__Caveat -__ This analysis use [user-based collaborative filtering](https://www.geeksforgeeks.org/user-based-collaborative-filtering/), which recommends 
products based on what similar customers have purchased. Meaning that a customer has 
to have made a purchase in the analysis time frame in order to find similarities with
other customers and thus recommend products. In a real business case, there may be
situations where an [item-based collaborative filtering](https://www.geeksforgeeks.org/item-to-item-based-collaborative-filtering/) method is used instead. This method uses relationships between pairs of products to recommend new products to customers. 
---

#### __Forecast___



