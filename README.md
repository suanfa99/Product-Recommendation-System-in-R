# Product-Recommendation-System-in-R
Product Recommendation system for Online Retailers (in R)

Personalized recommendations are an important part of many e-commerce applications. Recommender systems are gaining tremendous popularity across companies, such as Amazon and Netflix, due to their effectiveness and efficiency in helping users filter through enormous numbers of items and in helping enterprisers increase their sales. This paper presents an e-commerce based recommendation system that can be used by the retailer to increase profits and differentiate itself from its competitors. The model is based on collaborative filtering and creating clusters for the customers. The main analysis performed is RFM and Market basket analysis. The model trains itself using past purchases to predict future recommendations for customers of similar nature.

The analysis was broken down into two steps:

Clustering of the customers’ basis their RFM (Recency-Frequency-Monetary) signals of past purchases. Number of clusters were decided basis both hierarchical & non-hierarchical (k-means) clustering. It was then validated basis the business requirement of the Online retailer which is in line with their marketing campaign. 

User-based collaborative filtering & association mining approach on the identified set of clusters. These models were used to perform Market Basket Analysis on the whole set of data to recommend the products to a specific user based on the products frequently bought together in that cluster. R provided us with built-in functions and libraries to make the model fast and efficient. 
