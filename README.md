## Description of case

More industries and organizations are applying machine learning methods to optimize resources and maximise the efficiency of their businesses.  
Owing to this, the sporting organizations are increasingly relying on applications of Probabiliistic models and Bayesian methods to create and define rules for better performance.

In one such scenario, imagine you are incharge managing the soccer team for a leading club in Europe.
How can you find the best combination of players within your squad to maximise the probability of a positive result against a
stubborn opponent?


## Approach

Here, I make use of Association Rules based on the Apriori algorithm to figure out the strongest links in wins and losses that have occured in the past results. This is then used to make inferences about the common links and rules and reshape the squad strategies for the future. This specially helps when the team has a tight budget and thus are limited in the transfer market to pursue new (better) players. 


#### Dataset

The dataset sample can be obtained from Kaggle <https://www.kaggle.com/karangadiya/fifa19>. 
This included team, match and player level of details for every team in the European Soccer (Football) leagues.

#### Analysis 

For the analysis, AS Roma was picked as the parent club for which we are trying to find winning solutions.

The numerous steps were following:

* Exploratory analysis to visualize the performance of AS Roma in the Serie A for last 8 seasons
  * To identify the overall performance of Serie A leaders and followers and the competition shape, identify top opponents
* Using Team-Player attributes to find patters associated with Key Oppenent's 
  * Using Apriori Algorithm based association rules to identify the combination of players which have led to favorable results in the past 

###### Roma's performance and top opponents 

Roma has consistenly been a top-6 team in Serie A for many years now, occasionally finishing in top 2 as well. 2014 was a great year for the club where, the team won the Serie A, beating the opponents Juventus in a close 2 horse race. The figure below shows how Roma has performed over the 8 seasons in terms of match results. 

<img src = "images/roma_perf.png">






## Other use cases of the approach


We can combine this with further approaches like clustering to hone in onto the desired objective we want to achieve in any kind of data:

* Employee staffing data for Team Formation
* Market Basket Analysis for Retail industries


