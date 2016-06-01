---
layout: post
title: "Estimated vs Realized Returns for over 100,000 Prosper loans"
date: 2016-5-31
output:
  html_document
share: true
categories: blog
tags: [p2p, R, Prosper]
---



## Introduction
I recently opened a investor account through Prosper, and I was a bit surprised by the relatively high return estimates that they provide for their loans. As of May 31, 2016, they show a 6.81% estimated weighted average return on all their loans (the AA rating notes have 4.38% return and the HR loans have 11.13%). For a fixed income investment that has an active secondary market and offers some degree of diversification, this seems like a good investment opportunity. Nonetheless, I wanted to find out for myself how well does the estimated return matches with the actual historical return on the loans that they offer. Ultimately, Prosper is a financial institution that tries to sell its platform to investors, so it is not immune to over-promising ((remember the CDOs marketed in 2005?)[https://www.youtube.com/watch?v=3hG4X5iTK8M]). Hence, I expected to see a small divergence between estimated and realized returns, with the former offering a few basis points higher return. 

When you click on a listing that you want to invest in, you usually see this:

![center](/figs/2016-05-31-Estimated-vs-Realized-Prosper-Loans/ProsperSample.png)

There is only one number we care about, which is the estimated return on the loan. Now, it's important to point out that the estimated return that Prosper provides is based on historical data for loans with similar characteristics as the one you see above. In other words, the estimated return is not the return of this particular loan, but for the universe of similar type of loans that have been issued in the past.

So how is this return calculated? I'll go step by step through all assumptions that are listed on prosper website:
    - **Seasoned Return**: the estimated return is based on seasoned notes that have matured for at least 10 months. As Prosper indicates on their website, the returns have increased stability after a 10-month period. This is likely because bad borrowers are likely to default relatively early, leading to higher volatility and an isomorphic hazard curve. 
    - **Effective Yield**: this includes the borrower's interest rate, minus the servicing fees, minus estimated loss of interest from charge-offs, plus late-fees, and estimated principal recovery from charge-offs.
    -**Effective Loss**: the estimated amount of principal that would be lost due to defaults and chargeoffs on this universe of loans.
    -**Estimated return**: represents the difference between effective yield and effective loss.

To get the data, I downloaded the listing and the loan data from Prosper for the years 2005-2016. You'll have to open an account with them if you want to be able to download the data.  

###Libraries

{% highlight r %}
# load these libraries
pacman::p_load(data.table, ggplot2, rmarkdown, dplyr, ggthemes, Hmisc, lubridate, 
    highcharter)

# check that all packages loaded sucessfully
pacman::p_loaded(data.table, ggplot2, rmarkdown, dplyr, ggthemes, Hmisc, lubridate, 
    highcharter)
{% endhighlight %}



{% highlight text %}
 data.table     ggplot2   rmarkdown       dplyr    ggthemes       Hmisc 
       TRUE        TRUE        TRUE        TRUE        TRUE        TRUE 
  lubridate highcharter 
       TRUE        TRUE 
{% endhighlight %}

In the code below, I'll be reading the listing and the loans data. 


{% highlight text %}
Read 20.2% of 444782 rowsRead 33.7% of 444782 rowsRead 45.0% of 444782 rowsRead 58.5% of 444782 rowsRead 74.2% of 444782 rowsRead 87.7% of 444782 rowsRead 98.9% of 444782 rowsRead 444782 rows and 72 (of 72) columns from 0.132 GB file in 00:00:09
Read 41.7% of 191988 rowsRead 67.7% of 191988 rowsRead 93.8% of 191988 rowsRead 191988 rows and 72 (of 72) columns from 0.067 GB file in 00:00:05
Read 18.8% of 373305 rowsRead 32.1% of 373305 rowsRead 45.5% of 373305 rowsRead 56.3% of 373305 rowsRead 67.0% of 373305 rowsRead 77.7% of 373305 rowsRead 88.4% of 373305 rowsRead 99.1% of 373305 rowsRead 373305 rows and 72 (of 72) columns from 0.129 GB file in 00:00:10
Read 85.3% of 93797 rowsRead 93797 rows and 72 (of 72) columns from 0.033 GB file in 00:00:03
{% endhighlight %}



{% highlight text %}
[1] 1160403      72
{% endhighlight %}



{% highlight text %}
[1] 558157     21
{% endhighlight %}

Right from the start, the data presents us with some challenges. The listing data has information about estimated returns for a loan, while the loans data has information that would help us calculate the realized returns. We need to merge these two data sets together with a full join function. This is not the challenge. The challenge is that there is no unique key number for each loan across the two dataframes. In other words, we don't know how do loans in the loan listings dataframe corresponds to the funded loans in the loans dataframe. Without a unique identifier, this whole endeavor might seem like a lost cause. 

Fortunately, we can try to match our loans on other variable names. Several columns in the listing dataframe (`loan_origination_date`, `ammount_funded`,`prosper_rating`,`borrower_rate`,`listing_term`) are also present in the loan dataframe (`origination_date`,`ammount_borrowed`,`prosper_rating`,`borrower_rate`,`term`). If we match rows across two dataframes based on these columns, we should be able to get a dataset that contains both the estimated return and all the variables needed to calculate the realized return.



{% highlight text %}
[1] "origination_date" "amount_borrowed"  "prosper_rating"  
[4] "borrower_rate"    "term"            
{% endhighlight %}



{% highlight text %}
origination_date             term  amount_borrowed   prosper_rating 
     "character"        "integer"        "numeric"      "character" 
   borrower_rate 
       "numeric" 
{% endhighlight %}



{% highlight text %}
origination_date             term  amount_borrowed   prosper_rating 
     "character"      "character"      "character"      "character" 
   borrower_rate 
     "character" 
{% endhighlight %}



{% highlight text %}
[1] 542872     88
{% endhighlight %}

We have a total of 542872 loans and 88 variables after performing all the join operations. 

I'll remove all the loans before September 2009. The reason is because Prosper has undergone some major changes in their underwriting process when they resumed their operations after the ("Quiet Period")[http://techcrunch.com/2008/11/26/sec-outlines-its-reasoning-for-shutting-down-p2p-lender-prosper/] 

Lastly, I'll remove the loans that are still current. 



That leaves us with about 140,000 loans that we can explore. 

I'll extract only variables that are of interest to calculating realized returns. 



###Return Calculations


### Return Calculations
I calculate the cumulative return as follows:

$$r_c=(PR+I+LF-SF-P)/P$$

Where `PR` represents the principal repaid, `I` is the interest repaid, `LF` is the late fees, `SF` are the service fees, and `P` is the principal.

After finding the cumulative return, I annualized it using this formula:

$$r_a=(1+r_c)^{12/LoanAge} $$





Next, I'll plot the daily returns.

![center](/figs/2016-05-31-Estimated-vs-Realized Returns-for-over-100,000-Prosper-Loans/unnamed-chunk-7-1.png)

<iframe src="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperDaily.html"></iframe> <a href="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperDaily.html" target="_blank">open</a>


Several things become very apparent:
    1. The estimated returns provided by prosper are higher than the realized daily returns. 
    2. The two returns tend to converge over time. 
    3. The 2015-2016 period shows higher volatility in realized returns. This is because most loans are still not matured, so the daily return averages are calculated over fewer loans.
    
    
I was really surprized by these findings, since the chart above suggests that Prosper has been providing over-optimistic estimates. I tried to perform the same analyses as above on a different (Prosper dataset)[https://docs.google.com/document/d/1qEcwltBMlRYZT-l699-71TzInWfk4W9q5rTCSvDVMpc/pub?embedded=true], with loans up to 03/11/2014, nontheless the results were the same. You can see the D3 graph below.

<iframe src="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperOther.html"></iframe> <a href="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperOther.html" target="_blank">open</a>


