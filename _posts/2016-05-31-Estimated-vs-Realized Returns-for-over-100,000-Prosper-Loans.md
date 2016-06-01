---
layout: post
title: "Estimated vs Realized Returns for over 100,000 Prosper loans"
date: 2016-5-31
output:
  html_document
share: true
categories: blog
excerpt: "Does Prosper over-promise its returns?"
tags: [p2p, R, Prosper]
---


**Introduction**
-----
I recently opened a investor account through Prosper, and I was a bit surprised by the relatively high return estimates that they provide for their loans. As of May 31, 2016, they show a 6.81% estimated weighted average return on all their loans (the AA rating notes have 4.38% return and the HR loans have 11.13%). For a fixed income investment that has an active secondary market and offers some degree of diversification, this seems like a decent investment opportunity. Nonetheless, I wanted to find out for myself how well does the estimated return matches with the actual historical return on the loans that they offer. Ultimately, Prosper is a financial institution that tries to sell its platform to investors, so it is not immune to over-promising ([remember the CDOs marketed in 2005?](https://www.youtube.com/watch?v=3hG4X5iTK8M)). Hence, I expected to see a small divergence between estimated and realized returns, with the former offering a few basis points higher return. 

When you click on a listing that you want to invest in, you usually see this:

![](https://raw.githubusercontent.com/stasSajin/stasSajin.github.io/master/figs/2016-05-31-Estimated-vs-Realized%20Returns-for-over-100%2C000-Prosper-Loans/ProsperSample.PNG)

There is only one number we care about, which is the estimated return on the loan. Now, it's important to point out that the estimated return that Prosper provides is based on historical data for loans with similar characteristics as the one you see above. In other words, the estimated return is not the return of this particular loan, but for the universe of similar type of loans that have been issued in the past.

So how is this return calculated? I'll go step by step through all assumptions that are listed on Prosper website:

1. **Seasoned Return**: the estimated return is based on seasoned notes that have matured for at least 10 months. As Prosper indicates on their website, the returns have increased stability after a 10-month period. This is likely because bad borrowers are likely to default relatively early, leading to higher volatility and an isomorphic hazard curve. 
    
2. **Effective Yield**: this includes the borrower's interest rate, minus the servicing fees, minus estimated loss of interest from chargeoffs, plus late-fees, and estimated principal recovery from chargeoffs.

3. **Effective Loss**: the estimated amount of principal that would be lost due to defaults and chargeoffs on this universe of loans.

4. **Estimated return**: represents the difference between effective yield and effective loss.

To get the data, I downloaded the listing and the loan data from Prosper for the years 2005-2016. You'll have to open an account with them if you want to be able to download the data.  

**Libraries**

{% highlight text %}
 data.table     ggplot2   rmarkdown       dplyr    ggthemes       Hmisc 
       TRUE        TRUE        TRUE        TRUE        TRUE        TRUE 
  lubridate highcharter 
       TRUE        TRUE 
{% endhighlight %}

In the code below, I'll be reading the listing and the loans data. 

{% highlight r %}
# listing data file names
tempListLoan = list.files(pattern = "*listings.csv")
# loan data file names
tempLoan = list.files(pattern = "*loans.csv")

# read and combine the listing csv files
prosperListings <- rbindlist(lapply(tempListLoan, 
		function(file) fread(file, 
    		na.strings = c("NA", ""))))

# read and combine the loans csv files
prosperLoans <- rbindlist(lapply(tempLoan, 
		function(file) fread(file, na.strings = c("NA", 
    		"", "N/A"))))

# for listings
dim(prosperListings)

# for loans
dim(prosperLoans)
{% endhighlight %}

Right from the start, the data presents us with some challenges. The listing data has information about estimated returns for a loan, while the loans data has information that would help us calculate the realized returns. We need to merge these two data sets together with a full join function. This is not the challenge. The challenge is that there is no unique key number for each loan across the two dataframes. In other words, we don't know how do loans in the loan listings dataframe corresponds to the funded loans in the loans dataframe. Without a unique identifier, this whole endeavor might seem like a lost cause. 

Fortunately, we can try to match our loans on other variable names. Several columns in the listing dataframe (`loan_origination_date`,  `ammount_funded`, `prosper_rating`, `borrower_rate`, `listing_term`) are also present in the loan dataframe (`origination_date`, `ammount_borrowed`, `prosper_rating`, `borrower_rate`, `term`). If we match rows across two dataframes based on these columns, we should be able to get a dataset that contains both the estimated return and all the variables needed to calculate the realized return.


{% highlight r %}
# removelistings that don't have a loan_origination_date
listingsOriginated <- prosperListings %>% filter(!is.na(loan_origination_date))
rm(prosperListings)

# rename listing columns
listingsOriginated <- rename(listingsOriginated, 
		origination_date = loan_origination_date, 
    	term = listing_term, amount_borrowed = amount_funded)

intersect(names(listingsOriginated), names(prosperLoans))
{% endhighlight %}


{% highlight r %}
# there is also mismatch in variable class for for the two datasets, so we
# need to also change variable class in the prosperLoans data to match that
# of the listing data

listingsOriginated %>% select(origination_date, term, amount_borrowed, prosper_rating, 
    borrower_rate) %>% sapply(class)
{% endhighlight %}


{% highlight r %}
prosperLoans %>% select(origination_date, term, amount_borrowed, prosper_rating, 
    borrower_rate) %>% sapply(class)
{% endhighlight %}


{% highlight r %}
prosperLoans$term <- as.integer(prosperLoans$term)
prosperLoans$amount_borrowed <- as.numeric(prosperLoans$amount_borrowed)
prosperLoans$borrower_rate <- as.numeric(prosperLoans$borrower_rate)

# change the date format for the listingOriginated and prosperLoans
listingsOriginated$origination_date <- mdy_hm(listingsOriginated$origination_date)
prosperLoans$origination_date <- ymd_hms(prosperLoans$origination_date)

# Now we have 5 colums on which we can join the two dataframes.
noDuplicates <- subset(listingsOriginated, 
	!duplicated(subset(listingsOriginated, 
    select = c(origination_date, term, prosper_rating, 
	amount_borrowed, borrower_rate))))

combinedData <- inner_join(prosperLoans, noDuplicates, 
	by = intersect(names(prosperLoans), 
    names(noDuplicates)))

rm(listingsOriginated)
rm(prosperLoans)
rm(noDuplicates)
dim(combinedData)
{% endhighlight %}


{% highlight text %}
[1] 542872     88
{% endhighlight %}

We have a total of 542872 loans and 88 variables after performing all the join operations. 

I'll remove all the loans before September 2009. The reason is because Prosper has undergone some major changes in their underwriting process when they resumed their operations after the ["Quiet Period"](http://techcrunch.com/2008/11/26/sec-outlines-its-reasoning-for-shutting-down-p2p-lender-prosper/)

Lastly, I'll remove the loans that are still current. 


{% highlight r %}
# Filter the loans that are listed as currently paying (there are quite a
# lot of loans with that status). Also filter out loans since Sep, 2009.
combinedData$origination_date <- as.Date(combinedData$origination_date)
combinedData <- combinedData %>%
	filter(origination_date >= as.Date("2009-09-01") & 
    origination_date <= as.Date("2015-12-31") & 
	loan_status_description != "CURRENT")
{% endhighlight %}

That leaves us with about 140,000 loans that we can explore. 

**Return Calculations**
------
I'll extract only variables that are of interest to calculating realized returns. 

{% highlight r %}
returnData <- combinedData %>% select(amount_borrowed, borrower_rate, prosper_rating, 
    term, origination_date, loan_number, age_in_months, principal_balance, service_fees_paid, 
    principal_paid, interest_paid, prosper_fees_paid, late_fees_paid, debt_sale_proceeds_received, 
    loan_status_description, next_payment_due_date, next_payment_due_amount, 
    estimated_return, estimated_loss_rate)
{% endhighlight %}



I calculate the cumulative return as follows:

$$r_c=(PR+I+LF-SF-P)/P$$

Where `PR` represents the principal repaid, `I` is the interest repaid, `LF` is the late fees, `SF` are the service fees, and `P` is the principal.

After finding the cumulative return, I annualized it using this formula:

$$r_a=(1+r_c)^{12/LoanAge} $$



{% highlight r %}
returnData <- returnData %>% mutate(cummulativeReturn = (principal_paid + interest_paid + 
    prosper_fees_paid + late_fees_paid + service_fees_paid - amount_borrowed)/amount_borrowed) %>% 
    mutate(AnnualizedReturn = ((1 + cummulativeReturn)^(12/age_in_months)) - 
        1)
{% endhighlight %}

**Plots**
----
Next, I'll plot the daily returns.

{% highlight r %}
dataPlot1 <- returnData %>% select(loan_status_description, origination_date, 
    age_in_months, estimated_return, AnnualizedReturn) %>% filter(!is.na(estimated_return) & 
    loan_status_description == "COMPLETED")

dataPlot2 <- dataPlot1 %>% group_by(origination_date) %>% summarise(Estimated = mean(estimated_return), 
    Realized = mean(AnnualizedReturn)) %>% arrange(origination_date)

highchart() %>% hc_title(text = "Prosper Estimated vs. Realized Daily Returns") %>% 
    hc_subtitle(text = "This graph is based on the dataset provided on Prosper website") %>% 
    hc_tooltip(valueDecimals = 2) %>% hc_add_serie_times_values(as.Date(dataPlot2$origination_date), 
    dataPlot2$Estimated, name = "Mean Estimated Return") %>% hc_add_series_times_values(as.Date(dataPlot2$origination_date), 
    dataPlot2$Realized, name = "Mean Realized Return") %>% hc_add_theme(hc_theme_db())
{% endhighlight %}

<iframe src="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperDaily.html" width="800" height="550" frameBorder="0"></iframe> <a href="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperDaily.html" target="_blank">open</a>


Several things become very apparent:

1. The estimated returns provided by prosper are higher than the realized daily returns. 

2. The two returns tend to converge over time. 

3. The 2015-2016 period shows higher volatility in realized returns. This is because most loans are still not matured, so the daily return averages are calculated over fewer loans.
    
    
I was really surprized by these findings, since the chart above suggests that Prosper has been providing over-optimistic estimates. I tried to perform the same analyses as above on a different [Prosper dataset](https://docs.google.com/document/d/1qEcwltBMlRYZT-l699-71TzInWfk4W9q5rTCSvDVMpc/pub?embedded=true), with loans up to 03/11/2014, nontheless the results were the same. You can see the D3 graph below.

<iframe src="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperOther.html" width="880" height="1050" frameBorder="0"></iframe> <a href="/htmlwidgets/estimated-vs-realized-prosper-returns/prosperOther.html" target="_blank">open</a>


