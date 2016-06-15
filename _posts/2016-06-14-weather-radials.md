---
layout: post
title: "Imagine Hell--then make it cold"
date: 2016-6-14
output:
  html_document
share: true
categories: blog
excerpt: "Plotting weather radials"
tags: [weather radials, R, ggplot2, data viz]
---




# Introduction
Oscar Wilde once said that "conversations about the weather are the last refuge of the unimaginative." After living in so many places, I came to realize that conversing about the weather speaks not so much about how dull we are, but rather reveals more about how famously unpredictable and abysmal the local weather can be. Locals in SF and Sacramento rarely talk about the weather, since there is little variability to it. On the other hand, in New York, the weather can be extremely bipolar and unpleasant. It is unsuprising then that weather for New Yorkers is a big conversation starter, since it allows people to share in their misery regaridng the fridig cold or the unending rain. For instance, the first entry when you do a search for `Binghamton`, a town in NY state where I did my graduate work, in [Urban Dictionary](http://www.urbandictionary.com/define.php?term=Binghamton&utm_source=search-action) is this one:

> **Imagine Hell, then make it cold.**

I've been a big admirerer of design principles behind weather radials, so I decided to recreate some of the charts found on [weather-radials.com](http://weather-radials.com/) using R and ggplot2. I used the `weatherData` package, which has some very easy to use wraper functions for downloading the weather data from [Weather Underground](https://www.wunderground.com/)



{% highlight r %}
library(weatherData) #for downloading the weather data
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(highcharter) #for datetime_to_timestamp function
{% endhighlight %}

First, we're going to download the daily temperature data for year 2015. 


{% highlight r %}
bing <- getWeatherForDate("BGM", "2015-01-01","2015-12-31", 
                          opt_all_columns = TRUE)
sac <- getWeatherForDate("SAC", "2015-01-01","2015-12-31",
                         opt_all_columns = TRUE)


#create a column for city
bing$city<-"Binghamton"
sac$city<-"Sacramento"

#remove EST and PST colums
bing<-bing %>% select(-EST)
sac<-sac %>% select(-PST)

#combine the two dfs
weather_Data<-rbind(bing,sac)
{% endhighlight %}

Do some data pre-processing.


{% highlight r %}
#create weather vars
weather_Data <- weather_Data %>% 
  mutate(date2 = as.Date(ymd(Date))) 

#replacing the T and 0 values for precipitation with NA and changing it to numeric
weather_Data$PrecipitationIn[weather_Data$PrecipitationIn == "T"] <- NA
weather_Data$PrecipitationIn[weather_Data$PrecipitationIn == "0"] <- NA
weather_Data$PrecipitationIn<-as.numeric(weather_Data$PrecipitationIn)
{% endhighlight %}

# The Result


{% highlight r %}
#plot radial
weather_Data$PrecipitationIn[weather_Data$PrecipitationIn == "0"] <- NA
ggplot()+
    geom_line(data=weather_Data, 
              aes(x=date2, y=Mean_TemperatureF, color=Mean_TemperatureF),
              size=2)+
    scale_color_viridis(option = "B", limits=c(0, 100), 
              guide = guide_colourbar(title = "Temp (F)")) +
    geom_point(data=weather_Data,
               aes(x=date2, y=90, size=PrecipitationIn),
               alpha= .25)+
    scale_x_date(labels = date_format("%b"), breaks = date_breaks("month")) +
    scale_size_area(max_size = 10, breaks = seq(0,3,.5),
                    guide = guide_legend(title = "Precipitation (inches)")) +
    coord_polar() +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0,100,20),
      labels=sprintf("%sF", seq(0, 100, by=20)),limits=c(0,120))+
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0, size=10),
          plot.caption=element_text(hjust = 0, size=8))+
    labs(title = "Binghamton (NY) and Sacramento (CA) Weather Radials (2015)",
       subtitle = "The area of the inner most circles contain the temperatures from 0-20 F and consecutively larger circles contain an additional 20F. \nThe brightly colored lines represent the daily mean temperature for each location",
       x = NULL, y = NULL) +
    facet_wrap(~city)+labs(caption = "Note that Sacramento recieves very little precipitation, while Binghamton has pretty much all year-round rainy/snowy weather.")
{% endhighlight %}

![center](/figs/2016-06-14-weather-radials/unnamed-chunk-4-1.png)

You can note that Sacramento barely gets any rain and that Binghamton had about a 6-month long winter last year, with very large intra-day temperature spikes.

If you're interested in creating animated radials, I recommend that you check the [`highcharter`](http://jkunst.com/highcharter/showcase.html) package developed by Joshua Kunst.

----

Despite the unpredictable weather and low temperatures, it is suprising to know that bad weather is generally associted with better productivity and increased performance at cognitive tasks that require focused attention (see [Lee and colleagues, 2014](http://scholar.harvard.edu/files/jooajulialee/files/jap_final_rainmakers_2014-01192-001.pdf?m=1400193142)). 





