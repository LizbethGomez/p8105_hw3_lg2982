hw3
================
Lizbeth Gomez
10/10/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
data("instacart")

#1: Description:
nrow(instacart)
```

    ## [1] 1384617

``` r
ncol(instacart)
```

    ## [1] 15

``` r
#2:How many aisles are there, and which aisles are the most items ordered from?

instacart%>%
  count(aisle)
```

    ## # A tibble: 134 x 2
    ##    aisle                      n
    ##    <chr>                  <int>
    ##  1 air fresheners candles  1067
    ##  2 asian foods             7007
    ##  3 baby accessories         306
    ##  4 baby bath body care      328
    ##  5 baby food formula      13198
    ##  6 bakery desserts         1501
    ##  7 baking ingredients     13088
    ##  8 baking supplies decor   1094
    ##  9 beauty                   287
    ## 10 beers coolers           1839
    ## # … with 124 more rows

``` r
instacart %>%
  count(aisle, name = "aisle_count") %>% 
  arrange(desc(aisle_count))
```

    ## # A tibble: 134 x 2
    ##    aisle                         aisle_count
    ##    <chr>                               <int>
    ##  1 fresh vegetables                   150609
    ##  2 fresh fruits                       150473
    ##  3 packaged vegetables fruits          78493
    ##  4 yogurt                              55240
    ##  5 packaged cheese                     41699
    ##  6 water seltzer sparkling water       36617
    ##  7 milk                                32644
    ##  8 chips pretzels                      31269
    ##  9 soy lactosefree                     26240
    ## 10 bread                               23635
    ## # … with 124 more rows

``` r
#3: Make a plot that shows the number of items ordered in each aisle, limiting this to aisles with more than 10000 items ordered. Arrange aisles sensibly, and organize your plot so others can read it.

instacart %>%
  count(aisle, name = "items_count") %>% 
  arrange(desc (items_count)) %>% 
  filter(items_count > 10000) %>% 
ggplot(aes(x = items_count, y = aisle)) + 
  geom_point()
```

![](Homework_3_files/figure-gfm/problem%201-1.png)<!-- -->

``` r
#4: Make a table showing the three most popular items in each of the aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”. Include the number of times each item is ordered in your table.


instacart %>% 
  filter(aisle %in% c( "dog food care", "baking ingredients", "packaged vegetables fruits")) %>% 
  count(aisle, product_name) %>% 
  group_by(aisle) %>% 
  top_n(3) %>% 
  knitr::kable()
```

    ## Selecting by n

| aisle                      | product\_name                                 |    n |
| :------------------------- | :-------------------------------------------- | ---: |
| baking ingredients         | Cane Sugar                                    |  336 |
| baking ingredients         | Light Brown Sugar                             |  499 |
| baking ingredients         | Pure Baking Soda                              |  387 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |
| dog food care              | Small Dog Biscuits                            |   26 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |

``` r
#5:Make a table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week; format this table for human readers (i.e. produce a 2 x 7 table)

instacart %>% 
  filter(product_name %in% c( "Pink Lady Apples", " Coffee Ice Cream")) %>% 
  group_by(order_dow) %>% 
  summarise(average_order_hour = mean(order_hour_of_day)) %>% 
  mutate(order_dow = recode(order_dow, `1` = 'Mon', `2` = 'Tues', `3` = 'Wed', `4` = 'Thur', `5` = 'Fri', `6` = 'Sat', `0` = 'Sun'))%>% 
  knitr::kable()
```

| order\_dow    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              average\_order\_hour |
| :------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| Sun           |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          13.44118 |
| Mon           |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          11.36000 |
| Tues          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          11.70213 |
| Wed           |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          14.25000 |
| Thur          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          11.55172 |
| Fri           |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          12.78431 |
| Sat           |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          11.93750 |
| \*Dataset des | cription:The dataset contains 1,384,617 observations of 15 variables, where each row in the dataset is a product from an order. There is a single order per user in this dataset, which means that every single row is an item purchased at one point by the user at a given time. This dataset contains variables describing the time of day of item purchase( “order\_hour\_of\_the\_day”), the online order identifier for the user (“order\_id”), whether this user had purchased the item at a previous time (“reordered”), etc. Illustrative <example:For> instance unser number 34, purchased organic whole milk, bananas, marinara pasta sauce, low fat yogurt and half and half. one thing is for sure, they’re not allergic to dairy.\* |

\#Answers: *1: There are 134 aisles in this dataset* *2: Most products
are purchased from the fresh fruit aisle and the packaged vegetable
fruits aisle* *3: see plot above* *4: See table above* *5: See table
above*

``` r
 data("brfss_smart2010")

#Data cleaning
brfss_smart2010 = 
  brfss_smart2010%>%
  janitor::clean_names() %>% 
  rename( state = locationabbr,county = locationdesc)%>%
  filter(topic == "Overall Health") %>% 
  filter(response %in% c("Excellent", "Very good", "Good", "Fair", "Poor")) %>% 
  mutate(response =fct_reorder(response, display_order))

head(brfss_smart2010)
```

    ## # A tibble: 6 x 23
    ##    year state county class topic question response sample_size data_value
    ##   <int> <chr> <chr>  <chr> <chr> <chr>    <fct>          <int>      <dbl>
    ## 1  2010 AL    AL - … Heal… Over… How is … Excelle…          94       18.9
    ## 2  2010 AL    AL - … Heal… Over… How is … Very go…         148       30  
    ## 3  2010 AL    AL - … Heal… Over… How is … Good             208       33.1
    ## 4  2010 AL    AL - … Heal… Over… How is … Fair             107       12.5
    ## 5  2010 AL    AL - … Heal… Over… How is … Poor              45        5.5
    ## 6  2010 AL    AL - … Heal… Over… How is … Excelle…          91       15.6
    ## # … with 14 more variables: confidence_limit_low <dbl>,
    ## #   confidence_limit_high <dbl>, display_order <int>,
    ## #   data_value_unit <chr>, data_value_type <chr>,
    ## #   data_value_footnote_symbol <chr>, data_value_footnote <chr>,
    ## #   data_source <chr>, class_id <chr>, topic_id <chr>, location_id <chr>,
    ## #   question_id <chr>, respid <chr>, geo_location <chr>

``` r
#In 2002, which states were observed at 7 or more locations? What about in 2010?

brfss_smart2010%>%
  filter(year ==2002) %>% 
  count(state) %>% 
  filter(n >= 7)
```

    ## # A tibble: 36 x 2
    ##    state     n
    ##    <chr> <int>
    ##  1 AZ       10
    ##  2 CO       20
    ##  3 CT       35
    ##  4 DE       15
    ##  5 FL       35
    ##  6 GA       15
    ##  7 HI       20
    ##  8 ID       10
    ##  9 IL       15
    ## 10 IN       10
    ## # … with 26 more rows

``` r
brfss_smart2010%>%
  filter(year ==2010) %>% 
  count(state) %>% 
  filter(n >= 7)  
```

    ## # A tibble: 45 x 2
    ##    state     n
    ##    <chr> <int>
    ##  1 AL       15
    ##  2 AR       15
    ##  3 AZ       15
    ##  4 CA       60
    ##  5 CO       35
    ##  6 CT       25
    ##  7 DE       15
    ##  8 FL      205
    ##  9 GA       20
    ## 10 HI       20
    ## # … with 35 more rows

``` r
#Construct a dataset that is limited to Excellent responses, and contains, year, state, and a variable that averages the data_value across locations within a state. Make a “spaghetti” plot of this average value over time within a state (that is, make a plot showing a line for each state across years – the geom_line geometry and group aesthetic will help).
#Make a two-panel plot showing, for the years 2006, and 2010, distribution of data_value for responses (“Poor” to “Excellent”) among locations in NY State
```
