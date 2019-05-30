Reproducing London Plan Annual Monitoring Report tables
================
James Gleeson
May 2019

### Introduction

The aim of this notebook is to show how to reproduce a few tables in the [London Plan Annual Monitoring Report](https://www.london.gov.uk/what-we-do/planning/implementing-london-plan/monitoring-london-plan) (AMR) from the London Development Database (LDD) data published on the [London Datastore](https://data.london.gov.uk/dataset/london-plan-amr14-tables-and-data), and along the way to show some simple techniques for wrangling data and producing tables. It is a work in progress and will be added to over time.

The LDD includes detailed information on every housing development in London, based on data provided to the Greater London Authority by London's boroughs and development corporations. The data we'll be analysing is an extract from the LDD that covers all homes completed in 2016/17. Note that LDD is frequently revised over time so future snapshopts of completions in 2016/17 may not be exactly the same.

### Setup

First let's load the packages we are going to use.

``` r
library(tidyverse) # various functions for working with data. Includes dplyr
library(rio) # reads Excel data from URLs
library(formattable)
library(janitor) # useful for some basic data cleaning 
```

Now let's load the data. We'll read in the 'Unit level' sheet only as that's what's used to generate the tables we're interested in.

``` r
unitc <- rio::import("https://data.london.gov.uk/download/london-plan-amr14-tables-and-data/ad929204-cbe9-4bb2-bed7-1c1d28d210c9/LDD%20-%20Housing%20Completions%20for%20AMR14.xlsx",
                   which = 2) %>% # import 'unit level' sheet
clean_names() # convert variable names to a common format
```

### Data prep

The LDD records the exact number of bedrooms each unit has, while in the AMR this is 'topcoded' so that all those with 4 bedrooms or more are counted together. Let's create a similar topcoded variable. Note, although the AMR uses '1 bed', '2 beds' for the sake of brevity, these refer to dwellings with 1 bed*room*, 2 bed*rooms* and so on.

``` r
unitc <- unitc %>%
  mutate(bedrooms = case_when(number_of_bedrooms == 1 ~ "1 bed",
                              number_of_bedrooms == 2 ~ "2 beds",
                              number_of_bedrooms == 3 ~ "3 beds",
                              number_of_bedrooms >= 4 ~ "4 beds or more"))
```

### Analysis

#### Table 3.8

Let's start by trying to recreate table 3.8 in the AMR, which shows the number of bedrooms by tenure. First, we need to reorder the tenure variable so that it matches the order used in the AMR.

``` r
unitc$unit_tenure <- ordered(unitc$unit_tenure,
                            levels = c("Social Rented", "Intermediate", "Affordable Rent", "Market"))
```

Now we can produce the table. The code below filters out any `NA` records and counts the number of 'proposed units' - i.e. gross completions without netting off demolition. This table uses gross rather than net completions because the the number of bedrooms in demolished units is not always recorded for planning purposes.

There are a couple of things I haven't yet worked out how to do with tables like this: how to change the 'Total' label and how to insert a thousands separator without also adding two decimal places.

``` r
unitc %>% 
  filter(!is.na(bedrooms)) %>% # filter out rows where the number of bedrooms is not available
  group_by(bedrooms, Tenure = unit_tenure) %>% # group the data by bedrooms and tenure
  tally(proposed_units) %>% # sum the proposed units in each group
  spread(bedrooms, n) %>% # turn the table into 'wide' or crosstab format
  adorn_totals(where = "row") %>% # add row totals
  format_table() # pass the result through `format_table` to improve its look
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Tenure
</th>
<th style="text-align:right;">
1 bed
</th>
<th style="text-align:right;">
2 beds
</th>
<th style="text-align:right;">
3 beds
</th>
<th style="text-align:right;">
4 beds or more
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Social Rented
</td>
<td style="text-align:right;">
1018
</td>
<td style="text-align:right;">
1120
</td>
<td style="text-align:right;">
814
</td>
<td style="text-align:right;">
283
</td>
</tr>
<tr>
<td style="text-align:right;">
Intermediate
</td>
<td style="text-align:right;">
1151
</td>
<td style="text-align:right;">
1373
</td>
<td style="text-align:right;">
409
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:right;">
Affordable Rent
</td>
<td style="text-align:right;">
586
</td>
<td style="text-align:right;">
840
</td>
<td style="text-align:right;">
481
</td>
<td style="text-align:right;">
213
</td>
</tr>
<tr>
<td style="text-align:right;">
Market
</td>
<td style="text-align:right;">
16162
</td>
<td style="text-align:right;">
14446
</td>
<td style="text-align:right;">
4937
</td>
<td style="text-align:right;">
1879
</td>
</tr>
<tr>
<td style="text-align:right;">
Total
</td>
<td style="text-align:right;">
18917
</td>
<td style="text-align:right;">
17779
</td>
<td style="text-align:right;">
6641
</td>
<td style="text-align:right;">
2415
</td>
</tr>
</tbody>
</table>
#### Table 3.9

This table shows Gross conventional completions by bedrooms and borough. The code below shows how to mix integer and percent formats in one table using `format_table`. Again I'm not sure how to reduce the number of decimal places in the percent column.

``` r
unitc %>% 
  filter(!is.na(bedrooms)) %>%
  group_by(bedrooms, Borough = borough) %>%
  tally(proposed_units) %>%
  spread(bedrooms, n, fill = 0) %>% # need to specify blanks filled with zero rather than NA
  adorn_totals(where = c("row", "col")) %>%
  mutate(`% 3 or more` = ((`3 beds` + `4 beds or more`)/`Total`)) %>%
  format_table(list(area(col = 7) ~ percent)) 
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Borough
</th>
<th style="text-align:right;">
1 bed
</th>
<th style="text-align:right;">
2 beds
</th>
<th style="text-align:right;">
3 beds
</th>
<th style="text-align:right;">
4 beds or more
</th>
<th style="text-align:right;">
Total
</th>
<th style="text-align:right;">
% 3 or more
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Barking and Dagenham
</td>
<td style="text-align:right;">
139
</td>
<td style="text-align:right;">
241
</td>
<td style="text-align:right;">
168
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
601
</td>
<td style="text-align:right;">
36.77%
</td>
</tr>
<tr>
<td style="text-align:right;">
Barnet
</td>
<td style="text-align:right;">
704
</td>
<td style="text-align:right;">
1145
</td>
<td style="text-align:right;">
436
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
2384
</td>
<td style="text-align:right;">
22.44%
</td>
</tr>
<tr>
<td style="text-align:right;">
Bexley
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:right;">
355
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
176
</td>
<td style="text-align:right;">
841
</td>
<td style="text-align:right;">
38.88%
</td>
</tr>
<tr>
<td style="text-align:right;">
Brent
</td>
<td style="text-align:right;">
654
</td>
<td style="text-align:right;">
653
</td>
<td style="text-align:right;">
148
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1478
</td>
<td style="text-align:right;">
11.57%
</td>
</tr>
<tr>
<td style="text-align:right;">
Bromley
</td>
<td style="text-align:right;">
428
</td>
<td style="text-align:right;">
391
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:right;">
1035
</td>
<td style="text-align:right;">
20.87%
</td>
</tr>
<tr>
<td style="text-align:right;">
Camden
</td>
<td style="text-align:right;">
601
</td>
<td style="text-align:right;">
534
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
1307
</td>
<td style="text-align:right;">
13.16%
</td>
</tr>
<tr>
<td style="text-align:right;">
City of London
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.00%
</td>
</tr>
<tr>
<td style="text-align:right;">
Croydon
</td>
<td style="text-align:right;">
1627
</td>
<td style="text-align:right;">
878
</td>
<td style="text-align:right;">
408
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
2979
</td>
<td style="text-align:right;">
15.91%
</td>
</tr>
<tr>
<td style="text-align:right;">
Ealing
</td>
<td style="text-align:right;">
652
</td>
<td style="text-align:right;">
645
</td>
<td style="text-align:right;">
205
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
1566
</td>
<td style="text-align:right;">
17.18%
</td>
</tr>
<tr>
<td style="text-align:right;">
Enfield
</td>
<td style="text-align:right;">
462
</td>
<td style="text-align:right;">
289
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
1022
</td>
<td style="text-align:right;">
26.52%
</td>
</tr>
<tr>
<td style="text-align:right;">
Greenwich
</td>
<td style="text-align:right;">
1444
</td>
<td style="text-align:right;">
765
</td>
<td style="text-align:right;">
297
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
2536
</td>
<td style="text-align:right;">
12.89%
</td>
</tr>
<tr>
<td style="text-align:right;">
Hackney
</td>
<td style="text-align:right;">
494
</td>
<td style="text-align:right;">
505
</td>
<td style="text-align:right;">
282
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
1370
</td>
<td style="text-align:right;">
27.08%
</td>
</tr>
<tr>
<td style="text-align:right;">
Hammersmith and Fulham
</td>
<td style="text-align:right;">
531
</td>
<td style="text-align:right;">
480
</td>
<td style="text-align:right;">
158
</td>
<td style="text-align:right;">
161
</td>
<td style="text-align:right;">
1330
</td>
<td style="text-align:right;">
23.98%
</td>
</tr>
<tr>
<td style="text-align:right;">
Haringey
</td>
<td style="text-align:right;">
468
</td>
<td style="text-align:right;">
330
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
949
</td>
<td style="text-align:right;">
15.91%
</td>
</tr>
<tr>
<td style="text-align:right;">
Harrow
</td>
<td style="text-align:right;">
259
</td>
<td style="text-align:right;">
314
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
726
</td>
<td style="text-align:right;">
21.07%
</td>
</tr>
<tr>
<td style="text-align:right;">
Havering
</td>
<td style="text-align:right;">
157
</td>
<td style="text-align:right;">
206
</td>
<td style="text-align:right;">
190
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
677
</td>
<td style="text-align:right;">
46.38%
</td>
</tr>
<tr>
<td style="text-align:right;">
Hillingdon
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
318
</td>
<td style="text-align:right;">
65
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
897
</td>
<td style="text-align:right;">
20.40%
</td>
</tr>
<tr>
<td style="text-align:right;">
Hounslow
</td>
<td style="text-align:right;">
696
</td>
<td style="text-align:right;">
362
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1182
</td>
<td style="text-align:right;">
10.49%
</td>
</tr>
<tr>
<td style="text-align:right;">
Islington
</td>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
301
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
755
</td>
<td style="text-align:right;">
15.76%
</td>
</tr>
<tr>
<td style="text-align:right;">
Kensington and Chelsea
</td>
<td style="text-align:right;">
156
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
395
</td>
<td style="text-align:right;">
38.99%
</td>
</tr>
<tr>
<td style="text-align:right;">
Kingston upon Thames
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
305
</td>
<td style="text-align:right;">
21.31%
</td>
</tr>
<tr>
<td style="text-align:right;">
Lambeth
</td>
<td style="text-align:right;">
790
</td>
<td style="text-align:right;">
571
</td>
<td style="text-align:right;">
198
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
1617
</td>
<td style="text-align:right;">
15.83%
</td>
</tr>
<tr>
<td style="text-align:right;">
Lewisham
</td>
<td style="text-align:right;">
604
</td>
<td style="text-align:right;">
794
</td>
<td style="text-align:right;">
216
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
1656
</td>
<td style="text-align:right;">
15.58%
</td>
</tr>
<tr>
<td style="text-align:right;">
Merton
</td>
<td style="text-align:right;">
246
</td>
<td style="text-align:right;">
176
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
531
</td>
<td style="text-align:right;">
20.53%
</td>
</tr>
<tr>
<td style="text-align:right;">
Newham
</td>
<td style="text-align:right;">
1095
</td>
<td style="text-align:right;">
1058
</td>
<td style="text-align:right;">
387
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
2594
</td>
<td style="text-align:right;">
17.00%
</td>
</tr>
<tr>
<td style="text-align:right;">
Redbridge
</td>
<td style="text-align:right;">
403
</td>
<td style="text-align:right;">
277
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
821
</td>
<td style="text-align:right;">
17.17%
</td>
</tr>
<tr>
<td style="text-align:right;">
Richmond upon Thames
</td>
<td style="text-align:right;">
203
</td>
<td style="text-align:right;">
240
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
537
</td>
<td style="text-align:right;">
17.50%
</td>
</tr>
<tr>
<td style="text-align:right;">
Southwark
</td>
<td style="text-align:right;">
901
</td>
<td style="text-align:right;">
1136
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
2529
</td>
<td style="text-align:right;">
19.45%
</td>
</tr>
<tr>
<td style="text-align:right;">
Sutton
</td>
<td style="text-align:right;">
379
</td>
<td style="text-align:right;">
299
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:right;">
6.22%
</td>
</tr>
<tr>
<td style="text-align:right;">
Tower Hamlets
</td>
<td style="text-align:right;">
2159
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
832
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
5124
</td>
<td style="text-align:right;">
19.57%
</td>
</tr>
<tr>
<td style="text-align:right;">
Waltham Forest
</td>
<td style="text-align:right;">
377
</td>
<td style="text-align:right;">
476
</td>
<td style="text-align:right;">
186
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
1108
</td>
<td style="text-align:right;">
23.01%
</td>
</tr>
<tr>
<td style="text-align:right;">
Wandsworth
</td>
<td style="text-align:right;">
755
</td>
<td style="text-align:right;">
1282
</td>
<td style="text-align:right;">
301
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:right;">
2461
</td>
<td style="text-align:right;">
17.23%
</td>
</tr>
<tr>
<td style="text-align:right;">
Westminster
</td>
<td style="text-align:right;">
505
</td>
<td style="text-align:right;">
597
</td>
<td style="text-align:right;">
476
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
1704
</td>
<td style="text-align:right;">
35.33%
</td>
</tr>
<tr>
<td style="text-align:right;">
Total
</td>
<td style="text-align:right;">
18917
</td>
<td style="text-align:right;">
17779
</td>
<td style="text-align:right;">
6641
</td>
<td style="text-align:right;">
2415
</td>
<td style="text-align:right;">
45752
</td>
<td style="text-align:right;">
19.79%
</td>
</tr>
</tbody>
</table>
