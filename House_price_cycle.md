Animating the English house price ripple effect
================
James Gleeson
May 2019

This notebook is aimed at creating an animated plot of England's house price cycle, using data from Land Registry's [UK House Price Index](https://www.gov.uk/government/statistical-data-sets/uk-house-price-index-data-downloads-september-2018#download-the-data).

Broadly speaking, local house prices in England seem to follow a cyclical 'ripple' pattern, with the first phase showing rapid growth in the most expensive areas (generally in London or nearby), followed by a phase in which mid-priced areas grow most quickly, followed by a final phase in which prices grow fastest in the cheapest areas. This notebook shows how to illustrate this ripple pattern with an animated chart.

Thanks to [Thomas Lin Pederson](https://github.com/thomasp85) for the amazing `gganimate` [package](https://gganimate.com/), and to [Hadley Wickham](https://github.com/hadley) and other contributors for the `tidyverse`.

### Setup

The first step is to load the packages we'll be using (you'll need to install them if you haven't already done so). You may also need to install the [`gifski`](https://cran.r-project.org/web/packages/gifski/index.html) and [`png`](https://cran.r-project.org/web/packages/png/index.html) packages to produce the final animation.

``` r
library(tidyverse) # to prepare data
library(gganimate) # to animate charts
library(transformr) # to help out with animation
```

### Data import

Now let's download our data from the Land Registry's UKHPI pages.

``` r
hpi <- read_csv("http://publicdata.landregistry.gov.uk/market-trend-data/house-price-index-data/Average-prices-2018-09.csv?utm_medium=GOV.UK&utm_source=datadownload&utm_campaign=average_price&utm_term=9.30_14_11_18")
```

### Data prep

We only want data for local authorities, not countries, regions or counties. So let's filter out any areas with a code that's 'greater than' E09. We also won't be using the `Average_Price_SA` column so let's get rid of that. You could apply these changes to the imported 'hpi' object but I've created a new 'hpif' so you can always revert to the original if you need to.

``` r
hpif <- filter(hpi, Area_Code < "E10") %>%
  select(-ends_with("_SA"))
```

In order to be able to compare recent price changes with previous prices, we need to create a lagged price variable. In order to reduce volatility, we're going to calculate price growth over three years, so we need to lag prices by three years, or 36 months.

``` r
hpif <- hpif %>%
  arrange(Area_Code, Date) %>%
  group_by(Area_Code) %>%
  mutate(lag_price = lag(Average_Price, 36))
```

Now let's calculate the average annual price change over three years, i.e. the total percentage price change divided by three.

``` r
hpif <- hpif %>%
  mutate(Y3_price_change = (Average_Price/lag_price -1)/3)
```

And then create a lagged price *rank* variable, as our animation will organise local authorities from least to most expensive.

``` r
hpif <- hpif %>%
  group_by(Date) %>%
  mutate(lag_price_rank = dense_rank(desc(lag_price)))
```

We can now filter out cases in the first three years (i.e. where the three year price change is NA), as well as filtering out the City of London as it's so volatile due to small numbers of sales.

``` r
hpif <- hpif %>%
  filter(!is.na(Y3_price_change)) %>%
  filter(Region_Name != "City of London") %>%
  group_by(Date)
```

Let's also join on the region that each local authority is in. For this we download a lookup file from ONS and join it on using the area code.

``` r
lookup <- rio::import("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/userinformationenglandandwaleslocalauthoritytoregionlookup/june2017/lookuplasregionew2017.xlsx", skip=4)
hpif <- left_join(hpif, lookup, by=c("Area_Code" = "LA code"))
names(hpif) <- make.names(names(hpif))
```

One of the region names is quite long so let's change it.

``` r
hpif$Region.name[hpif$Region.name =="Yorkshire and The Humber"] <- "Yorks and Humber"
```

Change the region name variable into an ordered factor.

``` r
hpif$Region.name <- ordered(hpif$Region.name,
                            levels = c("North East", "North West", "Yorks and Humber",
                                       "East Midlands", "West Midlands", "East",
                                       "London", "South East", "South West"))
```

### Animated plot

We are now ready to make an animated plot of lagged price versus percentage price change. First let's create a base plot to work from.

``` r
p <- hpif %>%
  ggplot(aes(x = lag_price_rank, y = Y3_price_change)) +
  geom_point(aes(fill = Region.name), shape = 21, size = 3, alpha = 0.6) +
  stat_smooth(colour = "black") +
  scale_x_reverse(breaks = c(1, 100, 200, 300)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) +
  labs(title = "Local house price growth in England by initial price rank: {frame_time}",
       subtitle = "Average annual percentage price growth over three years",
       caption = "Data from UK House Price Index",
       x = "Rank of average price at start of three-year period (1 is most expensive)",
       y = "") 
```

Now we add animation. Because `gganimate` defaults to 100 frames but we have a relatively large number of time periods, we need to specify the number of frames as the number of different dates.

``` r
animate(p + transition_time(Date), nframes = nlevels(as.factor(hpif$Date)))
```

![](House_price_cycle_files/figure-markdown_github/animate%20plot-1.gif)
