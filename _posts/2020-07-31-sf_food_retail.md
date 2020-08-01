---
layout: post
title: "SF Food Access"
tags: research
---

A good friend of mine is in the public health sector focusing on food
and nutrition policy, specifically focusing on access and equity for
farmer’s markets. I’ve learned a lot from her around the patterns and
trends associated with farmer’s markets in California. There’s a
normative aspect to her work: a belief that localizing supply chains and
connecting farmers directly with consumers is going to be a key go
forward strategy for both environmentalism *and* food access. One of her
research programs right now is focused on understanding the barriers
discouraging Black communities from patronizing these markets to the
same levels that other communities in California do. There’s the obvious
factor of price; yet it doesn’t fully explain the consumption patterns
as there are definitely lower price point markets that working class
immigrant enclaves heavily rely on. In addition to price then, maybe
there’s a secondary factor of proximity. Regardless, it seems like
wrapped up in this is just larger questions around food access for poor
Black communities in America, a topic that I’m not even close to
understanding enough to be able to have a proper discussion about.

As part of building out my general knowledge there, I’ve been reading
some papers around the discriminatory aspects of food access in America.
In parallel, I decided to take a look at the data itself, specifically
San Francisco. Fortunately, the SF City Government has open sourced a
lot of interesting datasets, with one being a collection of **all**
registered business in the city (including grocery stores and markets!).
Below, I document my efforts in exploring this dataset and understanding
a bit more about the trends of food and grocery retail in San Francisco.

Pretty much my default package list for any analysis leveraging public
data. (If I’m querying a database for work, I’ll be looking more towards
database clients such as `RPostgresql`)

    library(tidyr)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(scales)
    library(httr)
    library(jsonlite)
    library(readr)
    library(purrr)

    # https://data.sfgov.org/Economy-and-Community/Registered-Business-Locations-San-Francisco/g8m3-pdis

The SF gov has a nicely maintained public API for accessing datasets.
You can also just download a csv and read it in manually, but I thought
I’d be fancy for reproducibility purposes! A few nuances regarding
access patterns:

-   there’s a 50000 record limit with one request, which means we have
    to paginate using an offset parameter
-   we get back lat/long coordinates as individual elements in a list;
    when `jsonlite` parses the .json, it stores these values as a nested
    dataframe column within the larger dataframe
-   there’s some weird empty records that we have to catch

<!-- -->

    # check how many total records so we can back into the number of requests we have to make
    records <- fromJSON(content(GET("https://data.sfgov.org/resource/g8m3-pdis.json?$select=count(ttxid)"), as="text"))[1,1]
    total_pages <- ceiling(as.integer(records)/50000)

    endpoint <- "https://data.sfgov.org/resource/g8m3-pdis.json"

    # loop through the number of requests
    for (i in seq(c(1:total_pages))) {
      # logic with offset is pretty straightforward; on page 1 the offset is 0, for page 2 we want our offset to start us at 50,001
      url <- paste0(endpoint, "?$limit=50000&$offset=", format((i-1)*50000, scientific = FALSE), "&$order=ttxid")
      print(url)
      df_temp <- fromJSON(content(GET(url, add_headers(`X-App-Token` = "AQrzpwEVnUYlmA22uZxnaqiVY")), as = "text"))
      
      if ("location" %in% colnames(df_temp)) {
        # there's a nested df called location within our parsed dataframe; one of the columns is a nested list-wise column called coordinates, contain lat/long as individual elements within a list
        location_df <- 
          df_temp$location %>%
          mutate(
            lat = map_dbl(coordinates, function(x) {if (is.null(x[1])) {as.double(NA)} else {x[1]}}),
            long = map_dbl(coordinates, function(x) {if (is.null(x[2])) {as.double(NA)} else {x[2]}})
          ) %>%
          select(-coordinates)
      
        df_temp <- cbind.data.frame(df_temp %>% select(-location), location_df)
      } else {
        df_temp <- df_temp %>% mutate(lat = as.double(NA), long = as.double(NA), type = as.character(NA))  
      }
      
      # bind everything together
      if (exists("business_df")) {
        business_df <- rbind.data.frame(business_df, df_temp)
      } else {
        business_df <- df_temp
      }
    }

    rm(df_temp)
    rm(location_df)

    # save the rds so we don't have to rerun this everytime
    business_df %>% saveRDS("sf_business_data.rds")

    business_df <- readRDS("sf_business_data.rds")

Really basic `Get to Know Your Data`. Looks like Food Services and
Retail Trade are the NAIC codes we want for groceries. There’s a *lot*
of business without a code; I did some quick checks and I don’t think
there are any groceries in there, mostly apartments and things like
that. One thing to note is that multiple businesses can be registered at
the same address. For example, there’s a market in Laurel Heights that’s
one record, but inside that market is a butcher store, coffee shop,
cheese shop, etc. that all registered as separate businesses within same
address. The “primary” market has a `Food Services` NAIC code, but all
the secondary businesses have an `NA` code.

    business_df %>% 
      group_by(naic_code_description) %>%
      count()

    ## # A tibble: 19 x 2
    ## # Groups:   naic_code_description [19]
    ##    naic_code_description                                n
    ##    <chr>                                            <int>
    ##  1 Accommodations                                    8180
    ##  2 Administrative and Support Services               4228
    ##  3 Arts, Entertainment, and Recreation               8316
    ##  4 Certain Services                                  4494
    ##  5 Construction                                     18268
    ##  6 Financial Services                                5259
    ##  7 Food Services                                    13593
    ##  8 Information                                       4989
    ##  9 Insurance                                          869
    ## 10 Manufacturing                                     2429
    ## 11 Multiple                                          5447
    ## 12 Private Education and Health Services             8564
    ## 13 Professional, Scientific, and Technical Services 27547
    ## 14 Real Estate and Rental and Leasing Services      26275
    ## 15 Retail Trade                                     14610
    ## 16 Transportation and Warehousing                    6700
    ## 17 Utilities                                          359
    ## 18 Wholesale Trade                                   3893
    ## 19 <NA>                                             96203

    business_df %>% 
      filter(
        is.na(naic_code_description)
      ) %>% 
      head

    ##            ttxid certificate_number           ownership_name
    ## 1 0000071-01-001            0000071        Tournahu George L
    ## 2 0000071-02-001            0000071        Tournahu George L
    ## 3 0000071-02-003            0000071        Tournahu George L
    ## 4 0000071-03-001            0000071        Tournahu George L
    ## 5 0000086-01-001            0000086 Harry D Som Living Trust
    ## 6 0000086-02-002            0000086 Harry D Som Living Trust
    ##                    dba_name full_business_address          city state
    ## 1             Tournahu Arms     1842 Jefferson St San Francisco    CA
    ## 2 3301 Broderick Apartments     3301 Broderick St San Francisco    CA
    ## 3 3301 Broderick Apartments     3301 Broderick St San Francisco    CA
    ## 4      1840-42 Jefferson St 1840 Jefferson St #42 San Francisco    CA
    ## 5           1601 Grant Apts        1601 Grant Ave San Francisco    CA
    ## 6        1601 Grant Parking        1601 Grant Ave San Francisco    CA
    ##   business_zip          dba_start_date     location_start_date
    ## 1        94123 1968-10-01T00:00:00.000 1968-10-01T00:00:00.000
    ## 2        94123 1968-10-01T00:00:00.000 1988-05-01T00:00:00.000
    ## 3        94123 1968-10-01T00:00:00.000 1988-05-01T00:00:00.000
    ## 4        94123 1968-10-01T00:00:00.000 1991-10-07T00:00:00.000
    ## 5        94133 1968-10-01T00:00:00.000 1968-10-01T00:00:00.000
    ## 6        94133 1968-10-01T00:00:00.000 2013-01-01T00:00:00.000
    ##      mailing_address_1     mail_city mail_zipcode mail_state naic_code
    ## 1 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 2 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 3 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 4 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 5                 <NA>          <NA>         <NA>       <NA>      <NA>
    ## 6                 <NA>          <NA>         <NA>       <NA>      <NA>
    ##   naic_code_description parking_tax transient_occupancy_tax
    ## 1                  <NA>       FALSE                   FALSE
    ## 2                  <NA>       FALSE                   FALSE
    ## 3                  <NA>       FALSE                   FALSE
    ## 4                  <NA>        TRUE                   FALSE
    ## 5                  <NA>       FALSE                   FALSE
    ## 6                  <NA>       FALSE                   FALSE
    ##   supervisor_district neighborhoods_analysis_boundaries
    ## 1                <NA>                              <NA>
    ## 2                   2                            Marina
    ## 3                   2                            Marina
    ## 4                <NA>                              <NA>
    ## 5                   3                       North Beach
    ## 6                   3                       North Beach
    ##         location_end_date            dba_end_date  lic
    ## 1 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 2 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 3 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 4                    <NA>                    <NA> <NA>
    ## 5 2016-06-30T00:00:00.000 2016-06-30T00:00:00.000 <NA>
    ## 6 2016-06-30T00:00:00.000 2016-06-30T00:00:00.000 <NA>
    ##   lic_code_description business_corridor  type       lat     long
    ## 1                 <NA>              <NA> Point -122.4430 37.80473
    ## 2                 <NA>              <NA> Point -122.4448 37.80088
    ## 3                 <NA>              <NA> Point -122.4448 37.80088
    ## 4                 <NA>              <NA> Point -122.4428 37.80476
    ## 5                 <NA>              <NA> Point -122.4078 37.80172
    ## 6                 <NA>              <NA> Point -122.4078 37.80172

    business_df %>%
      filter(
        is.na(naic_code_description)
      ) %>%
      left_join(
        business_df %>%
          filter(
            naic_code_description %in% c("Food Services", "Retail Trade")
          ) %>%
          select(ttxid_match = ttxid, full_business_address),
        by="full_business_address"
      ) %>%
      filter(
        is.na(ttxid_match)
      ) %>% 
      head

    ##            ttxid certificate_number           ownership_name
    ## 1 0000071-01-001            0000071        Tournahu George L
    ## 2 0000071-02-001            0000071        Tournahu George L
    ## 3 0000071-02-003            0000071        Tournahu George L
    ## 4 0000071-03-001            0000071        Tournahu George L
    ## 5 0000086-01-001            0000086 Harry D Som Living Trust
    ## 6 0000086-02-002            0000086 Harry D Som Living Trust
    ##                    dba_name full_business_address          city state
    ## 1             Tournahu Arms     1842 Jefferson St San Francisco    CA
    ## 2 3301 Broderick Apartments     3301 Broderick St San Francisco    CA
    ## 3 3301 Broderick Apartments     3301 Broderick St San Francisco    CA
    ## 4      1840-42 Jefferson St 1840 Jefferson St #42 San Francisco    CA
    ## 5           1601 Grant Apts        1601 Grant Ave San Francisco    CA
    ## 6        1601 Grant Parking        1601 Grant Ave San Francisco    CA
    ##   business_zip          dba_start_date     location_start_date
    ## 1        94123 1968-10-01T00:00:00.000 1968-10-01T00:00:00.000
    ## 2        94123 1968-10-01T00:00:00.000 1988-05-01T00:00:00.000
    ## 3        94123 1968-10-01T00:00:00.000 1988-05-01T00:00:00.000
    ## 4        94123 1968-10-01T00:00:00.000 1991-10-07T00:00:00.000
    ## 5        94133 1968-10-01T00:00:00.000 1968-10-01T00:00:00.000
    ## 6        94133 1968-10-01T00:00:00.000 2013-01-01T00:00:00.000
    ##      mailing_address_1     mail_city mail_zipcode mail_state naic_code
    ## 1 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 2 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 3 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 4 2443 FILLMORE ST 271 San Francisco        94115         CA      <NA>
    ## 5                 <NA>          <NA>         <NA>       <NA>      <NA>
    ## 6                 <NA>          <NA>         <NA>       <NA>      <NA>
    ##   naic_code_description parking_tax transient_occupancy_tax
    ## 1                  <NA>       FALSE                   FALSE
    ## 2                  <NA>       FALSE                   FALSE
    ## 3                  <NA>       FALSE                   FALSE
    ## 4                  <NA>        TRUE                   FALSE
    ## 5                  <NA>       FALSE                   FALSE
    ## 6                  <NA>       FALSE                   FALSE
    ##   supervisor_district neighborhoods_analysis_boundaries
    ## 1                <NA>                              <NA>
    ## 2                   2                            Marina
    ## 3                   2                            Marina
    ## 4                <NA>                              <NA>
    ## 5                   3                       North Beach
    ## 6                   3                       North Beach
    ##         location_end_date            dba_end_date  lic
    ## 1 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 2 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 3 2013-12-31T00:00:00.000                    <NA> <NA>
    ## 4                    <NA>                    <NA> <NA>
    ## 5 2016-06-30T00:00:00.000 2016-06-30T00:00:00.000 <NA>
    ## 6 2016-06-30T00:00:00.000 2016-06-30T00:00:00.000 <NA>
    ##   lic_code_description business_corridor  type       lat     long
    ## 1                 <NA>              <NA> Point -122.4430 37.80473
    ## 2                 <NA>              <NA> Point -122.4448 37.80088
    ## 3                 <NA>              <NA> Point -122.4448 37.80088
    ## 4                 <NA>              <NA> Point -122.4428 37.80476
    ## 5                 <NA>              <NA> Point -122.4078 37.80172
    ## 6                 <NA>              <NA> Point -122.4078 37.80172
    ##   ttxid_match
    ## 1        <NA>
    ## 2        <NA>
    ## 3        <NA>
    ## 4        <NA>
    ## 5        <NA>
    ## 6        <NA>

    # "Food Services", "Retail Trade"

Mostly SF based businesses, no surprise.

    business_df %>%
      filter(
        naic_code_description %in% c("Food Services", "Retail Trade")
      ) %>%
      group_by(city) %>%
      count(sort = TRUE)

    ## # A tibble: 517 x 2
    ## # Groups:   city [517]
    ##    city                    n
    ##    <chr>               <int>
    ##  1 San Francisco       25865
    ##  2 Oakland               157
    ##  3 San+francisco         111
    ##  4 Daly City             101
    ##  5 Sf                     73
    ##  6 South San Francisco    52
    ##  7 San Jose               51
    ##  8 S San Fran             45
    ##  9 San Mateo              37
    ## 10 Hayward                36
    ## # … with 507 more rows
