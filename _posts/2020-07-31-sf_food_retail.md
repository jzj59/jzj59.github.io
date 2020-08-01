---
layout: post
title: "SF Food Access"
tags: research
---

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

    records <- fromJSON(content(GET("https://data.sfgov.org/resource/g8m3-pdis.json?$select=count(ttxid)"), as="text"))[1,1]
    total_pages <- ceiling(as.integer(records)/50000)
    endpoint <- "https://data.sfgov.org/resource/g8m3-pdis.json"

    for (i in seq(c(1:total_pages))) {
      url <- paste0(endpoint, "?$limit=50000&$offset=", format((i-1)*50000, scientific = FALSE), "&$order=ttxid")
      print(url)
      df_temp <- fromJSON(content(GET(url, add_headers(`X-App-Token` = "AQrzpwEVnUYlmA22uZxnaqiVY")), as = "text"))
      
      if ("location" %in% colnames(df_temp)) {
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
      
      if (exists("business_df")) {
        business_df <- rbind.data.frame(business_df, df_temp)
      } else {
        business_df <- df_temp
      }
    }

    rm(df_temp)
    rm(location_df)

    business_df %>% saveRDS("sf_business_data.rds")

    business_df <- readRDS("sf_business_data.rds")

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
