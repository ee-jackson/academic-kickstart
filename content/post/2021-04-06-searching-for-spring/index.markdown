---
title: "Searching for Spring: how to get data from the iNaturalist API in R"
author: Eleanor Jackson
date: '2021-04-06'
slug: searching-for-spring
categories: ["Adventures in R"]
tags: ["R", "Phenology"]
subtitle: ''
summary: ''
authors: ["admin"]
lastmod: '2021-04-06T17:48:49+01:00'
featured: no
image:
  caption: ''
  focal_point: 'Center'
  preview_only: true
projects: []
---

[iNaturalist](https://www.inaturalist.org) is an online community where people can record and share observations. We have been using iNaturalist in our group for student projects. It's proved a great way to teach how to collect and analyse data while the lab and field have been off-limits. To find a way to make data collection easier, I have been playing around with [iNaturalist's API](https://api.inaturalist.org/v1/docs/).

It has been a long, dark winter for those of us under lockdown in the UK and to give myself something to look forward to, I decided to look into when we might expect to see the first Spring bulbs emerging. 

iNaturalist does have a [bulk download facility](https://www.inaturalist.org/observations/export), but you can't pull the data directly into R, and I want to filter the data by `term_id`, which is not an option when using the bulk download facility.



I'm going to use functions from [{httr}](https://github.com/r-lib/httr) to query the API, and [{jsonlite}](https://github.com/jeroen/jsonlite) to deal with the ugly json file that the API will return  🙅


```r
library("tidyverse")
library("httr")
library("jsonlite")
```

The API will only give us 200 records at a time, this is the max number of records per page, so I'm writing a function that I can use to repeatedly hit the server. The call I'm using includes filters to pull out the data I want to look at. Briefly, I'm filtering for observations of plants `iconic_taxa=Plantae`, in the UK `place_id=6857`, which have the annotations Plant Phenology `term_id=12`, and Flowering `term_value_id=13`. Annotations are [a little different](https://www.inaturalist.org/pages/annotations) to Observation Fields in iNaturalist. Observation Fields can be created and added by anyone, whereas Annotations are maintained by iNaturalist administrators. This means I will probably pull fewer observations, but they might be more reliable.

Usually you'd use the `page` parameter to cycle through each page and retreive all the results, however, if there are more than 10k results, [iNaturalist recommends](https://www.inaturalist.org/pages/api+recommended+practices) you sort by ascending ID `order_by=id&order=asc`, and use the `id_above` parameter set to the ID of the record in the last batch.


```r
get_obs <- function(max_id){

	# an API call that has "id_above =" at the end
	call <- paste("https://api.inaturalist.org/v1/observations?
	              iconic_taxa=Plantae&term_id=12&term_value_id=13&place_id=6857
	              &d1=2017-01-01&per_page=200&order_by=id&order=asc&id_above=", 
	              max_id, sep="")
	 
	# making the API call, parsing it to JSON and then flatten
	GET(url = call) %>%
	content(as = "text", encoding = "UTF-8") %>%
	fromJSON(flatten = TRUE) -> get_call_json
	 
	# this grabs just the data we want and makes it a data frame
	as.data.frame(get_call_json$results)
	
}
```

Now that we have our function, I'm going to use it to pull the first page of results by setting `max_id` to zero. Once we've got that, we can create a list with our first page of results as the first item. I can then use a while loop to continually hit the API and append each new page to the list. The loop will run while the number of rows in a page is equal to 200. The last page of results will have less than 200 rows, and the loop will stop running. I've also told it to print the page number so I can see how it's progressing, although I won't print all that output in this post.


```r
# get the first page
obs <- get_obs(max_id = 0)
max_id <- max(obs[["id"]])
thisisalist <- list(page_1 = obs)
page <- 1

while (nrow(obs) == 200) {
	Sys.sleep(0.5)
	page <- page + 1
	page_count <- paste("page", page, sep = "_")
	obs <- get_obs(max_id = max_id)
	thisisalist[[page_count]] <- obs
	max_id <- max(obs[["id"]])
	print(page_count)
	print(max_id)
}
```



We can now bind all the elements of the list into one big dataframe and explore.


```r
thisisnotalist <- bind_rows(thisisalist)
glimpse(thisisnotalist)
```

```
## Rows: 21,836
## Columns: 160
## $ quality_grade                                     <chr> "research", "researc…
## $ time_observed_at                                  <chr> "2017-01-08T13:03:45…
## $ taxon_geoprivacy                                  <chr> NA, NA, NA, NA, NA, …
## $ annotations                                       <list> [<data.frame[1 x 27…
## $ uuid                                              <chr> "81e8c2c1-9e27-4b3f-…
## $ id                                                <int> 4913810, 4919144, 49…
## $ cached_votes_total                                <int> 0, 0, 0, 0, 0, 0, 0,…
## $ identifications_most_agree                        <lgl> TRUE, TRUE, TRUE, TR…
## $ species_guess                                     <chr> NA, NA, NA, NA, NA, …
## $ identifications_most_disagree                     <lgl> FALSE, FALSE, FALSE,…
## $ tags                                              <list> <>, <>, <>, <>, <>,…
## $ positional_accuracy                               <int> 20, 500, 31, 19, 24,…
## $ comments_count                                    <int> 0, 0, 0, 0, 0, 0, 0,…
## $ site_id                                           <int> 1, 1, 1, 1, 1, 1, 1,…
## $ created_time_zone                                 <chr> "Etc/UTC", "Etc/UTC"…
## $ license_code                                      <chr> "cc-by-nc-nd", "cc-b…
## $ observed_time_zone                                <chr> "Etc/UTC", "Etc/UTC"…
## $ quality_metrics                                   <list> [<data.frame[0 x 0]…
## $ public_positional_accuracy                        <int> 20, 500, 31, 19, 24,…
## $ reviewed_by                                       <list> <160, 216168, 35338…
## $ oauth_application_id                              <int> 3, 3, 3, 3, 3, 3, 3,…
## $ flags                                             <list> [], [], [], [], [],…
## $ created_at                                        <chr> "2017-01-08T19:12:15…
## $ description                                       <chr> NA, NA, NA, NA, NA, …
## $ time_zone_offset                                  <chr> "+00:00", "+00:00", …
## $ project_ids_with_curator_id                       <list> <>, <>, <>, <>, <>,…
## $ observed_on                                       <chr> "2017-01-08", "2017-…
## $ observed_on_string                                <chr> "Sun Jan 08 2017 13:…
## $ updated_at                                        <chr> "2017-01-08T19:40:15…
## $ sounds                                            <list> [], [], [], [], [],…
## $ place_ids                                         <list> <6857, 6858, 30408,…
## $ captive                                           <lgl> FALSE, FALSE, FALSE,…
## $ ident_taxon_ids                                   <list> <48460, 47126, 2111…
## $ outlinks                                          <list> [<data.frame[0 x 0]…
## $ faves_count                                       <int> 0, 0, 0, 0, 0, 0, 0,…
## $ ofvs                                              <list> [<data.frame[0 x 0]…
## $ num_identification_agreements                     <int> 1, 1, 1, 1, 1, 1, 1,…
## $ comments                                          <list> [<data.frame[0 x 0]…
## $ map_scale                                         <int> NA, NA, NA, NA, NA, …
## $ uri                                               <chr> "https://www.inatura…
## $ project_ids                                       <list> <>, <>, <>, <>, <>,…
## $ community_taxon_id                                <int> 53211, 53211, 53211,…
## $ owners_identification_from_vision                 <lgl> FALSE, FALSE, FALSE,…
## $ identifications_count                             <int> 1, 1, 1, 1, 1, 1, 1,…
## $ obscured                                          <lgl> FALSE, FALSE, FALSE,…
## $ num_identification_disagreements                  <int> 0, 0, 0, 0, 0, 0, 0,…
## $ geoprivacy                                        <chr> NA, NA, NA, NA, NA, …
## $ location                                          <chr> "51.2741473136,0.039…
## $ votes                                             <list> [], [], [], [], [],…
## $ spam                                              <lgl> FALSE, FALSE, FALSE,…
## $ mappable                                          <lgl> TRUE, TRUE, TRUE, TR…
## $ identifications_some_agree                        <lgl> TRUE, TRUE, TRUE, TR…
## $ project_ids_without_curator_id                    <list> <>, <>, <>, <>, <>,…
## $ place_guess                                       <chr> "Clacket Wood, Weste…
## $ identifications                                   <list> [<data.frame[2 x 84…
## $ project_observations                              <list> [<data.frame[0 x 0]…
## $ photos                                            <list> [<data.frame[1 x 7]…
## $ observation_photos                                <list> [<data.frame[1 x 10…
## $ faves                                             <list> [], [], [], [], [],…
## $ non_owner_ids                                     <list> [<data.frame[1 x 84…
## $ out_of_range                                      <lgl> NA, NA, NA, NA, NA, …
## $ id_please                                         <lgl> NA, NA, NA, NA, NA, …
## $ observed_on_details.date                          <chr> "2017-01-08", "2017-…
## $ observed_on_details.week                          <int> 1, 2, 2, 2, 3, 3, 3,…
## $ observed_on_details.month                         <int> 1, 1, 1, 1, 1, 1, 1,…
## $ observed_on_details.hour                          <int> 13, 11, 9, 10, 10, 1…
## $ observed_on_details.year                          <int> 2017, 2017, 2017, 20…
## $ observed_on_details.day                           <int> 8, 9, 10, 13, 19, 19…
## $ created_at_details.date                           <chr> "2017-01-08", "2017-…
## $ created_at_details.week                           <int> 1, 2, 2, 2, 3, 3, 3,…
## $ created_at_details.month                          <int> 1, 1, 1, 1, 1, 1, 1,…
## $ created_at_details.hour                           <int> 19, 20, 20, 22, 19, …
## $ created_at_details.year                           <int> 2017, 2017, 2017, 20…
## $ created_at_details.day                            <int> 8, 9, 10, 13, 19, 19…
## $ taxon.is_active                                   <lgl> TRUE, TRUE, TRUE, TR…
## $ taxon.ancestry                                    <chr> "48460/47126/211194/…
## $ taxon.min_species_ancestry                        <chr> "48460,47126,211194,…
## $ taxon.endemic                                     <lgl> FALSE, FALSE, FALSE,…
## $ taxon.iconic_taxon_id                             <int> 47126, 47126, 47126,…
## $ taxon.min_species_taxon_id                        <int> 53211, 53211, 53211,…
## $ taxon.threatened                                  <lgl> FALSE, FALSE, FALSE,…
## $ taxon.rank_level                                  <int> 10, 10, 10, 10, 10, …
## $ taxon.introduced                                  <lgl> FALSE, FALSE, FALSE,…
## $ taxon.native                                      <lgl> TRUE, TRUE, TRUE, TR…
## $ taxon.parent_id                                   <int> 53207, 53207, 53207,…
## $ taxon.name                                        <chr> "Senecio vulgaris", …
## $ taxon.rank                                        <chr> "species", "species"…
## $ taxon.extinct                                     <lgl> FALSE, FALSE, FALSE,…
## $ taxon.id                                          <int> 53211, 53211, 53211,…
## $ taxon.ancestor_ids                                <list> <48460, 47126, 2111…
## $ taxon.photos_locked                               <lgl> FALSE, FALSE, FALSE,…
## $ taxon.taxon_schemes_count                         <int> 6, 6, 6, 6, 6, 6, 6,…
## $ taxon.wikipedia_url                               <chr> "http://en.wikipedia…
## $ taxon.current_synonymous_taxon_ids                <lgl> NA, NA, NA, NA, NA, …
## $ taxon.created_at                                  <chr> "2009-09-18T19:06:22…
## $ taxon.taxon_changes_count                         <int> 0, 0, 0, 0, 0, 0, 0,…
## $ taxon.complete_species_count                      <int> NA, NA, NA, NA, NA, …
## $ taxon.universal_search_rank                       <int> 15375, 15375, 15375,…
## $ taxon.observations_count                          <int> 15375, 15375, 15375,…
## $ taxon.atlas_id                                    <int> NA, NA, NA, NA, NA, …
## $ taxon.iconic_taxon_name                           <chr> "Plantae", "Plantae"…
## $ taxon.preferred_common_name                       <chr> "common groundsel", …
## $ taxon.preferred_establishment_means               <chr> "native", "native", …
## $ taxon.complete_rank                               <chr> NA, NA, NA, NA, NA, …
## $ taxon.flag_counts.resolved                        <int> 0, 0, 0, 0, 0, 0, 0,…
## $ taxon.flag_counts.unresolved                      <int> 0, 0, 0, 0, 0, 0, 0,…
## $ taxon.default_photo.id                            <int> 5289, 5289, 5289, 52…
## $ taxon.default_photo.license_code                  <chr> "cc-by-nc-sa", "cc-b…
## $ taxon.default_photo.attribution                   <chr> "(c) Anita, some rig…
## $ taxon.default_photo.url                           <chr> "https://inaturalist…
## $ taxon.default_photo.flags                         <list> [], [], [], [], [],…
## $ taxon.default_photo.square_url                    <chr> "https://inaturalist…
## $ taxon.default_photo.medium_url                    <chr> "https://inaturalist…
## $ taxon.default_photo.original_dimensions.height    <int> 1004, 1004, 1004, 10…
## $ taxon.default_photo.original_dimensions.width     <int> 1296, 1296, 1296, 12…
## $ taxon.establishment_means.id                      <int> 3359384, 3359384, 33…
## $ taxon.establishment_means.place_id                <int> 6857, 6857, 6857, 68…
## $ taxon.establishment_means.user_id                 <int> 5692, 5692, 5692, 56…
## $ taxon.establishment_means.occurrence_status_level <int> NA, NA, NA, NA, NA, …
## $ taxon.establishment_means.establishment_means     <chr> "native", "native", …
## $ taxon.conservation_status.place_id                <int> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.source_id               <int> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.user_id                 <int> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.authority               <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.status                  <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.status_name             <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.geoprivacy              <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.iucn                    <int> NA, NA, NA, NA, NA, …
## $ preferences.prefers_community_taxon               <lgl> NA, NA, NA, NA, NA, …
## $ geojson.coordinates                               <list> <0.0393538, 51.2741…
## $ geojson.type                                      <chr> "Point", "Point", "P…
## $ user.site_id                                      <int> 1, 1, 1, 1, 1, 1, 1,…
## $ user.created_at                                   <chr> "2016-10-18T21:16:18…
## $ user.id                                           <int> 353381, 353381, 3533…
## $ user.login                                        <chr> "wildhamandpetersham…
## $ user.spam                                         <lgl> FALSE, FALSE, FALSE,…
## $ user.suspended                                    <lgl> FALSE, FALSE, FALSE,…
## $ user.login_autocomplete                           <chr> "wildhamandpetersham…
## $ user.login_exact                                  <chr> "wildhamandpetersham…
## $ user.name                                         <chr> "Paul Cook", "Paul C…
## $ user.name_autocomplete                            <chr> "Paul Cook", "Paul C…
## $ user.orcid                                        <chr> NA, NA, NA, NA, NA, …
## $ user.icon                                         <chr> "https://static.inat…
## $ user.observations_count                           <int> 38008, 38008, 38008,…
## $ user.identifications_count                        <int> 2148, 2148, 2148, 21…
## $ user.journal_posts_count                          <int> 0, 0, 0, 0, 0, 0, 0,…
## $ user.activity_count                               <int> 40156, 40156, 40156,…
## $ user.species_count                                <int> 3097, 3097, 3097, 30…
## $ user.universal_search_rank                        <int> 38008, 38008, 38008,…
## $ user.roles                                        <list> <>, <>, <>, <>, <>,…
## $ user.icon_url                                     <chr> "https://static.inat…
## $ user.preferences.prefers_community_taxa           <lgl> TRUE, TRUE, TRUE, TR…
## $ user.preferences.prefers_observation_fields_by    <chr> "anyone", "anyone", …
## $ user.preferences.prefers_project_addition_by      <chr> "any", "any", "any",…
## $ taxon.uuid                                        <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.user.id                 <int> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.user.login              <chr> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.user.icon_url           <lgl> NA, NA, NA, NA, NA, …
## $ taxon.conservation_status.user.orcid              <lgl> NA, NA, NA, NA, NA, …
## $ user.uuid                                         <chr> NA, NA, NA, NA, NA, …
```

That's a lot of different variables! I'm interested in `observed_on`. Let's convert it to date format and do some quick overview plots.


```r
thisisnotalist %>%
	mutate(observed_on_date = as.Date(observed_on, "%Y-%m-%d"),
	 day_of_year = as.numeric(strftime(observed_on_date, 
	 format = "%j")) ) -> flower_obs

flower_obs %>%
	ggplot(aes(x = observed_on_date)) +
	geom_bar(color = "#036C34") +
	scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
	labs(x = "Day observed")
```

<img src="index_files/figure-html/plot_date-1.png" width="672" />

The number of observations recorded has increased each year, possibly due to increasing popularity of iNaturalist. There are also many more observations in the summer months than in the winter. We can get a better look at this pattern plotting by month.


```r
flower_obs %>%
  filter(observed_on_details.year < 2021) %>% # we don't have the full data for this yr
	ggplot(aes(x= as.factor(observed_on_details.month))) +
	geom_bar(fill = "grey90", color = "#036C34") +
	labs(x= "Month observed")
```

<img src="index_files/figure-html/plot_month-1.png" width="672" />

There's a peak in April with observations slowly dropping throughout the year. I didn't quite expect that. I would have assumed that observations of flowering plants would be relatively flat from April to August as different plants come into flower at different points throughout the spring and summer. Perhaps this is due to some kind of sampling bias in how people are recording observations. Let's have a look at which plants are the first to flower.


```r
flower_obs %>%
	group_by(taxon.name, taxon.preferred_common_name) %>%
	summarise(median_day = median(day_of_year), n_obs = n()) %>%
	ungroup() %>%
	# filter to sp with more than 20 observations 
	# to make sure we get a representitive sample size
	filter(n_obs > 20) %>% 
	slice_min(order_by = median_day, n = 15) %>%
	knitr::kable()
```



|taxon.name                |taxon.preferred_common_name | median_day| n_obs|
|:-------------------------|:---------------------------|----------:|-----:|
|Petasites fragrans        |Winter Heliotrope           |       29.0|    25|
|Corylus avellana          |common hazel                |       43.0|    37|
|Crocus tommasinianus      |Woodland crocus             |       48.0|    22|
|Galanthus nivalis         |common snowdrop             |       50.0|   208|
|Cyclamen coum             |Eastern Sowbread            |       55.0|    31|
|Viburnum tinus            |Laurustinus viburnum        |       58.0|    23|
|Prunus cerasifera         |Cherry-plum                 |       65.5|    48|
|Narcissus pseudonarcissus |wild daffodil               |       68.0|    30|
|Viola odorata             |Sweet violet                |       71.0|    64|
|Draba verna               |Common Whitlowgrass         |       72.0|    27|
|Mercurialis perennis      |Dog's Mercury               |       77.0|    39|
|Cardamine hirsuta         |hairy bittercress           |       77.5|    40|
|Narcissus                 |daffodils                   |       79.5|    56|
|Primula vulgaris          |Primrose                    |       82.0|   185|
|Tussilago farfara         |colt's-foot                 |       82.0|    91|

I always think of Crocus coming up first, along with Snowdrops, so it's reassuring to see them high up on this list. I think it would be nice to get a visualisation of the emergence of different flowers with the changing seasons. I'm going to choose a few which I know emerge fairly sequentially: Snowdrops (genus = _Galanthus_), Crocus, Daffodils (_Narcissus_), Hyacinths (_Hyacinthoides_), Iris, and finally Cyclamen, which flowers through the winter.

I have to do some fiddling around here with dates. I'm going to change the year for every observation to be 2020, so that data from different years can be grouped together. I also use `floor_date` from [{lubridate}](https://github.com/tidyverse/lubridate) to round dates to the week they were observed. e.g. Tuesday 6th and Thursday 8th would both be rounded down to Sunday 4th.


```r
library("viridis") # for colours
library("lubridate")

flower_obs %>%
	filter(str_detect(taxon.name, "Narcissus") | 
		str_detect(taxon.name, "Iris") | 
		str_detect(taxon.name, "Crocus")| 
		str_detect(taxon.name, "Galanthus")|
		str_detect(taxon.name, "Hyacinthoides")|
		str_detect(taxon.name, "Cyclamen")) %>% 
	separate(taxon.name, into = c("genus", "sp"), sep = " ") %>%
	mutate(date = as.Date(paste(2020, strftime(observed_on, format = "%m-%d"), 
	                            sep="-")) ) %>% 
	mutate(week = floor_date(date, "week")) %>%
	ggplot(aes(x= week, fill = genus)) +
	geom_bar(position = "fill") +
	scale_x_date(date_breaks = "1 month",  expand = c(0,0),
		date_labels = "%B", limits = as.Date(c("2020-01-01", "2020-12-31"))) +
	coord_polar() + 
	scale_fill_viridis_d(option = "plasma") +
	labs(y = "% of observations", x = "Week observed") +
  theme_void(base_size = 9)+
  theme(axis.text.x = element_text())
```

<img src="index_files/figure-html/plot_polar-1.png" width="672" />

I feel like this is a really nice one to use `coord_polar()` for. You can see how the months blend into each other, without a break at Dec/Jan. _Narcissus_ flowering phenology is well represented here, you can see how it steadily increases to a peak and then dips down again. 

But ok, what about if you live in Edinburgh, are you going to see Daffs on the same day as people in London? Maybe we can see how geography affects flowering observations.

We want [{sf}](https://github.com/r-spatial/sf) to work with spatial features, [{rnaturalearthdata}](https://github.com/ropensci/rnaturalearthdata) to get a map of the UK, [{rnaturalearth}](https://github.com/ropensci/rnaturalearth) for functions to work with that data and, for fun, [{gganimate}](https://github.com/thomasp85/gganimate).


```r
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("gganimate")

# get a map of the UK as a 'sf' polygon from the natural earth data
uk_map <- ne_countries(country = "united kingdom", scale = 'medium', returnclass = "sf")

# filter our data for daffodil observations
# and extract lattitude and longitude from the 'location' variable
flower_obs %>%
	filter(str_detect(taxon.name, "Narcissus") & mappable == TRUE ) %>%
	separate(location, into = c("lat", "long"), sep = ",") %>%
	select("lat", "long", "day_of_year") %>%
	st_as_sf(coords = c( "long","lat"), 
		crs = 4326, agr = "constant")  -> geo_daff

ggplot() +
	geom_sf(data = uk_map) +
	geom_sf(data = geo_daff, shape = 16, size = 1.5, 
		alpha = 0.8, colour = "forestgreen") +
	transition_states(as.factor(day_of_year),
                    state_length = 3) + 
	shadow_mark(past = TRUE, future = FALSE) +
	ggtitle("Observations of Daffodils in flower\n
		Day of the year: {closest_state}") 
```

![](index_files/figure-html/plot_map-1.gif)<!-- -->

I was hoping we'd see some kind of wave with flowers appearing in the South first, but alas, maybe there's just not enough data. It looks pretty cool though 🤷 

Some reading for you:
*	[Barve, V.V _et al._ Methods for broad‐scale plant phenology assessments using citizen scientists’ photographs. _Applications in Plant Sciences_ (2020)](https://doi.org/10.1002/aps3.11315)
* [Mesaglio, T _et al._ An overview of the history, current contributions and future outlook of iNaturalist in Australia. _Wildlife Research_ (2021)](https://doi.org/10.1071/WR20154)
* [Nowak, K _et al._ Using community photography to investigate phenology: A case study of coat molt in the mountain goat (_Oreamnos americanus_) with missing data. _Ecology and Evolution_ (2020)](https://doi.org/10.1002/ece3.6954)
