Occurrence of human infection with Salmonella Typhi in sub-Saharan
Africa
================
2023-07-24

Source the util.R file and load packages

``` r
source("R/util.R") # this has ggplot theme, theme_map() 
library(tidyverse) # dplyr and ggplot2 packages are used
# library(data.table) # fread function
library(raster) # raster 
library(countrycode) # iso3c codes
library(sf) # st_contains function to check if the point is inside a polygon
library(patchwork) # combine multiple plots 
library(readxl) # read XLSX files
```

## Figure 2

Plot the frequency of publications by country and year

``` r
d <- read_xlsx("data/occ_data_final.xlsx", "data")

d %>% 
  distinct(STUDY_INFO, .keep_all = TRUE) %>% 
  filter(YEAR_PUBLICATION >= 2000) %>%
  group_by(YEAR_PUBLICATION) %>%
  summarise(n=n()) -> cnt

p1 <- ggplot(cnt, aes(YEAR_PUBLICATION, n)) + 
  geom_bar(stat="identity") + 
  theme_bw() +
  theme(axis.ticks.length=unit(-2.75, "pt"))+
  scale_x_continuous(sec.axis = dup_axis()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.x.top = element_blank(),
        axis.text.y.right = element_blank(),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank()) +
  labs(x = "Year of publication", y = "Number of articles")

p1
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#### Figure 2B

d %>%
  group_by(COUNTRY, YEAR_PUBLICATION) %>%
  summarize(count=n()) -> d_co_yr

d %>%
  group_by(YEAR_PUBLICATION) %>%
  summarize(count = n()) -> d_tot

d_tot$COUNTRY <- "Total"
d_co_yr <- bind_rows(d_co_yr, d_tot)
cntry_ordered <- unique(d_co_yr$COUNTRY)
d_co_yr$COUNTRY <- factor(d_co_yr$COUNTRY, levels = rev(cntry_ordered))

cntry_labels <- cntry_ordered
# cntry_labels[6] <- "CAR" # abbreviation for better displaying
cntry_labels[23] <- "Tanzania" # abbreviation for better displaying

p2 <- ggplot(data = d_co_yr) +
  geom_point(aes(x = YEAR_PUBLICATION, y = COUNTRY, size = count)) +
  scale_size_continuous(limits = c(1, 50)) +
  scale_x_continuous(breaks = seq(2000, 2020, by=5), limits = c(2000, 2020)) +
  scale_y_discrete(breaks = cntry_ordered, labels = cntry_labels) +
  labs(x="Year of publication", y = NULL, size="No. of reports" ) +
  theme_bw() +
  theme(axis.ticks.length=unit(-2.75, "pt"),
        legend.position = "bottom")

p2
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#### Figure 2C

d %>% 
  group_by(COUNTRY, STUDY_DATE_BEGIN) %>%
  summarize(count=n()) -> d_co_yr

d %>%
  group_by(STUDY_DATE_BEGIN) %>%
  summarize(count = n()) -> d_tot

d_tot$COUNTRY <- "Total"
d_co_yr <- bind_rows(d_co_yr, d_tot)
cntry_ordered <- unique(d_co_yr$COUNTRY)
d_co_yr$COUNTRY <- factor(d_co_yr$COUNTRY, levels = rev(cntry_ordered))

cntry_labels <- cntry_ordered
# cntry_labels[6] <- "CAR" # abbreviation for better displaying
cntry_labels[23] <- "Tanzania" # abbreviation for better displaying

p3 <- ggplot(data = d_co_yr) + 
  geom_point(aes(x = STUDY_DATE_BEGIN, y = COUNTRY, size = count)) +
  scale_size_continuous(limits = c(1, 50)) +
  scale_x_continuous(breaks = seq(2000, 2020, by=5), limits = c(2000, 2020)) +
  scale_y_discrete(breaks = cntry_ordered, labels = cntry_labels) + 
  labs(x="Year of study", y = NULL, size="No. of reports" ) +
  theme_bw() + 
  theme(axis.ticks.length=unit(-2.75, "pt"), 
        legend.position = "bottom")

p3
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
p <- p1 + p2 + p3 + 
  plot_layout(ncol = 1, heights = c(1.2, 2, 2)) + 
  plot_annotation(tag_levels = "A")

# fac <- 3.5
# ggsave(paste0("plots/occ_plots_", tstamp(), ".png"), p,
#        width = 3.4*1.8, height = 3.4*fac, units="in")
```

## Figure 3

Plot the grid cells of typhoid occurrence on the map of countries that
have the largest number of records: “Nigeria”, “Tanzania, United
Republic of”, “Kenya”, “Ghana” T

``` r
rst <- readRDS("data/elevation_20km_africa.rds")
afss <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")

dculture <- d[grepl("culture", d$DIAGNOSTIC_METHOD_SUMMARY, ignore.case = TRUE),]
dother <-  d[!grepl("culture", d$DIAGNOSTIC_METHOD_SUMMARY, ignore.case = TRUE),]

# culture-based occurrence
dculture %>% 
  filter(!is.na(LONGITUDE)) %>% 
  distinct(LONGITUDE, .keep_all = TRUE) -> dculture2

# occurrence matrix
occmat <- data.frame(X = as.double(dculture2$LONGITUDE),
                     Y = as.double(dculture2$LATITUDE))
occmat <- occmat[!is.na(occmat$X) & !is.na(occmat$Y), ]

dother %>%
  mutate(LONGITUDE=as.double(LONGITUDE), LATITUDE=as.double(LATITUDE)) %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>% 
  distinct(LONGITUDE, .keep_all = TRUE) -> dother2

occmat2 <- data.frame(X = dother2$LONGITUDE, Y = dother2$LATITUDE)
occmat2 <- occmat2[!is.na(occmat2$X) & !is.na(occmat2$Y), ]

cnames <- c("Nigeria", "Tanzania, United Republic of", "Kenya", "Ghana")
iso3c <- countrycode(cnames, 'country.name', 'iso3c')
plist <- vector("list", length(cnames))
for (ii in 1:length(cnames)){
  cname <- cnames[ii]
  cntry <- afss[afss$NAME_0 == cname,]
  r <- crop(rst, extent(cntry), snap="out")                
  # r <- mask(r, cntry)
  r <- setValues(r, 0)
  # set the value to 1 for the grid cell of culture-based occurrence
  for (i in 1:nrow(occmat)){
    cells <- raster::extract(r, occmat[i,], df=TRUE, cellnumbers=TRUE)
    raster::values(r)[unlist(cells$cell)] <- 1 
  }
  
  # set the value to 2 for the grid cells of non-culture-based occurrence
  for (i in 1:nrow(occmat2)){
    cells <- raster::extract(r, occmat2[i,], df = TRUE, cellnumbers = TRUE)
    values(r)[unlist(cells$cell)] <- 2 
  }
  
  ## masks the grids that are outside the polygon
  r <- mask(r, cntry)
  
  rpts <- rasterToPoints(r)
  rptsdf <- as.data.frame(rpts)
  colnames(rptsdf) <- c("lon", "lat", "occ")
  rptsdf$occ <- as.factor(rptsdf$occ)
  
  p <- ggplot(rptsdf) +
    geom_raster(aes(lon, lat, fill=occ)) +
    scale_fill_manual(values=c("grey95", "darkred", "steelblue"), guide="none") +
    geom_polygon(data = cntry, aes(long, lat, group = group), 
                 fill = NA, inherit.aes = FALSE) +
    geom_path(data = cntry, aes(long, lat, group = group),
              color = "black", inherit.aes = FALSE) +
    coord_equal() + 
    theme_map() + 
    theme(legend.title = element_text(size=12), 
          legend.text = element_text(size=12))+
    ggtitle(iso3c[[ii]])
  
  p
  plist[[ii]] <- p
# ggsave(paste0("figs/occ_grids_", iso3c, "_", tstamp(), ".png"), p, width=7.7, height=7.3, units="in")
}

p <- plist[[1]] + plist[[2]] + plist[[3]] + plist[[4]] +
  plot_layout(ncol = 2)

p
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# ggsave(paste0("plots/occ_grids_4_countries_", tstamp(), ".png"), p, width=7.7, height=7.3, units="in")
```

Number of points for the country

``` r
afss <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")
dculture <- d[grepl("culture", d$DIAGNOSTIC_METHOD_SUMMARY, ignore.case = TRUE),]
dother <-  d[!grepl("culture", d$DIAGNOSTIC_METHOD_SUMMARY, ignore.case = TRUE),]

# culture-based occurrence
dculture %>% 
  filter(!is.na(LONGITUDE)) %>% 
  distinct(LONGITUDE, .keep_all = TRUE) -> dculture2

# occurrence matrix
occmat <- data.frame(X = as.double(dculture2$LONGITUDE),
                     Y = as.double(dculture2$LATITUDE))
occmat <- occmat[!is.na(occmat$X) & !is.na(occmat$Y), ]
# conver the data frame to a spatial object to find the points wthin a polygon
xy <- occmat[,c(1,2)]
spdf <- SpatialPointsDataFrame(coords = xy, 
                               data = occmat,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

dother %>%
  mutate(LONGITUDE=as.double(LONGITUDE), LATITUDE=as.double(LATITUDE)) %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>% 
  distinct(LONGITUDE, .keep_all = TRUE) -> dother2

occmat2 <- data.frame(X = dother2$LONGITUDE, Y = dother2$LATITUDE)
occmat2 <- occmat2[!is.na(occmat2$X) & !is.na(occmat2$Y), ]

xy2 <- occmat2[,c(1,2)]
spdf2 <- SpatialPointsDataFrame(coords = xy2, 
                               data = occmat2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

df <- data.frame(
  country = c("Nigeria","Kenya", "Tanzania, United Republic of", "Ghana"),
  iso3c = NA,
  points_within = 0)

for (i in 1:nrow(df)) {
  cname <- df$country[i]
  df$iso3c[i] <- countrycode(cname, 'country.name', 'iso3c')
  cntry <- afss[afss$NAME_0 == cname,]
  pts_within <- st_contains(st_as_sf(cntry), st_as_sf(spdf))
  # length(pts_within)
  pts_within2 <- st_contains(st_as_sf(cntry), st_as_sf(spdf2))
  df$points_within[i] <- length(pts_within[[1]]) + length(pts_within2[[1]])
}
df
```

    ##                        country iso3c points_within
    ## 1                      Nigeria   NGA            65
    ## 2                        Kenya   KEN            24
    ## 3 Tanzania, United Republic of   TZA            23
    ## 4                        Ghana   GHA            19

## Table 1

``` r
d |> 
  distinct(STUDY_INFO, .keep_all = TRUE) %>% 
  group_by(DIAGNOSTIC_METHOD_SUMMARY) |> 
  tally(sort=T) -> tab

tab$percent <- 100 * tab$n / sum(tab$n)
tab
```

    ## # A tibble: 13 × 3
    ##    DIAGNOSTIC_METHOD_SUMMARY     n percent
    ##    <chr>                     <int>   <dbl>
    ##  1 Blood culture                91  39.7  
    ##  2 Clinical symptoms            66  28.8  
    ##  3 Widal test                   24  10.5  
    ##  4 Stool culture                14   6.11 
    ##  5 Culture                      12   5.24 
    ##  6 Unclear                       9   3.93 
    ##  7 Autopsy                       3   1.31 
    ##  8 CSF culture                   3   1.31 
    ##  9 Bone marrow culture           2   0.873
    ## 10 Rapid test                    2   0.873
    ## 11 Aspirate culture              1   0.437
    ## 12 Biochemical test              1   0.437
    ## 13 Urine culture                 1   0.437

Study types

``` r
d |> 
  distinct(STUDY_INFO, .keep_all = TRUE) %>% 
  group_by(STUDY_TYPE) |> 
  tally(sort=T)
```

    ## # A tibble: 7 × 2
    ##   STUDY_TYPE                n
    ##   <chr>                 <int>
    ## 1 Prospective study        93
    ## 2 Retrospective study      74
    ## 3 Cross-sectional study    33
    ## 4 Outbreak                 13
    ## 5 Case-control study        8
    ## 6 Case report               7
    ## 7 Literature review         1
