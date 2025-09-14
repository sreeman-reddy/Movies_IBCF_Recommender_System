Item-Based Collaborative Filtering
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(DT)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following objects are masked from 'package:data.table':
    ## 
    ##     dcast, melt
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
rmat <- read.csv("Rmat.csv", row.names=1, header = TRUE) #6040-by-3706 rating matrix with movies as columns and userid as rows

ratingurl <- "https://liangfgithub.github.io/MovieData/ratings.dat?raw=true" #ratings left by users for movies they watched
movieurl <- "https://liangfgithub.github.io/MovieData/movies.dat?raw=true" #dataset of movies 

ratings = read.csv(paste0(ratingurl, 'ratings.dat?raw=true'), sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines(paste0(movieurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
```

# System I: Recommendation Based on Popularity

Most Popular Movies: highly-rated (based on their average ratings)
movies that have received at least 1000 ratings.

``` r
small_image_url = "https://liangfgithub.github.io/MovieImages/"
top10_popular <- ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), 
            ave_ratings = round(mean(Rating), dig=3)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(ratings_per_movie > 1000) %>%
  top_n(10, ave_ratings) %>%
  mutate(Image = paste0('<img src="', 
                        small_image_url, 
                        MovieID, 
                        '.jpg?raw=true"></img>')) %>%
  arrange(desc(ave_ratings)) %>%
  select('MovieID', 'Title', 'Image') %>%
  datatable(class = "nowrap hover row-border", 
            escape = FALSE, 
            options = list(dom = 't',
                           scrollX = TRUE, autoWidth = TRUE))

top10_popular
```

![](item_based_cf_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
top50_popular <- ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), 
            ave_ratings = round(mean(Rating), dig=3)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(ratings_per_movie > 1000) %>%
  arrange(desc(ave_ratings)) %>%
  top_n(50, ave_ratings) %>% 
  mutate(MovieID = paste0('m', MovieID)) %>% 
  select('MovieID', 'Title')
write.csv(top50_popular, "popular50.csv")
```

# System II: Recommendation Based on IBCF

## Step 1: Normalize the rating matrix by centering each row

``` r
rmat_norm <- rmat - rowMeans(rmat, na.rm = TRUE)
```

## Step 2: Compute the (transformed) Cosine similarity among the 3,706 movies

``` r
cos_sim <- function(movie_i, movie_j) {
  common_users <- !is.na(movie_i) & !is.na(movie_j)
  n_common <- sum(common_users)
  
  if (n_common <= 2) {return(NA)}
  
  ratings_i <- movie_i[common_users]
  ratings_j <- movie_j[common_users]
  numer <- sum(ratings_i * ratings_j)
  denom_i <- sqrt(sum(ratings_i^2))
  denom_j <- sqrt(sum(ratings_j^2))
  
  if (denom_i == 0 || denom_j == 0) {return(NA)}
  
  similarity <- 0.5+0.5*(numer/(denom_i*denom_j))
  
  return(similarity)
}

n_movies <- ncol(rmat_norm)
S <- matrix(NA, nrow = n_movies, ncol = n_movies, dimnames = list(colnames(rmat_norm), colnames(rmat_norm)))

for (i in 1:n_movies) {
  for (j in i:n_movies) {
    S[i,j] <- cos_sim(rmat_norm[,i], rmat_norm[,j])
    if (i != j) {
      S[j,i] <- S[i,j]
    }
  }
}

diag(S) <- NA
```

``` r
top30 <- function(row) {
  non_na <- row[!is.na(row)]
  if (length(non_na) > 30) {
    top_30 <- sort(non_na, decreasing = TRUE)[1:30]
    row[!is.na(row)] <- NA
    row[names(top_30)] <- top_30
  }
  return(row)
}
S30 <- t(apply(S, 1, top30))
```

``` r
write.csv(S30, "Smat.csv")
```

``` r
subset <- c('m1', 'm10', 'm100', 'm1510', 'm260', 'm3212')
S[subset, subset]
```

    ##              m1       m10      m100 m1510      m260 m3212
    ## m1           NA 0.5121055 0.3919999    NA 0.7411482    NA
    ## m10   0.5121055        NA 0.5474583    NA 0.5343338    NA
    ## m100  0.3919999 0.5474583        NA    NA 0.3296943    NA
    ## m1510        NA        NA        NA    NA        NA    NA
    ## m260  0.7411482 0.5343338 0.3296943    NA        NA    NA
    ## m3212        NA        NA        NA    NA        NA    NA

## Step 4: Create a function named myIBCF

``` r
myIBCF <- function(newuser) {
  
  S <- read.csv("Smat.csv", row.names = 1)
  popularity_ranking <- read.csv("popular50.csv")
  
  predict_rating <- function(i) {
    S_i <- S[i, ]
    valid_indices <- which(!is.na(newuser) & !is.na(S_i))
    if (length(valid_indices) == 0) return(NA)
    
    num_sum <- sum(S_i[valid_indices] * newuser[valid_indices])
    denom_sum <- sum(S_i[valid_indices])
    
    if (denom_sum == 0) return(NA)
    return(num_sum / denom_sum)
  }
  
  predictions <- sapply(1:length(newuser), function(i) {
    if (is.na(newuser[i])) predict_rating(i) else NA
  })
  
  names(predictions) <- colnames(rmat)
  
  top_indices <- order(predictions, decreasing = TRUE)[1:10]
  top_predictions <- predictions[top_indices]
  
  
  if (sum(!is.na(top_predictions)) < 10) {
    na_count <- sum(is.na(top_predictions))
    popular_indices <- popularity_ranking$MovieID[!(popularity_ranking$MovieID %in% which(!is.na(newuser)))]
    fill_indices <- popular_indices[1:na_count]
    top_indices <- c(names(top_predictions[!is.na(top_predictions)]), fill_indices)
  }
  
  recommendations <- colnames(S[top_indices[1:10]])
  return(recommendations)
}
```

# Testing the function

Printing the top 10 recommendations for the following two users: -User
“u1181” from the rating matrix R -A hypothetical user who rates movie
“m1613” with 5 and movie “m1755” with 4

``` r
rmat <- read.csv("Rmat.csv", row.names=1, header = TRUE)

# myIBCF(c(3,rep(NA, ncol(rmat)-1)))
#myIBCF(rep(NA, ncol(rmat)))

myIBCF(rmat['u1181',])
```

    ##  [1] "m3732" "m749"  "m3899" "m1039" "m1235" "m1253" "m1734" "m1914" "m2082"
    ## [10] "m2361"

``` r
newuser <- rep(NA, ncol(rmat))
m1613idx <- which(colnames(rmat) == "m1613")
m1755idx <- which(colnames(rmat) == "m1755")
newuser[m1613idx] <- 5
newuser[m1755idx] <- 4

myIBCF(newuser)
```

    ##  [1] "m2836" "m338"  "m3466" "m1017" "m1100" "m1468" "m1541" "m158"  "m1688"
    ## [10] "m1752"
