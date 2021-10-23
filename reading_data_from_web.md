reading\_data\_from\_web
================
Kaila Boyer
10/23/2021

## NSDUH data

``` r
url <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

drug_use_html <- read_html(url)

drug_use_df <- drug_use_html %>% 
                 html_table() %>% 
                 first()  %>% 
                 slice(-1)
```

## Learning Assessment 1

``` r
nyc <- read_html("https://www.bestplaces.net/cost_of_living/city/new_york/new_york") %>% 
  html_table(header = TRUE) %>% 
  first()
```

## CSS Selectors

Get star wars data

``` r
sw_url <- "https://www.imdb.com/list/ls070150896/"

sw_html <- 
  read_html(sw_url) 

sw_titles <- sw_html %>% 
                 html_elements(".lister-item-header a") %>% 
                 html_text()

sw_revenue <- sw_html %>% 
                 html_elements(".text-small:nth-child(7) span:nth-child(5)") %>% 
                 html_text()

sw_df <- 
          tibble(
            title = sw_titles, 
            revenue = sw_revenue
          )
```

## Learning Assessment 2 - Napoleon Dynamite

``` r
dynamite_url <- "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"


dynamite_html <- read_html(dynamite_url)

dynamite_reveiw_titles <- 
  dynamite_html %>% 
  html_elements(" .a-text-bold span") %>% 
  html_text()

dynamite_stars <- 
  dynamite_html %>% 
  html_elements("#cm_cr-review_list .review-rating") %>% 
  html_text()
```
