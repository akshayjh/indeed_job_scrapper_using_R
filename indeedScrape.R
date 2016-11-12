#####################################################################################
# Project name: Scraping job portal from indeed
#
# Author :  Anonymous31173
# email:    ymous337@gmail.com
# Note:
# Install packages of Rvest , xml, httr , ggmap, mapdata, mapproj, ggplot, knitr
# run the below given command to install above mentioned packages at once.
#       install.packages("rvest","xml", "httr", "ggmap", "mapdata", "mapproj", "ggplot", "knitr")
#   Important note: You need to have an internet connection to run this code. 
######################################################################################


library(rvest)
# Submit the form on indeed.com for a job description and location using html_form() and set_values()
query = "data science"
loc = "New York"
session <- html_session("http://www.indeed.com")  # you are fetching the url here.
session
form <- html_form(session)[[1]]
form
form <- set_values(form, q = query, l = loc)
form

# Version 1 of our submit_form function
submit_form2 <- function(session, form){
  library(XML)
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q
  q <- paste(q, collapse = '&')
  q
  q <- gsub(" ", "+", q)
  q
  url <- paste(url, q, sep = '')
  url
  html_session(url)
}

# Version 2 of our submit_form function
library(httr)
# Appends element of a list to another without changing variable type of x
# build_url function uses the httr package and requires a variable of the url class
appendList <- function (x, val)
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}

# Simulating submit_form for GET requests
submit_geturl <- function (session, form)
{
  query <- rvest:::submit_request(form)
  query$method <- NULL
  query$encode <- NULL
  query$url <- NULL
  names(query) <- "query"
  
  relativeurl <- XML::getRelativeURL(form$url, session$url)
  basepath <- parse_url(relativeurl)
  
  fullpath <- appendList(basepath,query)
  fullpath <- build_url(fullpath)
  fullpath
}


# Submit form and get new url
session1 <- submit_form2(session, form)

 # Get reviews of last company using follow_link()
session2 <- follow_link(session1, css = "#more_9 li:nth-child(3) a")
session2
reviews <- session2 %>% html_nodes(".description") %>% html_text()
reviews





# Get average salary for each job listing based on title and location
salary_links <- html_nodes(session1, css = "#resultsCol li:nth-child(2) a") %>% html_attr("href")
salary_links
salary_links <- paste(session$url, salary_links, sep='')
salary_links
salaries <- lapply(salary_links, . %>% read_html() %>% html_nodes("#salary_display_table .salary") %>% html_text())
salaries
salary <- unlist(salaries)
salary
# Store web url
data_sci_indeed <- session1
data_sci_indeed
# Get job titles
job_title <- data_sci_indeed %>% 
  html_nodes("[itemprop=title]") %>%
  html_text()
job_title
# Get companies
company <- data_sci_indeed %>%
  html_nodes("[itemprop=hiringOrganization]") %>%
  html_text()
company
# Get locations
location <- data_sci_indeed %>%
  html_nodes("[itemprop=addressLocality]") %>%
  html_text()
location
library(ggmap)
library(mapdata)
library(mapproj)
loc<-geocode(location)
loc
df <- as.data.frame(cbind(loc$lon,loc$lat))
mapgilbert <- get_map(location = c(lon = mean(df$V1), lat = mean(df$V2)), zoom = 15, maptype = "satellite", scale = 2)
ggmap(mapgilbert) + geom_point(data = df, aes(x = V1, y = V2, fill = "red", alpha = 0.8), size = 5, shape = 21) +guides(fill=FALSE, alpha=FALSE, size=FALSE)

#map<-qmap(location = 'new york', zoom= 14, maptype= "satellite" )
#map
# Get descriptions
description <- data_sci_indeed %>%
  html_nodes("[itemprop=description]") %>%
  html_text()
description
# Get the links
link <- data_sci_indeed %>%
  html_nodes("[itemprop=title]") %>%
  html_attr("href")
link
link <- paste('[Link](https://www.indeed.com', link, sep='')
link
link <- paste(link, ')', sep='')
link
indeed_jobs <- data.frame(job_title,company,location,description,salary,link)
indeed_jobs
library(knitr)
kable(indeed_jobs, format = "html")
############################THANK YOU###########################################
