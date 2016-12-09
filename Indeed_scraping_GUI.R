library(FSelector)
library(MASS)

library(gWidgets)
library(gWidgetsRGtk2)
########################################GUI CODE########################################

win<-gwindow("Indeed Scraping Tool")

group<-ggroup(container = win)
left_group<-ggroup(container =  group,horizontal=FALSE)
right_group<-ggroup(container =  group,horizontal=FALSE)

frame6<-gframe("Indeed Scraper",cont=left_group,expand=TRUE)
size(frame6)<-c(400,450)
tbl=glayout(container =  frame6)
tbl[4,1] = glabel(text = "Web URL:", container = tbl)
tbl[4,2] = glabel(text = "http://www.indeed.com", container = tbl)

tbl[6,1] = glabel(text = "Designation: ",  container = tbl)
tbl[6,2] =gedit("",container = tbl)

tbl[8,1]<- glabel(tex="City: ",container = tbl)
tbl[8,2]= gedit("",container = tbl)

tbl[10,1]<-clear<<-gbutton("CLEAR", container = tbl)
tbl[10,2]<-submit<<-gbutton("SUBMIT", container = tbl)

tbl[12,1]<-get_jobtitle<<-gbutton("GET JOB TITLE",container=tbl)
tbl[12,2]<-get_salary<<-gbutton("GET SALARY",container=tbl)

tbl[14,1]<-get_jd<<-gbutton("JOB DESRIPTION",container=tbl)
#tbl[14,2]<-get_salary1<<-gbutton("SHOW ALL DATA",container=tbl)

tbl[14,2]<-show_map<<-gbutton("SHOW MAP",container=tbl)

########################Result#################
frame1<-gframe("Results",container =  right_group,expand=TRUE)
size(frame1)<-c(700,200)

frame2<-gframe("title",container=frame1,expand=TRUE)

frame3<-gframe("Salary",container=frame1,expand=TRUE)
frame4<-gframe("Description",container=frame1,expand=TRUE)

frame5<-gframe("ALL DATA",container=right_group,expand=TRUE)
size(frame5)<-c(700,250)

################function definition################


  addHandlerClicked(submit,handler=function(h,...)
  { 
    
    library(rvest)
    # Submit the form on indeed.com for a job description and location using html_form() and set_values()
    query = svalue(tbl[6,2])
    loc = svalue(tbl[8,2])
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
    #install.packages("httr")
    #library(httr)
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
    location <<- data_sci_indeed %>%
      html_nodes("[itemprop=addressLocality]") %>%
      html_text()
    location
    
    #install.packages("ggmap")
    library(ggmap)
    #install.packages("mapdata")
#     library(mapdata)
#     #install.packages("mapproj")
#     library(mapproj)
#     loc<-geocode(location)
#     loc
#     df <- as.data.frame(cbind(loc$lon,loc$lat))
#     mapgilbert <- get_map(location = c(lon = mean(df$V1), lat = mean(df$V2)), zoom = 15, maptype = "satellite", scale = 2)
#     ggmap(mapgilbert) + geom_point(data = df, aes(x = V1, y = V2, fill = "red", alpha = 0.8), size = 5, shape = 21) +guides(fill=FALSE, alpha=FALSE, size=FALSE)
#     
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
    indeed_jobs <<- data.frame(job_title,company,location,description,salary,link)
   print( indeed_jobs)
   
   k<<-as.vector(indeed_jobs[,1])
    l<<-as.vector(indeed_jobs[,4])
    m<<-as.vector(indeed_jobs[,5])
    fnl_tbl<<-gtable( indeed_jobs)
    add(frame5,fnl_tbl,expand=TRUE)
    
  })
##########################################################################

addHandlerClicked(get_jobtitle,handler=function(h,...)
{
 # indeed_jobs
  job_tbl1<<-gtable(k)
  add(frame2,job_tbl1,expand=TRUE)
  
})


###################################################################################
addHandlerClicked(get_salary,handler=function(h,...)
{
  
  job_tbl2<<-gtable(m)
  add(frame3,job_tbl2,expand=TRUE)
  
})


#####################################################################################
addHandlerClicked(get_jd,handler=function(h,...)
{
  # indeed_jobs 
  job_tbl3<<-gtable(l)
  add(frame4,job_tbl3,expand=TRUE)
  
})







########################## Map ###############################################
#ggraphics(cont=right_group)
addHandlerClicked(show_map,handler=function(h,...)
{
# win2<-gwindow("Location Map, showing Lattitude and Longitude")
# 
# group1<-ggroup(container = win2)
# frame4<-gframe("Indeed Scraper",expand=TRUE)
# size(frame4)<-c(400,450)

library(ggmap)
#install.packages("mapdata") library(mapdata) #install.packages("mapproj") 
library(mapproj) 
loc<-geocode(location) 
df <-na.omit(as.data.frame(cbind(loc$lon,loc$lat)))
mapgilbert <- get_map(location = c(lon =mean(df$V1), lat = mean(df$V2)), zoom = 15, maptype = "satellite", scale = 2) 

ggmap(mapgilbert) + geom_point(data = df, aes(x = V1, y = V2, fill = "red",
alpha = 0.8), size = 5, shape = 21) +guides(fill=FALSE, alpha=FALSE,size=FALSE)
X11()
plot(qmap(location = 'new york', zoom= 14, maptype= "satellite" ))

})


###################clear###################################


addHandlerClicked(clear,handler=function(h,...)
{
  tbl[6,2]<-gedit("",container = tbl,coerce.with = as.numeric)
  tbl[8,2]<-gedit("",container = tbl,coerce.with = as.numeric)
  
  delete(frame2,job_tbl1)
  delete(frame3,job_tbl2)
  delete(frame4,job_tbl3)
 
  delete(frame5,fnl_tbl)
})