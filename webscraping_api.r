

library(jsonlite)
library(httr)
library(tidyverse)


#sequences<-3:5
#represents page numbers
pages<-1:231
#go to url and count number of pages
newurl<- vector("list", length(pages)) #* length(sequences))
#ensures the list of urls is the appropriate length
#index<-1
#basic sequence=1 in the url indicates front pages only, sequence is the page number
for(i in 1:length(pages)){
#took forever but figured out how to nest the loops in order to get loop to run through all pages first then switch the sequence then run through pages again etc had to ask AI appropriate order of nesting
#put pages for loop first because i need to run through all the pages before switching sequence number

 #AI was telling me to use seq_along instead of length but that literally didn't work?
    newurl[[i]]<-paste0("https://chroniclingamerica.loc.gov/search/pages/results/?date1=1756&date2=1783&searchType=advanced&language=&lccn=&proxdistance=5&state=New+Hampshire&rows=20&ortext=&proxtext=&phrasetext=&andtext=&dateFilterType=yearRange&page=", pages[i],"&format=json")
 
  #index<-index+1
  #this ensures that each url is saved instead of overwritten, added this after nesting the loops because previously newurl[i] was running through set pages but nesting it increases the urls in a way that [i] can't account for
 #no matter what I change the sequences or pages to, index ensures I will always have the correct number of urls
 }

#for(s in 1:length(sequences)){
#this loops is meant to get all the urls for all the pages
#to get new url for new year just change the date1 and date2 in url then go to url and look at pages and change pages variable to match
results<- vector("list", length(newurl))
#turns the results into a list 

for(i in 1:length(newurl)){
    json_stuff<-GET(newurl[[i]])
    if( json_stuff$status_code == 200){
        #only pulling json info if successful status code of 200
        results[[i]] <-content(json_stuff, "text")
        #ensures I don't overlap the api
    } else{
        results[[i]]<-NULL
        #making bad results NULL
    }
    Sys.sleep(5)
    #pauses whether or not there is failure to ensure I don't overload api
}
results <- Filter(Negate(is.null), results)
#this removes null results from the list I made so the json data can be organized
#the new null results i believe were large pictures on pages or something like that based on cursory debugging
organized_json<-list()
#turning my results into a list
for(i in 1:length(results)){
    json_data<-fromJSON(results[[i]])
    organized_json[[i]]<-json_data$items
}

df <- bind_rows(organized_json)



df_clean <- df %>% mutate(across(where(is.list), ~ sapply(.x, function(x) {
  if (length(x) == 0) return("") else paste(x, collapse = "; ")
})))

write.csv(df_clean, "C:/Users/anton/Desktop/NHgazette1756-1783.csv", row.names = FALSE)



