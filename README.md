# Description
R function to webscrape AFL data from AFLtables.com. This function is intended to be used with R studio.
## Packages you will need
You will need some packages to use this function. Use this code snippet to automatically install the necessary packages

```
load.libraries <- c( 'FeatureHashing', 'Matrix','plyr','dplyr','XML','stringr','rvest','httr','data.table')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
```

## Basic use:
As a basic use, you can scrape all of the data from any year by specifying the `yearBack` variable. 

For example, to scrape all data form year 2000 to now would be:
```
PlayerTable<-updateAFLData(yearBack = 2000)
```
The default year is 2000. 

## Using it to update data

Scraping can be a slow process. The real power of updateAFLData is it's abilty to take in previously scraped data, assess it for missing matches, and only scrape those matches which are missing in the data set. This can be used in a workflow that relies on weekly updates to the data as the season progresses. 

Consider that you have already scraped data from the year 2000 to the current round and saved it as CollectedData.csv. After the round has finished, you can use updateAFLdata to update your dataset. An example workflow is as follows:

```
# get current Data
PreviousCollectedData<-fread('CollectedData.csv'),data.table = F) 
  
# Update 
updatedData<-updateAFLData(PreviousCollectedData,yearBack = 2000)

# resave
write.csv(updatedData,file=('CollectedData.csv'))

```


