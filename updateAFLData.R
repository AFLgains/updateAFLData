updateAFLData<-function(currentData = NULL,yearBack=2000){
  
  ## WebScraping and Cleaning --------------
  
  now<-year(Sys.Date())  # get now
  seasonstocompile<-c(yearBack:now) 
  seasonData<-list()
  
  for (j in 1:length(seasonstocompile)){
    url <- paste0("http://afltables.com/afl/seas/",seasonstocompile[j],".html#10")
    doc <-htmlParse(rawToChar(GET(url)$content))
    links <- unname(xpathSApply(doc, "//a/@href"))
    url_list<-unique(links[grepl(paste0("games/",seasonstocompile[j]), links)])
    url_list<-gsub("../stats/","",url_list)
    
    #storage list
    s<-list()
   
    #find which dates are missing in the current year
    if (!is.null(currentData) ){
      datesUsed<-as.Date(unique(currentData$Date))
      datesToScrape<-gsub(paste0("games/",seasonstocompile[j],"/"),"",url_list)
      datesToScrape<-gsub(".html","",datesToScrape)
      datesToScrape<-substr(datesToScrape, nchar(datesToScrape)-8+1, nchar(datesToScrape))
      datesToScrape<-as.Date(datesToScrape, "%Y%m%d")
      
      #..and only scrape these ones
      gamesToScrape<-which(!datesToScrape %in% datesUsed)
      
      if (length(gamesToScrape)==0){ message(paste('No data scraped from season,',seasonstocompile[j],': Data up-to-date')) }
      
    } else {
      gamesToScrape<-1:nGames  
    }
    
    for (i in gamesToScrape)
    {
      message(paste0('Season ',seasonstocompile[j],', Game ',i))
      url <- paste0("http://afltables.com/afl/stats/",url_list[i])
      doc <-htmlParse(rawToChar(GET(url)$content))
      
      links <- unlist(xpathSApply(doc, "//td",fun=xmlValue))
      
      d<-strsplit(links[[2]]," ")[[1]]
      ind<-which(d %in% c("Round:","Venue:","Date:","Attendance:")==T)
      Round_append<-suppressWarnings(as.numeric(d[ind[1]+1]))
      Venue_append<-paste0(d[(ind[2]+1):(ind[3]-1)],sep=" ",collapse='')
      Date_append<-as.Date(d[ind[3]+2],"%d-%b-%Y")
      
      # Download the xml and then read using httr package
      tables <- GET(url)
      tables <- readHTMLTable(rawToChar(tables$content))
      
      z<-data.frame(tables[1])
      z[z=="Â"]<-0
      
      size <-unname(unlist(lapply(tables,ncol)))
      tab_index<-which(size==25)
      
      Home<-data.frame(tables[tab_index[1]+1])
      Home$Team<-z[1,1]
      Home$Opposition<-z[2,1]
      Home$Season<-seasonstocompile[j]
      Home$Score<-as.numeric(strsplit(as.character(z[1,5]), "\\.")[[1]][3])
      Home$Margin<-as.numeric(strsplit(as.character(z[1,5]), "\\.")[[1]][3])-as.numeric(strsplit(as.character(z[2,5]), "\\.")[[1]][3])
      Home$WinLoss<-ifelse(Home$Margin<0,"L",ifelse(Home$Margin>0,"W","D"))
      Home$Status<-"Home"
      
      Away<-data.frame(tables[tab_index[2]+2])
      Away$Team<-z[2,1]
      Away$Opposition<-z[1,1]
      Away$Season<-seasonstocompile[j]
      Away$Score<-as.numeric(strsplit(as.character(z[2,5]), "\\.")[[1]][3])
      Away$Margin<-as.numeric(strsplit(as.character(z[2,5]), "\\.")[[1]][3])-as.numeric(strsplit(as.character(z[1,5]), "\\.")[[1]][3])
      Away$WinLoss<-ifelse(Away$Margin<0,"L",ifelse(Away$Margin>0,"W","D"))
      Away$Round<-strsplit(links[[2]]," ")[[1]][2] 
      Away$Status<-"Away"
      
      temp<-rbind.fill(Home,Away)
      temp$Round <- paste0("R",Round_append)
      temp$Venue <- Venue_append
      temp$Date  <- Date_append
      
      s[[i]]<-temp
    }
    
    df <- ldply(s, data.frame)
    
    # Now to clean the data - rename the colum headings and ensuring that there are no leading spaces or anything like that
    names(df)
    if (nrow(df)>0){
    df<-dplyr::select(df, 
                      Player=NULL.Player,
                      Kicks=NULL.KI ,
                      Marks=NULL.MK ,
                      Handballs=NULL.HB ,
                      Disposals=NULL.DI ,
                      Goals= NULL.GL  ,
                      Behinds       =        NULL.BH,
                      Hitouts     =         NULL.HO,
                      Tackles     =  NULL.TK,
                      Rebound50s    =      NULL.RB,
                      Inside50s    =   NULL.IF,
                      Clearances    =          NULL.CL,
                      Clangers     =     NULL.CG,
                      FreesFor      =        NULL.FF,
                      FreesAgainst      =       NULL.FA,
                      ContendedPossessions   =          NULL.CP,
                      UncontendedPossessions       =         NULL.UP,
                      ContestedMarks   =   NULL.CM,
                      MarksInside50    =        NULL.MI,
                      OnePercenters    =       NULL.1.,
                      Bounces         =       NULL.BO,
                      GoalAssists     =          NULL.GA,
                      PercentPlayed    =         NULL..P,
                      Score,Margin,Season,WinLoss,Round,Venue,Date,Team ,Opposition,Status)
    
    # Turn all missing data into 0
    df[is.na(df)]<- 0
    
    # save the results
    seasonData[[j]]<-df

    } else {
  
    seasonData[[j]]<-currentData # this will just copy the current data which will be deleted in the end
          
    }
    
  }
  
  
  # combine
  updatedData<-ldply(seasonData)
  
  #Convert everything to a character
  updatedData <- data.frame(lapply(updatedData, as.character), stringsAsFactors=FALSE)
  
  # Get rid of the A thing
  updatedData[updatedData=="Â"]<-0
  
  # turn numeri columns into numeric
  colChar<-c("Player","Round","Venue","Date","Team","Opposition","Status","WinLoss")
  colNumeric<-colnames(updatedData)[!colnames(updatedData) %in% colChar]
  
  datNumeric<-updatedData[,colNumeric]
  datChar<-updatedData[,colChar]
  
  updatedData<-cbind(data.frame(lapply(datNumeric,as.numeric),stringsAsFactors = FALSE),datChar)
  
  # get rid of any NAs
  updatedData[is.na(updatedData)]<- 0
  
  
  # Get rid of finals matches
  updatedData<-filter(updatedData,Round!="EF",Round!="GF",Round!="PF",Round!="SF",Round!="QF",Round!="RNA")
  
  
  
  if(!is.null(currentData)){
    output<-rbind.fill(currentData,updatedData)
    output<-output[!duplicated(output),] #get rid of duplicates
  } else {
    output<-updatedData
    output<-output[!duplicated(output),] #get rid of duplicates
  }
  
  ##  return the output ---------------
  return(output)
}
