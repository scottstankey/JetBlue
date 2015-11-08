regions <- read.csv("~/Dropbox/JetBlue/GeographicRegion-stg.csv", stringsAsFactors=FALSE)
desttype <- read.csv("~/Dropbox/JetBlue/DestinationType-stg.csv", stringsAsFactors=FALSE)
citypair <- read.csv("~/Dropbox/JetBlue/CityPairDestinationType-stg.csv", stringsAsFactors=FALSE)
airports <- read.csv("~/Dropbox/JetBlue/AirportRegion-stg.csv", stringsAsFactors=FALSE)
markets <- read.csv("~/Dropbox/JetBlue/MarketGroup-stg.csv", stringsAsFactors=FALSE)
maccodes <- read.csv("~/Dropbox/JetBlue/MAC-stg.csv", stringsAsFactors=FALSE)
Fares <- read.csv("~/Dropbox/JetBlue/Fares.Fare-stg.csv", stringsAsFactors=FALSE)
Faress = Fares[,-c(1,5)]

dateconverter = function(x) {
  val = as.numeric(strsplit(strsplit(x, "/")[[1]][3], " ")[[1]][1])*1e8 + 
    as.numeric(strsplit(x, "/")[[1]][1])*1e6 + 
    as.numeric(strsplit(x, "/")[[1]][2])*1e4 + 
    as.numeric(strsplit(strsplit(strsplit(x, "/")[[1]][3], " ")[[1]][2],":")[[1]][1])*1e2 + 
    as.numeric(strsplit(strsplit(strsplit(x, "/")[[1]][3], " ")[[1]][2],":")[[1]][2])
  return(val)
}

cityinfodict = cbind(paste(citypair[,1],citypair[,2]),citypair[,3])


cityinfovec = NULL
for(name in names(table(cityinfodict[,1])))
{
  val = c(FALSE,F,F,F,F)
  if(19 %in% cityinfodict[which(cityinfodict[,1] == name),2])
  {
    val[1] = TRUE
  }
  if(21 %in% cityinfodict[which(cityinfodict[,1] == name),2])
  {
    val[2] = TRUE
  }
  if(22 %in% cityinfodict[which(cityinfodict[,1] == name),2])
  {
    val[3] = TRUE
  }
  if(23 %in% cityinfodict[which(cityinfodict[,1] == name),2])
  {
    val[4] = TRUE
  }
  if(24 %in% cityinfodict[which(cityinfodict[,1] == name),2])
  {
    val[5] = TRUE
  }
  cityinfovec = cbind(cityinfovec, val)
}
colnames(cityinfovec) = names(table(cityinfodict[,1]))
citylookup = t(cityinfovec)


FaressUpdate = cbind(Faress, 
              Faress[,"DollarFare"] + Faress[,"DollarTax"], 
              Faress[,"PointsFare"] + Faress[,"PointsTax"],
              sapply(Faress$FlightDate,dateconverter),
              paste(Faress$Origin,Faress$Destination))

colnames(FaressUpdate) = c(colnames(Faress),"Dollars","Points","Date", "Trip")

citynames = rownames(citylookup)

destfactors = NULL
for(i in 1:nrow(FaressUpdate))
{
  vec = c(F,F,F,F,F)
  if(length(which(citynames == FaressUpdate$Trip[i])) > 0) {
    vec = citylookup[which(citynames == FaressUpdate$Trip[i]),]
  }
  else {
    print("YOOOOOOOOOOO")
  }
  destfactors = rbind(destfactors,vec)
  print(i)
}

FaressUpdate = cbind(FaressUpdate, destfactors)

colnames(FaressUpdate)  = c(colnames(Faress),"Dollars","Points","Date", "Trip", "Family", "Nightlife", "Exploration", "Romance", "Beach")

regionvec= NULL
for(i in 1:nrow(FaressUpdate))
{
  vec = NA
  if(length(which(airports[,1] == FaressUpdate$Destination[i])) > 0)
  {
    vec = airports[which(airports[,1] == FaressUpdate$Destination[i]),2]
  }
  regionvec = c(regionvec,vec)
  print(i)
}

FaressUpdate = cbind(FaressUpdate, regionvec)

colnames(FaressUpdate) = c(colnames(FaressUpdate)[-ncol(FaressUpdate)],"Region")

marketvec= NULL
for(i in 1:nrow(FaressUpdate))
{
  vec = regions[FaressUpdate$Region[i],2]
  marketvec = c(marketvec,vec)
  print(i)
}

FaressUpdate = cbind(FaressUpdate, marketvec)
colnames(FaressUpdate) = c(colnames(FaressUpdate)[-ncol(FaressUpdate)],"Market")

for(i in 1:nrow(FaressUpdate)) {
  if (is.na(FaressUpdate[i,"Region"])) { 
    FaressUpdate[i,"Region"] = 0
  }
  if (is.na(FaressUpdate[i,"Market"])) { 
    FaressUpdate[i,"Market"] = 0
}
print(i)
}



#TODO Need to check whether there are cheap connecting flights. This will be hard.
#TODO Need to add a lazy val for start and end city as you might not want to choose one

#"Family", "Nightlife", "Exploration", "Romance", "Beach"

user1 = scripter("ORD", NULL, 
                 1601010000, 1603310000, 
                 500, 
                 rep(1, 22),
                 TRUE, FALSE, FALSE, NULL,
                 c(0,5,0,3,1),
                 c(1,5,0,0,0,3))

user2 = scripter("LAX", NULL, 
                 1602020000, 1602029999,
                 300,
                 c(rep(0,13), 5, rep(0,8)),
                 TRUE, TRUE, TRUE, NULL,
                 c(3,0,3,3,0),
                 c(0,0,0,0,5,0))


user3 = scripter("JFK", NULL, 
                 0, 10000000000000000,
                 150,
                 rep(1,22),
                 TRUE, TRUE, FALSE, FALSE,
                 c(3,0,1,0,5),
                 c(3,5,0,0,0,5)
                 )

#For User 2: Charles should notify me that he is out of swipes. Then, I should go back and widen the date range, I would think. 

#user3 = scripter(NULL,)


fares = fares[which(fares$FareType == "LOWEST"),]

fares = fares[,-1]


scripter = function (startdest, enddest, #Done
                     startdate, enddate, #Done
                     budget, #Done
                     region, #Rating / Done
                     nonstop, prefernonstop, connecting, international, 
                     activites, #Rating / Done
                     markets) #Rating / Done
{
  #TODO I want to add activations. Like people shouldn't have to click buttons on Charles UI to get to this
  # This gets us the proper inds that we needs in the most efficient way I think. Might be a faster way to do and statements
  fares = fares[which(fares$FareType == "LOWEST"),]
  budgetlist = originlist = startdatelist = enddatelist = domesticroutelist = internationalroutelist = regionlist =  c(1 : nrow(fares))
  if(nonstop) {
    if(is.null(budget) == FALSE) {budgetlist = which(fares$Dollars <= budget)}
    if(is.null(startdest) == FALSE) {originlist = which(fares$Origin == startdest)}
    if(is.null(startdate) == FALSE) {startdatelist = which(fares$Date >= startdate)} #Find some way to get today's date???
    if(is.null(enddate) == FALSE) {enddatelist = which(fares$Date <= enddate)} #Display flights soon first???
    if(is.null(international) == FALSE && international == TRUE){domesticroutelist = which(fares$IsDomesticRoute == FALSE)}
    if(is.null(international) == FALSE && international == FALSE){internationalroutelist = which(fares$IsDomesticRoute == TRUE)}
    inds = Reduce(intersect, list(budgetlist, originlist, startdatelist, enddatelist, domesticroutelist, internationalroutelist))
    tmp = fares[inds,]
    layover = rep(NA, nrow(tmp))
    flightdate1 = rep(NA, nrow(tmp))
    tmpfinal = cbind(tmp[,"Origin"], layover,tmp[,c("FlightDate", 
                                                    "FareType", "Date","Family", "Nightlife", "Exploration", 
                                                    "Romance", "Beach","Destination")], flightdate1, 
                     tmp[,c("Region", "Market", "DollarFare", "DollarTax", "Dollars")])
    colnames(tmpfinal) = c("Origin", "Layover", "FlightDate", "FareType", "Date", "Family", "Nightlife", "Exploration",
                           "Romance", "Beach", "Destination", "ConnectingFlightDate", "Region", "Market", "DollarFare", "DollarTax", "Dollars")
    
    tmpfinal = tmpfinal[,c("Origin", "Layover", "Destination", "FlightDate", "FareType", "Date", "ConnectingFlightDate",
                           "DollarFare", "DollarTax", "Dollars","Family", "Nightlife", "Exploration",
                           "Romance", "Beach" ,"Region", "Market")]
  }
  if(connecting) {
    if(is.null(budget) == FALSE) {budgetlist = which(fares$Dollars <= budget)}
    if(is.null(international) == FALSE && international == TRUE){internationalroutelist = which(fares$IsDomesticRoute == FALSE)}
    if(is.null(international) == FALSE && international == FALSE){domesticroutelist = which(fares$IsDomesticRoute == TRUE)}
    if(is.null(startdate) == FALSE) {startdatelist = which(fares$Date >= startdate)} #Find some way to get today's date???
    if(is.null(enddate) == FALSE) {enddatelist = which(fares$Date <= enddate)} #Display flights soon first???
    inds = Reduce(intersect, list(budgetlist, startdatelist, enddatelist, domesticroutelist, internationalroutelist))
    maxflightprice = budget - min(fares[inds, "Dollars"] )
    inds = inds[which(fares[inds,"Dollars"] < maxflightprice)]
    
    regionlist = NULL
    for(i in inds) {
      vec = FALSE
      if (length(region[fares$Region[i]]) > 0 && region[fares$Region[i]] > 0) {
        vec = TRUE
      }
      regionlist = c(regionlist, vec)
    }
    regionlist = inds[regionlist]
    
    
    marketlist = NULL
    for(i in inds) {
      vec = FALSE
      if (length(markets[fares$Market[i]]) > 0 && markets[fares$Market[i]] > 0) {
        vec = TRUE
      }
      marketlist = c(marketlist, vec)
    }
    marketlist = inds[marketlist]
    
    enddestlist = Reduce(intersect, list(union(regionlist, marketlist), inds))
    
    if(is.null(startdest) == FALSE) {originlist = which(fares$Origin == startdest)}
    startdestlist = Reduce(intersect, list(startdatelist, enddatelist,originlist))
    
    enddestlistupdate = enddestlist[which(fares[enddestlist,"Dollars"] < maxflightprice)]
    startdestlistupdate = startdestlist[which(fares[startdestlist,"Dollars"] < maxflightprice)]
    
    starttmp = fares[startdestlistupdate,]
    endtmp = fares[enddestlistupdate,]
    
    connectingtmp = NULL
    for(i in 1:nrow(starttmp)) {
      firstleg = starttmp[i,]
      joind = NULL
      connectorigin = starttmp[i, "Destination"] 
      time = starttmp[i,"Date"]
      firstflight = starttmp[i, "Dollars"]
      culled = endtmp[intersect(which(endtmp$Date > time), which(endtmp$Origin == connectorigin)),]
      culled = culled[which(culled$Dollars < {budget - firstflight}),]
      joind = cbind(do.call(rbind, replicate(nrow(culled), firstleg, simplify=FALSE)),culled) 
      
      connectingtmp = rbind(connectingtmp,joind)
    }
    pastefunc = function(x) {paste(x,1,sep= "")}
    colnames(connectingtmp) = c(colnames(tmp), sapply(colnames(tmp), pastefunc))
    
    #need to add the fares
    
    connectingfinal = cbind(connectingtmp[,c("Origin", "Destination", "FlightDate", "FareType", "Date", 
                                             "Family1", "Nightlife1", "Exploration1", "Romance1", "Beach1", "Destination1",
                                             "FlightDate1", "Region1", "Market1")], connectingtmp[,"DollarFare"] + connectingtmp[,"DollarFare1"],
                            connectingtmp[,"DollarTax"] + connectingtmp[,"DollarTax1"], connectingtmp[,"Dollars"] + connectingtmp[,"Dollars1"])
    colnames(connectingfinal) = c("Origin", "Layover", "FlightDate", "FareType", "Date", 
                                  "Family", "Nightlife", "Exploration", "Romance", "Beach", "Destination",
                                  "ConnectingFlightDate", "Region", "Market", "DollarFare", "DollarTax", "Dollars")
    
    connectingfinal = connectingfinal[,c("Origin", "Layover", "Destination", "FlightDate", "FareType", "Date", "ConnectingFlightDate",
                                         "DollarFare", "DollarTax", "Dollars","Family", "Nightlife", "Exploration",
                                         "Romance", "Beach" ,"Region", "Market")]
  }
  
  
  final = rbind(tmpfinal, connectingfinal)
  
  IsLayover = rep(T, nrow(final))
  IsLayover[which(is.na(final[,"Layover"]))] = FALSE
  
  final = cbind(final, IsLayover)
  # Ok now lets rate this shit
  
  
  actscore = NULL
  for(i in 1:nrow(final))
  {
    vec = 0
    vec = sum(activities * final[i,c("Family", "Nightlife", "Exploration", "Romance", "Beach")])
    actscore = c(actscore, vec)
    print(i)
  }
  
  marketscore = NULL
  for(i in 1:nrow(final))
  {
    if(final[i,"Market"] > 0){
      vec = markets[final[i,"Market"]]
    }
    else {
      vec = 0
    }
    marketscore = c(marketscore, vec)
    print(i)
  }
  
  regionscore = NULL
  for(i in 1:nrow(final))
  {
    if(final[i,"Region"]) {
      vec = region[final[i,"Region"]] 
    }
    else {
      vec= 0
    }
    regionscore = c(regionscore, vec)
    print(i)
  }
  
  budgetscore = exp((max(final[,"Dollars"]) - final[,"Dollars"]) / budget)
  budgetscore = budgetscore - min(budgetscore)
  
  layoverscore = prefernonstop * !final[,"IsLayover"]
  
  newfinal = cbind(final, actscore / sum(activities), 
                   marketscore / sum(markets), 
                   if(sum(region) > 0){regionscore / sum(region)}else{0}, 
                   budgetscore / {max(budgetscore) - min(budgetscore)},
                   layoverscore,
                   actscore / sum(activities)  + marketscore / sum(markets) + layoverscore + 
                     budgetscore / {max(budgetscore) - min(budgetscore)} + 
                     if(sum(region) > 0){regionscore / sum(region)}else{0})
  
  colnames(newfinal) = c(colnames(final), "ActivitiesScore", "MarketScore", "RegionScore", 
                         "BudgetScore", "LayoverScore", "CompositeScore")
  
  outp = newfinal[order(newfinal$CompositeScore, decreasing = TRUE),]
  #return(prettify(toJSON(outp)))
  return(outp)
}







