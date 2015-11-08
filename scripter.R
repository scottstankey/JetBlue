
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
                actscore / sum(activities)  + marketscore / sum(markets) + layoverscore +  budgetscore / {max(budgetscore) - min(budgetscore)} + 
                  if(sum(region) > 0){regionscore / sum(region)}else{0})

colnames(newfinal) = c(colnames(final), "ActivitiesScore", "MarketScore", "RegionScore", "BudgetScore", "LayoverScore", "CompositeScore")

outp = newfinal[order(newfinal$CompositeScore, decreasing = TRUE),]
#return(prettify(toJSON(outp)))
return(outp)
}





