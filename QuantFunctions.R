
GetFXFromDB<-function(con)
{
  sql1<-paste("SELECT table_name FROM information_schema.tables where table_schema='databasefx';")
  rs <- dbSendQuery(con, sql1)
  res<-fetch(rs, n=-1)
  return(res[-which(res=="broker_quotes"),])
}

GetAssetFromDB<-function(con,pair)
{
  sql1<-paste("SELECT " ,"*", " FROM databasefx.",pair,";")
  rs <- dbSendQuery(con, sql1)
  return(fetch(rs, n=-1))
}

StandarizeRR<-function(txt,pair,res1,res)
{
  tenor <- substring(unlist(strsplit(txt," "))[1],10)
  ATM<-paste(toupper(pair),"V",tenor,sep="")
  Headers <- gsub("\\ Curncy PX_LAST","\\",colnames(res1))
  tenor
  pos<-which(Headers %in% ATM )
  if (pos!=0){
    res<-res/res1[,pos]
  }
}

roll_volatility<-function(x,varwindow)
{
  x<-diff(log(x))
  x<-na.omit(sqrt(runVar(x,n=varwindow))*sqrt(252)*100)
  colnames(x)<-"Realized Vol"
  return(x)
  
}

modifyinfo<-function(x,boolvar,booldiff,boollogdiff,lagg,varwindow){
  
  if(boolvar)
    x<-roll_volatility(x,varwindow)
    
  if(booldiff)
  {
    x<-diff(x)
    colnames(x)<-"Adjacent differences"
    
  }
  if(boollogdiff)
  {
    x<-diff(log(x))
    colnames(x)<-"Log Returns"
    
  }
  
  x<-lag.xts(x,k=lagg)
  
  return(x)
  
}


lm_eqn <- function(df){
  m <- lm(df[,2] ~ df[,1]);
  eq <- substitute(italic(y) == a + b * italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

data_lm <- function(df){
  m <- lm(df[,2] ~ df[,1])
  eq<-rep(0,4)
  eq[1]<-round(m$coefficients[1], digits = 3)
  eq[2]<-round(m$coefficients[2], digits = 3)
  eq[3]<-round(summary(m)$r.squared, digits = 3)
  eq[4]<-round(summary(m)$coef[2,4], digits = 3)
  return(eq)
}


nextBusinessDay<-function(InputDate)
{
  if(Weekday(InputDate) == 7)
  {
   #Input Date = Saturday
    return(InputDate + 2)
  }else if(Weekday(InputDate) == 6)
  {
    #Input Date = Friday
    return(InputDate + 3)
  }else{
    return(InputDate + 1)
  }
}

previousBusinessDay<-function(InputDate)
{
  if( Weekday(InputDate) == 1)
  {
  #Input Date = Sunday
    return( InputDate - 2)
  }else if( Weekday(InputDate) == 2)
  {
  #Input Date = Monday
    return(InputDate)
  }else{
    return(InputDate)
  }
}

businessDayIncrement<-function(InputDate, Increment)
{
  businessDayIncrement = InputDate
  for(Count in 1:Increment){
    businessDayIncrement = nextBusinessDay(businessDayIncrement)
  }
  return(businessDayIncrement)
}

businessDayDecrement<-function(InputDate,Decrement)
{
  businessDayDecrement = InputDate
  for(Count in 1:Decrement){
    businessDayDecrement = previousBusinessDay(businessDayDecrement)
  }
  return(businessDayDecrement)
}

getSpotDateFromHorizon<-function(InputDate)
{
  return(businessDayIncrement(InputDate, 2))
}

getHorizonFromSpotDate<-function(InputDate)
{
  return(businessDayDecrement(InputDate, 2))
}


getExpiryFromTenor<-function(Horizon, Tenor)
{
  Count <- substr(toupper(Tenor), 1,1)
  SpotDate <- getSpotDateFromHorizon(Horizon)
  
  if(toupper(Tenor) == "ON"){
    return(nextBusinessDay(Horizon))
  }else if(substr(toupper(Tenor), 2,2) == "W")
    {
    return(Horizon + Count * 7)
  }else if(substr(toupper(Tenor), 2,2) == "M")
    {
    DeliveryDate <- DateAdd("M", Count, SpotDate)
    return(getHorizonFromSpotDate(DeliveryDate))
  }else if(substr(toupper(Tenor), 2,2) == "Y")
    {
    DeliveryDate <- DateAdd("yyyy", Count, SpotDate)
    return(getHorizonFromSpotDate(DeliveryDate))
  }else
    {
      return(0)
    }
}

