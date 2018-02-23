# Define server logic required to draw a histogram
source("ui.R")
source("QuantFunctions.R")

server<-function(input, output) {
  
  drv <- dbDriver("MySQL")
  user_<-"Claudio"
  password_<-"12345678"
  dbname_<-"databasefx"
  #host_<-"localhost"
  host_<-"150.216.112.128"
  
  con<-dbConnect(drv, user=user_, password=password_, dbname=dbname_, host=host_,port=3307)
  
  output$summarytable<-renderTable({
    
                res1<-GetAssetFromDB(con,input$PAIR)
                
                res<-res1[,names(res1)==txt()]
                
                if(input$bool5)
                  res<-StandarizeRR(txt(),input$PAIR,res1,res)
                
                x <- xts(res,order.by=as.Date(res1[,1]))
                colnames(x)<-txt()
                
                subs<-na.omit(x[time(x)>=input$daterange[1] & time(x)<=input$daterange[2],])
            
                if(input$bool4)
                  subs<-roll_volatility(subs,input$varwindow)
            
                if(input$bool3)
                  subs<-diff(log(subs))
            
                datos<-na.omit(coredata(subs))
                mysummary2 <-sapply(strsplit(summary(datos),":"),"[[",2)
                data<-round(as.numeric(gsub("\\ ","\\",mysummary2)),3)
                nombres<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
                mysummary<-as.data.frame(t(cbind(nombres,data)))
                colnames(mysummary)<-nombres
                mysummary[2,]
    
  })
  
  output$bases <- renderUI({
    
    res<-GetFXFromDB(con)
    selectInput("PAIR", "FX Pair", res)
  })
  output$bases2 <- renderUI({
    
    res<-GetFXFromDB(con)
    selectInput("PAIR2", "FX Pair1 (X)", res)
  })
  output$bases3 <- renderUI({
    
    res<-GetFXFromDB(con)
    selectInput("PAIR3", "FX Pair2 (Y)", res)
  })
  output$bases4 <- renderUI({
    
    res<-GetFXFromDB(con)
    selectInput("PAIR4", "FX Pair3", res)
  })
  output$bases5 <- renderUI({
    
    res<-GetFXFromDB(con)
    selectInput("PAIR5", "FX Pair", res)
  })
  output$bases6 <- renderUI({

    res<-GetFXFromDB(con)
    selectInput("PAIR6", "FX Pair", res)
  })
  output$bases7 <- renderUI({
    
    sql1<-paste("SELECT * FROM databasefx.broker_quotes;", sep="")
    rs <- dbSendQuery(con, sql1)
    res<-fetch(rs, n=-1)
    res<-colnames(res)[-(1:2)]
    res<-res[-which(res=="Time")]
    selectInput("Datos", "Select Data", res)
  })
  output$instrumentos <- renderUI({
    
    res<-GetAssetFromDB(con,input$PAIR)
    selectInput("instruments", "Choose instrument", names(res[1,])[-1])
  })
  output$instrumentos2 <- renderUI({
    
    res<-GetAssetFromDB(con,input$PAIR2)
    selectInput("instruments2", "Choose instrument", names(res[1,])[-1])
  })
  output$instrumentos3 <- renderUI({
    
    res<-GetAssetFromDB(con,input$PAIR3)
    selectInput("instruments3", "Choose instrument", names(res[1,])[-1])
  })
  output$instrumentos_broker <- renderUI({
    
    sql1<-paste("SELECT FX FROM databasefx.broker_quotes;")
    rs <- dbSendQuery(con, sql1)
    res<-fetch(rs, n=-1)
    selectInput("instrumentos_broker", "Choose Pair", unique(res)[,])
  })
  output$tenors <- renderUI({
    
    sql1<-paste("SELECT Tenor FROM databasefx.broker_quotes;")
    rs <- dbSendQuery(con, sql1)
    res<-fetch(rs, n=-1)
    selectInput("tenors_broker", "Choose Tenor", unique(res)[,])
  })
  
  txt <- reactive({ input$instruments})
  txt2 <- reactive({ input$instruments2})
  txt3 <- reactive({ input$instruments3})
  
  output$QQPlot <- renderPlot({
    
    res1<-GetAssetFromDB(con,input$PAIR)
    res<-res1[,names(res1)==txt()]
    
    #Si se necesita estandarizar los risk reversals
    if(input$bool5){
      res<-StandarizeRR(txt(),input$PAIR,res1,res)
    }
    
    x <- xts(res,order.by=as.Date(res1[,1]))
    
    colnames(x)<-txt()
    
    subs<-na.omit(x[time(x)>=input$daterange[1] & time(x)<=input$daterange[2],])
    
    if(input$bool4)
    {
      subs<-diff(log(subs))
      subs<-sqrt(runVar(subs,n=input$varwindow))*sqrt(252)*100
    }
    if(input$bool3)
    {
      subs<-diff(log(subs))
    }
    
    
    y <- quantile(coredata(subs)[!is.na(coredata(subs))], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    
    p<-ggplot(subs,aes(sample=coredata(subs)))
    
    p<-p+ stat_qq()+theme_bw() + geom_abline(slope = slope, intercept = int)
    
    p+theme(legend.position =  "bottom",legend.title = element_blank())
    
    
  })
  
  output$HistPlot <- renderPlot({
    
    res1<-GetAssetFromDB(con,input$PAIR)
    res<-res1[,names(res1)==txt()]
    
    #Si se necesita estandarizar los risk reversals
    if(input$bool5){
      res<-StandarizeRR(txt(),input$PAIR,res1,res)
    }
    
    x <- xts(res,order.by=as.Date(res1[,1]))
    
    colnames(x)<-txt()
    
    subs<-na.omit(x[time(x)>=input$daterange[1] & time(x)<=input$daterange[2],])
    
    if(input$bool4)
    {
      subs<-diff(log(subs))
      subs<-sqrt(runVar(subs,n=input$varwindow))*sqrt(252)*100
    }
    if(input$bool3)
    {
      subs<-diff(log(subs))
    }
    
  
      p<-ggplot(subs,aes(coredata(subs),group=factor(year(time(subs))),fill=factor(year(time(subs)))))
      p<-p+geom_histogram()+theme_bw()
      
      p<-p+geom_vline(aes(xintercept=mean(subs)),
                      color="blue", linetype="dashed", size=1)
      p<-p+geom_vline(aes(xintercept=mean(subs)+sd(subs)),
                      color="red", linetype="dashed", size=1)
      p<-p+geom_vline(aes(xintercept=median(subs)),
                      color="green", linetype="dashed", size=1)
      p<-p+geom_vline(aes(xintercept=mean(subs)-sd(subs)),
                      color="red", linetype="dashed", size=1)
      
      p<-p+geom_text(aes(mean(subs),Inf,label = paste("mean= ",round(mean(subs),3)), vjust = 1.5,hjust=1.2),angle=90)
      p<-p+geom_text(aes(median(subs),Inf,label = paste("median= ",round(median(subs),3)), vjust = -1,hjust=1.2),angle=90)
      p<-p+geom_text(aes(mean(subs)-sd(subs),Inf,label = paste("mean-sd= ",round(mean(subs)-sd(subs),3)), vjust = -1,hjust=1.2),angle=90)
      p<-p+geom_text(aes(mean(subs)+sd(subs),Inf,label = paste("mean+sd= ",round(mean(subs)+sd(subs),3)), vjust = 1.5,hjust=1.2),angle=90)
      
    
    p+theme(legend.position =  "bottom",legend.title = element_blank())
    
    
  })
  
  output$distPlot <- renderPlotly({
    
    res1<-GetAssetFromDB(con,input$PAIR)
    res<-res1[,names(res1)==txt()]
    
    
    #Si necesita estandarizar los risk reversals
    if(input$bool5){
      res<-StandarizeRR(txt(),input$PAIR,res1,res)
    }
    
    x <- xts(res,order.by=as.Date(res1[,1]))
    colnames(x)<-txt()
    
    subs<-na.omit(x[time(x)>=input$daterange[1] & time(x)<=input$daterange[2],])
    
    if(input$bool4)
    {
      subs<-diff(log(subs))
      subs<-sqrt(runVar(subs,n=input$varwindow))*sqrt(252)*100
    }
    if(input$bool3)
    {
      subs<-diff(log(subs))
    }

      p<-ggplot(subs,aes(as.Date(time(subs)),coredata(subs),
                         group=1,
                         text = paste('Price:', coredata(subs),
                                      '<br>Date: ', as.Date(time(subs)))))
      p<-p+geom_line()
      
      if(input$bool)
      {
        p<-p+geom_smooth(method='lm')
      }
      if(input$bool2)
      {
        p<-p+geom_smooth(color='red')
      }
      
      p<-p+labs(x = "",size=20)
      p<-p+labs(y = "",size=10)
      p<-p+ggtitle(colnames(x))
      p<-p+theme(axis.text=element_text(size=8),
                 axis.title=element_text(size=10,face="bold"))
      
    p<-p+theme(legend.position =  "bottom",legend.title = element_blank())
    
    ggplotly(p,tooltip = c("text")) %>%
      config(displayModeBar = F) 
    
  })
  
  output$Broker_data_plot<-renderPlotly({
    
    sql1<-paste("Select * from databasefx.broker_quotes WHERE FX='",
                input$instrumentos_broker,"' AND Tenor='",input$tenors_broker,"'",sep="")
    
    rs <- dbSendQuery(con, sql1)
    res<-fetch(rs, n=-1)
    res<-res[,c(dim(res)[2],which(colnames(res)==input$Datos))]
    
    x <- xts(as.double(res[,2]),order.by=strptime(res[,1], "%Y-%m-%d %H:%M:%S"))
    
    

    
    p<-ggplot(x,aes(strptime(time(x), "%Y-%m-%d %H:%M:%S"),coredata(x),
                       group=1,
                       text = paste('Price:', coredata(x),
                                   '<br>Date: ', strptime(time(x), "%Y-%m-%d %H:%M:%S")))
    )
    
    plot(as.double(coredata(x)))
    
    p<-p+geom_line()
    
    p<-p+theme(legend.position =  "bottom",legend.title = element_blank())
    p<-p+labs(x = "",size=20)
    p<-p+labs(y = "",size=10)
    p<-p+ggtitle(colnames(x))
    p<-p+theme(axis.text=element_text(size=8),
               axis.title=element_text(size=10,face="bold"))
   #p
    
    ggplotly(p
            ,tooltip = c("text")
    ) %>% config(displayModeBar = F) 
    
  })
  
  output$distPlotrealvol<-renderDygraph({
    
    vol_type <- switch(input$voltype,
                       c2c = "close",
                       gk = "garman.klass",
                       park="parkinson",
                       nomames="gk.yz")

    res12<-GetAssetFromDB(con,input$PAIR5)
    res2<-res12[,2:5]
    x2 <- xts(res2,order.by=as.Date(res12[,1]))
    x<-xts(rep(0,length(res12[,1])),order.by=as.Date(res12[,1]))
    colnames(x)<-0
    
      if(input$bool1w)
      {
        m<-na.omit(volatility(x2, n = 7, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"1w"
        x<-merge(x,m)
      }
      if(input$bool2w)
      {
        m<-na.omit(volatility(x2, n = 14, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"2w"
        x<-merge(x,m)
        
      }
      if(input$bool1m)
      {
        m<-na.omit(volatility(x2, n = 30, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"1m"
        x<-merge(x,m)
        
      }
      if(input$bool2m)
      {
        m<-na.omit(volatility(x2, n = 60, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"2m"
        x<-merge(x,m)
        
      }
      if(input$bool3m)
      {
        m<-na.omit(volatility(x2, n = 90, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"3m"
        x<-merge(x,m)
        
      }
      if(input$bool6m)
      {
        m<-na.omit(volatility(x2, n = 180, calc = vol_type, N = 252, mean0 = FALSE))
        colnames(m)<-"6m"
        x<-merge(x,m)
      }
      x<-x[,-1]
      x<-na.omit(x)

    
    x<-x[time(x)>=input$daterange5[1] & time(x)<=input$daterange5[2],]
    
    dygraph(x)%>%
      dyRangeSelector(height = 20)
  })
  
  output$distPlotrealvol2<-renderDygraph({
    
    tenor_type <- switch(input$tenortype,
                         w1 = 7,
                       w2 = 14,
                       m1 = 30,
                       m2 = 60,
                       m3 = 90,
                       m6 = 180)
    
    res12<-GetAssetFromDB(con,input$PAIR5)
    res2<-res12[,2:5]
    x2 <- xts(res2,order.by=as.Date(res12[,1]))
    x<-xts(rep(0,length(res12[,1])),order.by=as.Date(res12[,1]))
    colnames(x)<-0
    
    if(input$boolc2c)
    {
      m<-na.omit(volatility(x2, n = tenor_type, calc = "close", N = 252, mean0 = FALSE))
      colnames(m)<-"Close 2 Close"
      x<-merge(x,m)
    }
    if(input$boolgk)
    {
      m<-na.omit(volatility(x2, n = tenor_type, calc = "garman.klass", N = 252, mean0 = FALSE))
      colnames(m)<-"G K"
      x<-merge(x,m)
      
    }
    if(input$boolpark)
    {
      m<-na.omit(volatility(x2, n = tenor_type, calc = "parkinson", N = 252, mean0 = FALSE))
      colnames(m)<-"Parkinson"
      x<-merge(x,m)
      
    }
    if(input$boolnomames)
    {
      m<-na.omit(volatility(x2, n = tenor_type, calc = "gk.yz", N = 252, mean0 = FALSE))
      colnames(m)<-"G-K Y-Z"
      x<-merge(x,m)
      
      
    }
    
    x<-x[,-1]
    x<-na.omit(x)
    
    
    x<-x[time(x)>=input$daterange5[1] & time(x)<=input$daterange5[2],]
    
    dygraph(x)%>%
      dyRangeSelector(height = 20)
  })
  
  output$Multitimeseries<-renderDygraph({
    
    res12<-GetAssetFromDB(con,input$PAIR2)
    res2<-res12[,names(res12)==txt2()]
    x2 <- xts(res2,order.by=as.Date(res12[,1]))
    colnames(x2)<-txt2()
    
    x2<-modifyinfo(x2,input$boolvar1,input$booldiff1,
                   input$boollogdiff1,input$lagg,input$varwindow1)
    
    res13<-GetAssetFromDB(con,input$PAIR3)
    res3<-res13[,names(res13)==txt3()]
    x3 <- xts(res3,order.by=as.Date(res13[,1]))
    colnames(x3)<-txt3()
    
    x3<-modifyinfo(x3,input$boolvar2,input$booldiff2,
                   input$boollogdiff2,input$lagg2,input$varwindow2)
    
    xx<-na.omit(merge(x2,x3))
    
    subs<-xx[time(xx)>=input$daterange2[1] & time(xx)<=input$daterange2[2],]
    
    dygraph(subs,main="Time Series Plot")
    
  })
  
  output$distPlot2 <- renderPlotly({
    
    res12<-GetAssetFromDB(con,input$PAIR2)
    res2<-res12[,names(res12)==txt2()]
    x2 <- xts(res2,order.by=as.Date(res12[,1]))
    colnames(x2)<-txt2()
    
    x2<-modifyinfo(x2,input$boolvar1,input$booldiff1,
                         input$boollogdiff1,input$lagg,input$varwindow1)
    
    res13<-GetAssetFromDB(con,input$PAIR3)
    res3<-res13[,names(res13)==txt3()]
    x3 <- xts(res3,order.by=as.Date(res13[,1]))
    colnames(x3)<-txt3()
    
    x3<-modifyinfo(x3,input$boolvar2,input$booldiff2,
                   input$boollogdiff2,input$lagg2,input$varwindow2)
    
    xx<-na.omit(merge(x2,x3))
    
    subs<-xx[time(xx)>=input$daterange2[1] & time(xx)<=input$daterange2[2],]
  
    p<-ggplot(subs,aes(subs[,1],subs[,2],
              group=1,
              text = paste('X:', subs[,1],
                           '<br>Y: ', subs[,2])))+geom_point()
    
    p<-p+labs(x = colnames(xx)[1],size=30)+labs(y = colnames(xx)[2],size=30)
    p<-p+theme(axis.text=element_text(size=12),
               axis.title=element_text(size=14,face="bold"))
        

      if(input$boolbis)
      {
        df<-data.frame(v1=coredata(subs[,1]),v2=coredata(subs[,2]))
        datos<-data_lm(df)
        p<-p+geom_smooth(method='lm')+
          labs(title = paste("R2 = ",datos[3],
                             "a =",datos[1],
                             " slope =",datos[2],
                             " Pval =",datos[4]))
      }
      if(input$bool2bis)
      {
        p<-p+geom_smooth(color='red')
      }
      
      p<-p+theme_bw()+theme(legend.position =  "bottom",legend.title = element_blank())
      
      ggplotly(p,tooltip = c("text")) %>%
        config(displayModeBar = F) 
      
  })
  
  observeEvent(input$button_update, {
    blpConnect()
    drv <- dbDriver("MySQL")
    user_<-"Claudio"
    password_<-"12345678"
    dbname_<-"databasefx"
    host_<-"150.216.112.128"
    
    res1<-as.vector(GetAssetFromDB(con,input$PAIR4))
    res<-res1[,1]
    requested<-bdh(input$string_ticker, 
                   "PX_LAST", 
                   start.date=as.Date(res[1]),
                   include.non.trading.days = TRUE)
    
    if(dim(requested)[1]!=0)
    {
      requested<-requested[requested[,1] %in% as.Date(res),]
      sql<-paste("ALTER TABLE `databasefx`.`",input$PAIR4,"` ADD COLUMN `",input$string_ticker," PX_LAST`",
                 " DOUBLE NULL DEFAULT NULL AFTER `EURBRL12M Curncy PX_LAST`;",sep="")
      rs <- dbSendQuery(con, sql)
      
      for(i in 1:length(res))
      {
        if(is.na(requested[i]))
        {
          sql<-paste("insert into databasefx.",input$PAIR4," (",
                     input$string_ticker,"PX_LAST) Values (",
                     "NULL",")",sep="")
        }else{
          
          
          sql<-paste("insert into databasefx.",input$PAIR4," (",
                     input$string_ticker,"PX_LAST) Values (",
                     requested[i,2],")",sep="")
        }
        rs <- dbSendQuery(con, sql)
      }
      
      showNotification("The database has been updated",type="message")
    }else{
      showNotification("Name not found",type="error")
    }
    
  })
  
  observeEvent(input$button, {
    blpConnect()
    drv <- dbDriver("MySQL")
    user_<-"Claudio"
    password_<-"12345678"
    dbname_<-"databasefx"
    host_<-"150.216.112.128"
    
    con<-dbConnect(drv, user=user_, password=password_, dbname=dbname_, host=host_)
    
    sql<-paste("SELECT table_name FROM information_schema.tables where table_schema='databasefx';")
    rs <- dbSendQuery(con, sql)
    res<-fetch(rs, n=-1)
    res<-res[-which(res=="broker_quotes"),]
    withProgress(message = 'Download Progress', value = 0, {
      for(i in res)
      {
        incProgress(1/length(res), detail = i)
        sql<-paste("SELECT " ,"*", " FROM databasefx.",i,";",sep="")
        rs <- dbSendQuery(con, sql)
        res<-fetch(rs, n=-1)
        fields_raw<-colnames(res)[2:(length(colnames(res)))]
        last_date<-as.Date(last(res[,1]))
        if(Sys.Date()-1>last_date)
        {
          resultado<-0
          withProgress(message = 'Download Progress', value = 0, {
            for(j in fields_raw)
            {
              incProgress(1/length(fields_raw), detail = j)
              request<-unlist(strsplit(j," "))
              ticker<-paste(request[1]," ",request[2],sep="")
              requested<-bdh(ticker, 
                             request[3], 
                             start.date=last_date+1,
                             end.date = today()-1,
                             include.non.trading.days = TRUE)
              resultado<-cbind(resultado,requested[,2])
            }
          })
          resultado<-resultado[,-1]

          final<-data.frame(requested[,1],resultado)
          final<-final[!is.na(final[,2]),]
          colnames(final)<-c("Fecha",fields_raw)

          if(dim(final)[1]>0){
            for(k in 1:length(final[,1]))
            {
              
              sql1<-paste("insert into databasefx.",i," (",colnames(final)[1],sep="")
              sql2<-paste("VALUES ('",final[k,1],"'",sep="")
              for(l in 2:length(colnames(final)) )
              {
                
                sql1<-paste(sql1,", `",colnames(final)[l],"`",sep="")
                if(!is.na(final[k,l])){
                  sql2<-paste(sql2,",",final[k,l],sep="")
                }else{
                  sql2<-paste(sql2,",NULL",sep="")
                }
              }
              sql1<-paste(sql1,")",sep="")
              sql2<-paste(sql2,");",sep="")
              sql<-paste(sql1,sql2)
              dbSendQuery(con, sql)
            } } } }
    })
    showNotification("The database has been updated",type="message")
  })
  
  
}
