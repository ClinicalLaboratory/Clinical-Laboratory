cat("\014")# Clear Console
#dev.off()# Clear All Graphs in the plot area
rm(list=ls(all=TRUE)) 
closeAllConnections() # close any file connections if any
#require (shiny)
#require (forecast)
#require (tseries)
library (shiny)
library (tseries)
library (forecast)
library(png) 
library (DT)
library(zoo)
library(shinyapps)
library(date)
MyDataTemp <-c(0)
rm(list=ls(all=TRUE)) 
shinyServer(function(input, output) { 
        
        #         output$dateText  <- renderText({   
        #                 recordsDate<- as.character(input$dataset)
        #                 recordsDate<-as.POSIXlt(recordsDate)
        #                 recordsDate<-unclass (recordsDate)
        #                 paste("input$date is", length(RecordedTimeSeries[patternFreq:length(RecordedTimeSeries)]))
        #                                                         if(recordsDate$year>=100){
        #                                                                 year<<-recordsDate$year-100+2000
        #                                                         }else {year<<-recordsDate$year+1900}
        #                 paste("input$date is",inFile$datapath)
        #         })
        
        #######################
        dataInputHWMUL <- reactive({
                
                inFile <<- input$file1
                if (is.null(inFile))
                        return(NULL)
                if (input$ftype=="csv"){
                        MyData <- read.csv(inFile$datapath, sep=',' )
                        x<-MyData[,1]
                        MyDate<-MyData[,2]
                        y<<-MyData[,3]
                }else {y<<-read.table(inFile$datapath)}
                
                ###################
                #                 d = read.table("foobar.txt", 
                #                                sep="\t", 
                #                                col.names=c("id", "name"), 
                #                                fill=FALSE, 
                #                                strip.white=TRUE)
                ##################
                
                #                 output$dateText  <- renderText({
                #                         paste("input$date is",inFile[1])
                #                 })
                
                y<<-y[!is.na(y)]   
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                
                if (input$dataFreq=="Minute")   {SeriesFreq<-60}
                if (input$dataFreq=="Hour")     {SeriesFreq<-24}
                if (input$dataFreq=="Day of the week")      {SeriesFreq<-7}
                if (input$dataFreq=="Week")     {SeriesFreq<-52}
                if (input$dataFreq=="Two-Weeks"){SeriesFreq<-26}
                if (input$dataFreq=="Month of the year")    {SeriesFreq<-12}
                if (input$dataFreq=="Year")     {SeriesFreq<-1}
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                recordsDate<<- as.character(input$date)
                recordsDate<<-as.POSIXlt(recordsDate)
                recordsDate<<-unclass (recordsDate)
                
                if(recordsDate$year>=100){
                        year<<-recordsDate$year-100+2000
                        
                }else {year<<-recordsDate$year+1900}
                mon<<-recordsDate$mon
                
                
                RecordedTravelTime<<-y# scan("C:/Users/Emad/Dropbox/CLL_BloodImages/TravelTimeEstimation/R Code/TraveltimeData/700.dat")
                
                # adf.test(RecordedTravelTime, alternative="stationary", k=0)
                TSeries<<-ts(RecordedTravelTime,  frequency= SeriesFreq,start=c(year,mon+1))#start=c(2011,4), delta=1
                #                                 l    = mean(RecordedTravelTime[1:patternFreq])
                #                                 b    = (RecordedTravelTime[patternFreq]-RecordedTravelTime[1])/(patternFreq-1)
                #                                 sMul = RecordedTravelTime[1:patternFreq]/mean(RecordedTravelTime[1:patternFreq]) 
                #                  sAdd = RecordedTravelTime[1:patternFreq]-mean(RecordedTravelTime[1:patternFreq]) 
                #########################HWMultModel#################################################
                TforecastsHWMul <<- HoltWinters(TSeries, seasonal="mult",start.periods = patternFreq,
                                                optim.start = c(alpha = 0.3, beta = 0.1,gamma = 0.1)) # l.start=l,b.start=b,s.start=sMul,
                #####################################################################################
                #####################################################################################
                xhatHWMul<-TforecastsHWMul$fitted[,1]
                ResidualErrorsHWMul<-residuals(TforecastsHWMul)
                Tmean<-mean(RecordedTravelTime)
                
                ResidualErrorsHWMul<-residuals(TforecastsHWMul)
                
                SumSEHWMul<<-sum((xhatHWMul-Tmean)^2)
                SSRHWMul<-sum ((RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)]-Tmean)^2)
                RsequaredHWMul<<-SumSEHWMul/SSRHWMul
                
                HWM<-ets(TSeries,"MMM")
                HWMAIC<<-HWM$aic
                HWMBIC<<-HWM$bic
                
                dummy <- vector(mode="numeric", length=patternFreq)
                nxhatHWMul<- c(dummy,xhatHWMul)
                nresidulasHWMul<-c(dummy,ResidualErrorsHWMul)
                mydata1<-list(TSeries,nxhatHWMul,nresidulasHWMul)
                pMul <<- predict(TforecastsHWMul, Number_PointsToEstimate, prediction.interval = TRUE, level = 0.95)
                Estimated_ValueHWMul <<- pMul[,1]
                UpperLimitHWMul      <<- pMul[,2]
                LowerLimitHWMul      <<- pMul[,3]
                PrecisionHWMul       <- (UpperLimitHWMul-LowerLimitHWMul)/2
                
                
                if (input$dataFreq=="Month of the year")            {tick<-31}
                if (input$dataFreq=="Day of the week"  )            {tick<-1}
                
                tindex <-(c(1:Number_PointsToEstimate)+ length(RecordedTravelTime))*tick ## to to adjust the tick jump Done!
                TimeStampInFuture <<-as.Date(tindex,input$date)
                
                if (input$dataFreq=="Month of the year")            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %Y")}
                if (input$dataFreq=="Day of the week"  )            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %d %Y")}
                
                
                
                
                MyEstimatedDataHWMul <<- list(TimeStampInFuture,Estimated_ValueHWMul,UpperLimitHWMul,LowerLimitHWMul)
                
                
                
                SSEHWMUL<<-(sum((xhatHWMul-RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)])^2))/ length (RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)])
                
                
                tfHWMul <<- tempfile()
                write.table(MyEstimatedDataHWMul, tfHWMul, 
                            sep=',',col.names = c("Time_Stamp","Estimated_ValueHWMul","UpperLimitHWMul","LowerLimitHWMul"),row.names = FALSE)
        })
        
        output$HWMplot <- renderPlot ({
                
                inFile <<- input$file1
                if (is.null(inFile))
                        return(NULL)
                
                dataInputHWMUL()
                minyscale<-min(0,min(TSeries,pMul))
                
                
                xlabin<-as.character(input$xaxisHWM)
                ylabin<-as.character(input$yaxisHWM)
                mainin<-as.character(input$titleHWM)
                
                
                plot(
                        TforecastsHWMul, pMul,main = mainin,type="l",ylim=c(minyscale,max(TSeries,pMul)+mean(TSeries)/6),
                        xlab=xlabin,ylab=ylabin#,xtat="n"
                )
                if (input$showGridHWM){grid(lwd = 1, col = "blue")}
                legend(
                        "topleft",c("Original Data","Fitted/Estimated Data", "95% Confidence Interval"),
                        pch = 15,col = c("black","red","blue")
                )
                #######################################################  
                #DT::renderDataTable
                output$HWMtable <- DT::renderDataTable({
                        options(digits=3)
                        inFile <- input$file1
                        if (is.null(inFile))
                                return(NULL)              
                        MyDataHWMul<-read.csv(tfHWMul , header = TRUE, sep = ",", quote = "\"",
                                              dec = ".", fill = TRUE)
                        
                        
                        
                        
                        DateIndex <- MyDataHWMul[,1]
                        MyDataHWMul<-format(round(MyDataHWMul[,2:4], 2), nsmall = 2) # fix to only adjust the number not the first text column
                        MyDataHWMul<<-cbind(DateIndex, MyDataHWMul)   
                        
                        
                        #DT::datatable
                        #MyDataHWMul<-format(round(MyDataHWMul, 2), nsmall = 2)
                },options = list(pageLength = 15,searching = FALSE),rownames= FALSE
                )#DT::datatable(MyDataHWMul, options = list(pageLength = 15))
                #########################################################            
        })    
        ########################################HWA##########################################################                     
        dataInputHWADD <- reactive({
                inFile <<- input$file1
                if (is.null(inFile))
                        return(NULL)
                if (input$ftype=="csv"){
                        MyData <- read.csv(inFile$datapath, sep=',' )
                        x<-MyData[,1]
                        MyDate<-MyData[,2]
                        y<<-MyData[,3]
                }else {y<<-read.table(inFile$datapath)}
                y<<-y[!is.na(y)] 
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                #######################################################
                RecordedTravelTime<-y# scan("C:/Users/Emad/Dropbox/CLL_BloodImages/TravelTimeEstimation/R Code/TraveltimeData/700.dat")
                # Dickey-Fuller test for variable  y_{t}=(\rho-1)y_{t-1}+u_{t}=\delta y_{t-1}+u_{t}\, 
                #adf.test(RecordedTravelTime, alternative="stationary", k=0)
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                recordsDate<<- as.character(input$date)
                recordsDate<<-as.POSIXlt(recordsDate)
                recordsDate<<-unclass (recordsDate)
                paste("input$date is", recordsDate$year)
                if(recordsDate$year>=100){
                        year<<-recordsDate$year-100+2000
                }else {year<<-recordsDate$year+1900}
                
                if (input$dataFreq=="Minute")   {SeriesFreq<-60}
                if (input$dataFreq=="Hour")     {SeriesFreq<-24}
                if (input$dataFreq=="Day of the week")      {SeriesFreq<-7}
                if (input$dataFreq=="Week")     {SeriesFreq<-52}
                if (input$dataFreq=="Two-Weeks"){SeriesFreq<-26}
                if (input$dataFreq=="Month of the year")    {SeriesFreq<-12}
                if (input$dataFreq=="Year")     {SeriesFreq<-1}
                TSeries<<-ts(RecordedTravelTime,  frequency=SeriesFreq,start=c(year,mon+1))#start=c(2011,4), delta=1
                #                                l    = mean(RecordedTravelTime[1:patternFreq])
                #                                b    = (RecordedTravelTime[patternFreq]-RecordedTravelTime[1])/(patternFreq-1)
                #                                sMul = RecordedTravelTime[1:patternFreq]/mean(RecordedTravelTime[1:patternFreq]) 
                #                  sAdd = RecordedTravelTime[1:patternFreq]-mean(RecordedTravelTime[1:patternFreq]) 
                #########################HWMultModel#################################################
                TforecastsHWAdd <<- HoltWinters(TSeries, seasonal="additive",start.periods = patternFreq,
                                                optim.start = c(alpha = 0.3, beta = 0.1,gamma = 0.1)) # l.start=l,b.start=b,s.start=sAdd,
                #####################################################################################
                xhatHWAdd<-TforecastsHWAdd$fitted[,1]
                ResidualErrorsHWAdd<-residuals(TforecastsHWAdd)
                Tmean<-mean(RecordedTravelTime)
                
                ResidualErrorsHWAdd<-residuals(TforecastsHWAdd)
                SumSEHWAdd<<-sum((xhatHWAdd-Tmean)^2)
                SSRHWAdd<-sum ((RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)]-Tmean)^2) 
                RsequaredHWAdd<<-SumSEHWAdd/SSRHWAdd
                
                dummy <- vector(mode="numeric", length=patternFreq)
                nxhatHWAdd<- c(dummy,xhatHWAdd)
                nresidulasHWAdd<-c(dummy,ResidualErrorsHWAdd)
                pAdd <<- predict(TforecastsHWAdd, Number_PointsToEstimate, prediction.interval = TRUE, level = 0.95)
                Estimated_ValueHWAdd <<- pAdd[,1]
                UpperLimitHWAdd      <<- pAdd[,2]
                LowerLimitHWAdd      <<- pAdd[,3]
                PrecisionHWAdd       <<- (UpperLimitHWAdd-LowerLimitHWAdd)/2
                
                
                if (input$dataFreq=="Month of the year")            {tick<-31}
                if (input$dataFreq=="Day of the week"  )            {tick<-1}
                
                tindex <-(c(1:Number_PointsToEstimate)+ length(RecordedTravelTime))*tick ## to to adjust the tick jump Done!
                TimeStampInFuture <<-as.Date(tindex,input$date)
                
                if (input$dataFreq=="Month of the year")            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %Y")}
                if (input$dataFreq=="Day of the week"  )            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %d %Y")}
                
                
                MyEstimatedDataHWAdd <<- list(TimeStampInFuture,Estimated_ValueHWAdd,UpperLimitHWAdd,LowerLimitHWAdd)
                
                SSEHWADD<<-(sum((xhatHWAdd-RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)])^2))/ length (RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)])
                
                HWAdd_alpha<-TforecastsHWAdd$alpha
                HWAdd_beta<-TforecastsHWAdd$beta
                HWAdd_gamma<-TforecastsHWAdd$gamma
                HWA<-ets(TSeries,"AAA")
                HWAAIC<<-HWA$aic
                HWABIC<<-HWA$bic
                tfHWAdd <<- tempfile()
                write.table(MyEstimatedDataHWAdd, tfHWAdd, 
                            sep=',',col.names = c("Time_Stamp","Estimated_ValueHWAdd","UpperLimitHWAdd","LowerLimitHWAdd"),row.names = FALSE)
        })        
        
        
        output$HWAplot <- renderPlot({
                inFile <<- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                dataInputHWADD()
                minyscale<-min(0,min(TSeries,pAdd))
                xlabin<-as.character(input$xaxisHWA)
                ylabin<-as.character(input$yaxisHWA)
                mainin<-as.character(input$titleHWA)
                plot(
                        TforecastsHWAdd, pAdd,main = mainin,type="l",ylim=c(minyscale,max(TSeries,pAdd)+mean(TSeries)/6),
                        xlab=xlabin,ylab=ylabin#,xtat="n"
                )
                if (input$showGridHWA){grid(lwd = 1, col = "blue")}
                legend(
                        "topleft",c("Original Data","Fitted/Estimated Data", "95% Confidence Interval"),
                        pch = 15,col = c("black","red","blue")
                )
                #######################################################    
                output$HWAtable <- DT::renderDataTable({
                        options(digits=3)
                        inFile <- input$file1
                        if (is.null(inFile))
                                return(NULL)              
                        MyDataHWAdd<-read.csv(tfHWAdd, header = TRUE, sep = ",", quote = "\"",
                                              dec = ".", fill = TRUE)
                        
                        DateIndex <- MyDataHWAdd[,1]
                        MyDataHWAdd<-format(round(MyDataHWAdd[,2:4], 2), nsmall = 2) # fix to only adjust the number not the first text column
                        MyDataHWAdd<-cbind(DateIndex, MyDataHWAdd)                
                        
                       # MyDataHWAdd<-format(round(MyDataHWAdd, 2), nsmall = 2)
                }, options = list(pageLength = 15,searching = FALSE),rownames= FALSE#options=list(iDisplayLength=10,bFilter = FALSE)
                )
        })
        #########################################################   
        
        dataInputRM <- reactive({
                
                inFile <<- input$file1
                if (is.null(inFile))
                        return(NULL)
                
                if (input$ftype=="csv"){
                        MyData <- read.csv(inFile$datapath, sep=',' )
                        x<-MyData[,1]
                        MyDate<-MyData[,2]
                        y<<-MyData[,3]
                }else {y<<-read.table(inFile$datapath)}
                y<<-y[!is.na(y)] 
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                RecordedTimeSeries<<-y
                TimeStamp<<-1:length(RecordedTravelTime)
                patternFreq<<- input$Points_In_Pattern
                
                recordsDate<<- as.character(input$date)
                recordsDate<<-as.POSIXlt(recordsDate)
                recordsDate<<-unclass (recordsDate)
                paste("input$date is", recordsDate$year)
                if(recordsDate$year>=100){
                        year<<-recordsDate$year-100+2000
                }else {year<<-recordsDate$year+1900}
                
                if (input$dataFreq=="Minute")           {SeriesFreq<-60}
                if (input$dataFreq=="Hour")             {SeriesFreq<-24}
                if (input$dataFreq=="Day of the week")              {SeriesFreq<-7}
                if (input$dataFreq=="Week")             {SeriesFreq<-52}
                if (input$dataFreq=="Two-Weeks")        {SeriesFreq<-26}
                if (input$dataFreq=="Month of the year")            {SeriesFreq<-12}
                if (input$dataFreq=="Year")             {SeriesFreq<-1}
                
                patternFreq<<- input$Points_In_Pattern
                TimeStamp<-1:length(RecordedTimeSeries)
                Number_PointsToEstimate <<- input$EstimatedPoints
                RecordedTimeSeries<-ts(RecordedTimeSeries,  frequency=patternFreq,start=c(year-1,mon+1))
                RegModel <-tslm(RecordedTimeSeries~TimeStamp)
                NTimeStamp<-seq(1,Number_PointsToEstimate,1)+length(RecordedTimeSeries)+1
                LmPredict <- forecast(RegModel, h=5,level=95,newdata=data.frame(TimeStamp=NTimeStamp))
                Tmean<-mean(RecordedTravelTime)
                SumSELM<<-sum((RegModel$fitted.values-Tmean)^2)
                SSRLM<-sum ((RecordedTravelTime[(patternFreq+1):length(RecordedTravelTime)]-Tmean)^2) 
                RsequaredLM<<-SumSELM/SSRLM
                
                fitData <<- fitted(RegModel)
                Estimated_ValueLM <<- LmPredict$mean
                UpperLimitLM <<- LmPredict$upper
                LowerLimitLM <<- LmPredict$lower
                PrecisionLM  <<- (UpperLimitLM-LowerLimitLM)/2
                
                if (input$dataFreq=="Month of the year")            {tick<-31}
                if (input$dataFreq=="Day of the week"  )            {tick<-1}
                
                tindex <-(c(1:Number_PointsToEstimate)+ length(RecordedTravelTime))*tick ## to to adjust the tick jump Done!
                TimeStampInFuture <<-as.Date(tindex,input$date)
                
                if (input$dataFreq=="Month of the year")            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %Y")}
                if (input$dataFreq=="Day of the week"  )            {TimeStampInFuture<-format((TimeStampInFuture-tick), format="%B %d %Y")}
                
                MyEstimatedDataLM <<- list(TimeStampInFuture,Estimated_ValueLM,UpperLimitLM,LowerLimitLM)
                
                tflm <<- tempfile()
                write.table(MyEstimatedDataLM, tflm, 
                            sep=',',col.names = c("Time_Stamp","Estimated_ValueLinearModel","UpperLimitLM","LowerLimitLM"),row.names = FALSE)
        })
        
        output$RMplot <- renderPlot({
                
                inFile <<- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                MyData <- read.csv(inFile$datapath, sep=',' )
                
                Number_PointsToEstimate <<- input$EstimatedPoints
                patternFreq<<- input$Points_In_Pattern
                #                 x<-MyData[,1]
                #                 MyDate<-MyData[,2]
                #                 RecordedTimeSeries<<-MyData[,3]
                #                 
                dataInputRM()
                
                patternFreq<<- input$Points_In_Pattern
                TimeStamp<-1:length(RecordedTimeSeries)
                Number_PointsToEstimate <<- input$EstimatedPoints
                RecordedTimeSeries<-ts(RecordedTimeSeries,  frequency=patternFreq,start=c(year,mon+1))
                RegModel <-tslm(RecordedTimeSeries~TimeStamp)
                NTimeStamp<-seq(1,Number_PointsToEstimate,1)+length(RecordedTimeSeries)+1
                LmPredict <- forecast(RegModel, h=5,level=95,newdata=data.frame(TimeStamp=NTimeStamp))
                
                minyscale<-min(0,min(RecordedTimeSeries,LmPredict$lower))
                
                xlabin<-as.character(input$xaxisRM)
                ylabin<-as.character(input$yaxisRM)
                mainin<-as.character(input$titleRM)
                
                plot(RecordedTimeSeries,  ylim=c(0,max(LmPredict$upper,RecordedTimeSeries)),ylab=ylabin, xlab=xlabin, main=mainin,
                     xlim=c(min(time(RecordedTimeSeries)),max(time(LmPredict$mean))))
                if (input$showGridRM){grid(lwd = 1, col = "blue")}
                legend(
                        "bottomleft",c("Original Data","Fitted/Estimated Data", "95% Confidence Interval"),
                        pch = 15,col = c("black","red","blue")
                )
                abline(v=max(time(RecordedTimeSeries)), lty=2)
                lines(fitted(RegModel),col="red")
                lines (LmPredict$mean,col="red")
                lines (LmPredict$upper,col="blue")
                lines (LmPredict$lower,col="blue")
                
                ###################################################################################################################
                output$Regressiontable <- DT::renderDataTable({
                        options(digits=3)
                        inFile <- input$file1
                        if (is.null(inFile))
                                return(NULL)              
                        MyDatalm<-read.csv(tflm, header = TRUE, sep = ",", quote = "\"",
                                           dec = ".", fill = TRUE)
                        DateIndex <- MyDatalm[,1]
                        MyDatalm<-format(round(MyDatalm[,2:4], 2), nsmall = 2) # fix to only adjust the number not the first text column
                        MyDatalm<-cbind(DateIndex, MyDatalm)
                }, options = list(pageLength = 15,searching = FALSE),rownames= FALSE#options=list(iDisplayLength=10,bFilter = FALSE)
                )
        })
        
        output$downloadData <- downloadHandler(
                filename = function() { 
                        'EstimatedData.csv'#paste(input$dataset, 'EstimatedData.csv', sep='') #
                },
                
                content = function(file) {
                        
                        dataInputRM()
                        dataInputHWADD()
                        dataInputHWMUL()
                        MyList<-list(MyEstimatedDataHWMul,MyEstimatedDataHWAdd,MyEstimatedDataLM)
                        write.table(MyList, file,
                                    sep=',',col.names = c("Date_Index","Estimated_Value_HWMul","UpperLimit_HWMul","LowerLimit_HWMul",
                                                          "Date_Index","Estimated_Value_HWAdd","UpperLimit_HWAdd","LowerLimit_HWAdd",
                                                          "Date_Index","Estimated_Value_LM","UpperLimit_LM","LowerLimit_LM"),row.names = FALSE)
                        
                }
        )
        
        output$CompPlot <- renderPlot({
                inFile <<- input$file1
                if (is.null(inFile))
                        return(NULL)
                
                dataInputHWMUL()
                dataInputHWADD()
                dataInputRM()
                
                par(mfrow=c(2,2))
                acf(RecordedTravelTime, main="Stationary nature  test of the input time-series")
                
                plotForecastErrors(residuals(TforecastsHWMul),"the Multiplicative Holt-Winters Model residual error")
                
                legend("topleft",'groups',c("Histogram","Normal Distribution"), 
                       lty = c(1,1),col = c("red","blue"))
                
                plotForecastErrors(residuals(TforecastsHWAdd),"the Additive Holt-Winters Model residual error")
                legend("topleft",'groups',c("Histogram","Normal Distribution"), 
                       lty = c(1,1),col = c("red","blue"))
                
                patternFreq<<- input$Points_In_Pattern
                TimeStamp<-1:length(RecordedTimeSeries)
                Number_PointsToEstimate <<- input$EstimatedPoints
                
                RecordedTimeSeries<-ts(RecordedTimeSeries,  frequency=patternFreq,start=c(year-1,mon+1))
                RegModel <-tslm(RecordedTimeSeries~TimeStamp)
                format(2^31-1, scientific = TRUE)
                plotForecastErrors(RegModel$residuals,"the Linear Regression Model residual error")
                legend("topleft",'groups',c("Histogram","Normal Distribution"), 
                       lty = c(1,1),col = c("red","blue"))
                
                output$CompTables <- renderDataTable({
                        options(digits=3)
                        inFile <- input$file1
                        if (is.null(inFile))
                                return(NULL)  
                        MyData <- read.csv(inFile$datapath, sep=',' )
                        
                        Number_PointsToEstimate <<- input$EstimatedPoints
                        patternFreq<<- input$Points_In_Pattern
                        #                         x<-MyData[,1]
                        #                         MyDate<-MyData[,2]
                        #                         MyDate<<-y[!is.na(MyDate)] 
                        #                         RecordedTimeSeries<<-MyData[,3]
                        
                        patternFreq<<- input$Points_In_Pattern
                        TimeStamp<-1:length(RecordedTimeSeries)
                        Number_PointsToEstimate <<- input$EstimatedPoints
                        RecordedTimeSeries<-ts(RecordedTimeSeries,  frequency=patternFreq,start=c(year-1,mon+1))
                        RegModel <-tslm(RecordedTimeSeries~TimeStamp)
                        
                        Tmean<-mean(RecordedTimeSeries)
                        SumSELM<<-sum((RegModel$fitted.values-Tmean)^2)
                        
                        SSRLM<-sum ((RecordedTimeSeries-Tmean)^2) 
                        
                        RsequaredLM<<-SumSELM/SSRLM
                        
                        ERL<-sum((RegModel$fitted.values-RecordedTimeSeries)^2)
                        SSELR<-(ERL/length(RecordedTimeSeries))
                        
                        AICRL<-AIC (RegModel)
                        BICRL<-BIC (RegModel)
                        
                        RsequaredHWMul<-format(round(RsequaredHWMul, 2), nsmall = 2)
                        RsequaredHWAdd<-format(round(RsequaredHWAdd, 2), nsmall = 2)
                        RsequaredLM<-format(round(RsequaredLM, 2), nsmall = 2)
                        SSEHWMUL<-format(round(SSEHWMUL, 2), nsmall = 2)
                        SSEHWADD<-format(round(SSEHWADD, 2), nsmall = 2)
                        SSELR<-format(round(SSELR, 2), nsmall = 2)
                        HWMAIC<-format(round(HWMAIC, 2), nsmall = 2)
                        HWMBIC<-format(round(HWMBIC, 2), nsmall = 2)
                        HWAAIC<-format(round(HWAAIC, 2), nsmall = 2)
                        HWABIC<-format(round(HWABIC, 2), nsmall = 2)
                        AICRL<-format(round(AICRL, 2), nsmall = 2)
                        BICRL<-format(round(BICRL, 2), nsmall = 2)
                        
                        #                         mdat <- matrix(c(
                        #                                 "Multiplicative Holt-Winters",RsequaredHWMul,SSEHWMUL, 3,HWMAIC,HWMBIC,
                        #                                 "Additive Holt-Winters",      RsequaredHWAdd,SSEHWADD, 3,HWAAIC,HWABIC,
                        #                                 "Linear Regression",          RsequaredLM,   SSELR,    2,AICRL, BICRL),
                        #                                 nrow = 3, ncol = 6, byrow = TRUE,
                        #                                 dimnames = list(c("row1", "row2", "row3"),
                        #                                                 c("Model", "Rsquared", "MSE","Number of Parameters", "AIC", "BIC")))
                        
                        models<-c("Holt-Winters Multiplicative", "Holt-Winters Additive", "Linear Regression" )
                        rank<- c(RsequaredHWMul,RsequaredHWAdd,RsequaredLM)   
                        test<-cbind(models, rank)
                        test<-data.frame(test)
                        temp<-test[order(test$rank,decreasing = TRUE),]
                        temp<-as.matrix(temp)
                        mdat <- matrix(c(
                                "Best Model" ,temp[1,1],#,temp[1,2],
                                "Second Best Model",temp[2,1],#,temp[2,2],
                                "Third Best Model" ,temp[3,1]),#,temp[3,2]),
                                nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("row1", "row2", "row3"),
                                                c("Model Rank", "Model Name")))
                        
                        
                        mdat
                }, options = list(pageLength = 15,searching = FALSE,paging = FALSE,ordering=FALSE,info=FALSE),rownames= FALSE#options=list(iDisplayLength=10,bFilter = FALSE,paging = FALSE,ordering=FALSE,info=FALSE)
                )
                
        })
        
        
        ###########Help and Citation goes here
        output$downloadcitation <- downloadHandler(
                filename = function() { 
                        'Citation.txt'#paste(input$dataset, 'EstimatedData.csv', sep='') #
                },
                
                content = function(file) {
                        
                        ref1<-("E. A. Mohammed and C. Naugler, Open Source Software for Demand Forecasting of Clinical Laboratory Test Volumes  using Time Series Analysis")
                       
                        Myref<-c(ref1)
                        writeLines(Myref, file)
                }
        )
        ######################################
        output$downloadTextDemo <- downloadHandler(
                filename = function() { 
                        'SampleData.txt'#paste(input$dataset, 'EstimatedData.csv', sep='') #
                },
                
                content = function(file) {
                        
                        tempData<-c("5381010", "5617374", "5511991", "5192649", "5190415", "5393446", "5627847", "5466107", "5042830", "5601075",
                                    "5437848", "5947648", "5631941", "5922973", "5827747", "5459750", "5497993", "5658741", "5976369", "5813401",
                                    "5309724", "5857164", "5543738", "6070444", "6191628", "6229208", "5929340", "5888116", "5726679", "5868913",
                                    "6116892", "5855872", "5302823", "6111356", "5642618", "6340995", "6228022", "6406441", "6229813", "6001708")
                        
                        
                        writeLines(tempData,file)
                }
        )
        #####################################
        output$downloadCSVDemo<- downloadHandler(
                filename = function() { 
                        'SampleData.csv'#paste(input$dataset, 'EstimatedData.csv', sep='') #
                },
                
                content = function(file) {
                        
                        DateIndex<-c(1:40)
                        
                        Date<- c(201104, 201105, 201106, 201107, 201108, 201109, 201110, 201111, 201112, 201201,
                                 201202, 201203, 201204, 201205, 201206, 201207, 201208, 201209, 201210, 201211,
                                 201212, 201301, 201302, 201303, 201304, 201305, 201306, 201307, 201308, 201309,
                                 201310, 201311, 201312, 201401, 201402, 201403, 201404, 201405, 201406, 201407)
                        
                        Testvol<-c( 5381010, 5617374, 5511991, 5192649, 5190415, 5393446, 5627847, 5466107, 5042830, 5601075,
                                    5437848, 5947648, 5631941, 5922973, 5827747, 5459750, 5497993, 5658741, 5976369, 5813401,
                                    5309724, 5857164, 5543738, 6070444, 6191628, 6229208, 5929340, 5888116, 5726679, 5868913,
                                    6116892, 5855872, 5302823, 6111356, 5642618, 6340995, 6228022, 6406441, 6229813, 6001708)
                        
                        MyList<-list(DateIndex,Date,Testvol)
                        write.table(MyList, file,
                                    sep=',',col.names = c("Date_Index","Date","Test_Volumes"),row.names = FALSE)
                        
                        
                }
        )
        #
        #####################################
        # image2 sends pre-rendered images
        output$image2 <- renderImage({
                #                 if (is.null(input$picture))
                #                         return(NULL)
                
                #if (input$picture == "face") {
                return(list(
                        src = "EmadPic.jpg",
                        contentType = "image/jpg",
                        alt = "Face"
                ))
                #} 
                
        }, deleteFile = FALSE)
        
        #######################################
        
        #####################################################
        # output$image3 <- renderImage({
        #         #                 if (is.null(input$picture))
        #         #                         return(NULL)
        #         
        #         #if (input$picture == "face") {
        #         return(list(
        #                 src = "BHF.JPG",
        #                 contentType = "image/jpg",
        #                 alt = "Face"
        #         ))
        #         #} 
        #         
        # }, deleteFile = FALSE)
        
        #####################################################
        
        #####################################################
        output$image4 <- renderImage({
                #                 if (is.null(input$picture))
                #                         return(NULL)
                
                #if (input$picture == "face") {
                return(list(
                        src = "Naugler_headshot.JPG",
                        contentType = "image/jpg",
                        alt = "Face"
                ))
                #} 
                
        }, deleteFile = FALSE)
        
        #####################################################
        
        
})