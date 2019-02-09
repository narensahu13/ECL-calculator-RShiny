#####-----------------------------------------Beginning-----------------------------------------------
##This Tool works on the specific data types. for the format of data please see the data files.
#This tool calculates PiT PD, LGD (by colltaeral based haircut approach), EAD, and ECL . This assumes TTC PD is given.
# The data sets are demo data created specific for the tool.
function(input, output,session){
  
#####-----------------------------------------Load Data-----------------------------------------------
# Load all the data files  
  observeEvent(input$upload,{
   # Load bank demo data (facility and collateral sheets in one excel file)
    infile<-input$bank_raw_data
    facility<<-read_excel(infile$datapath,sheet = "Facility")
    #    infile<-input$collateral_file
    collateral<<-read_excel(infile$datapath,sheet = "Collateral")
    #Load haircut file
    infile<-input$haircut_file
    haircut<<-read_excel(infile$datapath)
    #Load TTC PD file which contains TTC PD sheet, Scenario data (3 cases), assetcorrelation data
    infile<-input$pd_table
    TTCPD<<-read_excel(infile$datapath,sheet = "TTC PD",trim_ws = TRUE,range = cell_cols("A:G"))
    CCI<<- read_excel(infile$datapath,sheet = "PiT Models",range = cell_cols("A:G"))
    Correlation<<-read_excel(infile$datapath,sheet = "Correlation",range = cell_cols("A:G"))
    facilityrow<<-nrow(facility)
    collateralrow<<-nrow(collateral)
    showModal(modalDialog(title= "Data Upload", "Data has been successfully uploaded. Please proceed to the next tab.", easyClose = T))
    

   haircuts<<- callModule(editableDT,"hc",data=reactive(haircut),inputwidth=reactive(100))
    options = list(autoWidth = TRUE,columnDefs = list(list(width = '200px', targets = "_all")))
  })
  
  
 # temp<<-haircuts
 #haircuts<-haircuts2
  #cat(stderr(),namestemp[1,3])
#####---------------------------------------------Staging Calculations---------------------------------------------------
  
  observeEvent(input$run_staging,{
 #   facility <- facilityload()
  #  pdtable <- pdtableload()
    withProgress(message = "Calculation in Progress",{
    stage2dpdthreshold <- input$stage2dpd
    stage3dpdthreshold <- input$stage3dpd
    
    #haircuts<-haircuts
    #Staging based on DPD rule, watchlist flag, restructuring flag, expired loan indicator,manual override 
    facility$DPDRule <- ifelse(as.numeric(facility$DPD) < stage2dpdthreshold,1,ifelse(as.numeric(facility$DPD)<stage3dpdthreshold,2,3))
    facility$watchlist <- ifelse(facility$WATCH_LIST_FLAG == "N",1,2)
    facility$restructure <- ifelse(facility$RESTRUCTURED_RESCHEDULED_FLAG == "N",1,2)
    facility$expired <- ifelse(as.Date(facility$MIS_DATE) <= as.Date(facility$MATURITY_DATE),0,3)
    facility$finalstage <- do.call(pmax,facility[,c("DPDRule","watchlist","restructure","expired")])
    tables <- as.data.frame(table(facility$finalstage,facility$SEGMENT))
    table2 <- as.data.frame(table(facility$finalstage))
    colnames(tables)<-c("Stage","Segment","Number of Facilities")
    colnames(table2)<-c("Stage","Number of Facilities")
    
    stagesheet<<-cbind.data.frame(facility$CUSTOMER_ID,facility$FACILITY_ID,facility$SEGMENT,as.Date(facility$MATURITY_DATE),facility$DPD,facility$WATCH_LIST_FLAG,
                                 facility$RESTRUCTURED_RESCHEDULED_FLAG, facility$expired,facility$DPDRule,facility$watchlist,facility$restructure)
    stagesheet$MANUAL_OVERRIDE<<-as.numeric("")
    stagesheet<<-cbind.data.frame(stagesheet,facility$finalstage)
    colnames(stagesheet)<<-c("CUSTOMER_ID","FACILITY_ID","SEGMENT","MATURITY_DATE","DPD","WATCHLIST_FLAG",
                            "RESTRUCTURED_FLAG","STAGE_EXPIRED_RULE","STAGE_DPD_RULE","STAGE_WATCHLIST_RULE","STAGE_RESTRUCTURED_RULE","MANUAL_OVERRIDE","FINAL_STAGE")

#####------------------------------------Plotting Staging Graphs---------------------------------------------------
    theme_update(plot.title = element_text(hjust = 0.5))
    
    output$plot1 <- renderPlot({
      ggplot(table2,aes(x=Stage,y=`Number of Facilities`,label=`Number of Facilities`))+
        geom_bar(stat = "identity",position = "dodge",width=0.7, fill = "#FF6667")+  ylab("Number of Deals")+xlab("Stage")+
        geom_text(size = 6, position = position_stack(vjust = 0.5))+
        theme(legend.position = "bottom",plot.title = element_text(size = 15,face = "bold"))+labs(fill="Stage")
    })
    
    output$plot2<-renderPlot({
      ggplot(filter(tables,Segment==input$Segment), aes(Stage, `Number of Facilities`,label=`Number of Facilities`)) +
        geom_bar(aes(fill = factor(Stage)), position = "dodge", stat="identity")+ggtitle(input$Segment)+
        theme(legend.position = "bottom",plot.title = element_text(size = 15,face = "bold"))+labs(fill="Stage")+
        ylab("Number of Deals")+xlab("Stage")+ geom_text(size = 6, position = position_stack(vjust = 0.5)) +
      theme(legend.position = "bottom",plot.title = element_text(size = 15,face = "bold"))+labs(fill="Stage")
    })
    callModule(editableDT,"table1",data=reactive(stagesheet),inputwidth=reactive(100))
    options = list(alengthMenu=c(5,30,30),iDisplatLength=5,bSortClasses=TRUE,bAutoWidth=FALSE,aoColumn = list(list(sWidth = "30px", sWidth = "30px",
                                                                                                                   sWidth = "30px", sWidth = "30px")),autoWidth = TRUE,pageLength = 20,columnDefs = list(list(width = '200px', targets = "_all")))
    
#####-----------------------------------------LGD Calculations----------------------------------------------------------------    
 
 
    totals<<-collateral %>% group_by(CUSTOMER_ID,COLLATERAL_TYPE) %>% summarise(TotalCollateral=sum(COLLATERAL_VALUE)) 
    
   # observe({
      
     # haircuts<<-haircuts()
      #totals<<-totals()
      #cat(stderr(),"AAAA")
      total<<-merge(totals[,c("CUSTOMER_ID","COLLATERAL_TYPE","TotalCollateral")],haircuts()[,c("COLLATERAL_TYPE","HAIRCUTS")])
      #cat(stderr(),class(total$HAIRCUTS))
  #  }) 
            #cat(stderr(),haircut()[2,3])
            # cat(stderr(),names(total))
            # cat(stderr(),"TC",class(total$TotalCollateral),"HC",class(total$HAIRCUTS))
            ###calculate haircut adjusted collateral
    #total<<-total()
            total$HCadjColl<<-total$TotalCollateral*(1-as.numeric(total$HAIRCUTS))
            total1<<-total %>% group_by(CUSTOMER_ID) %>% summarise(TotalHCadjColl=sum(HCadjColl))
            
            total2<<-facility %>% group_by(CUSTOMER_ID) %>% summarise(TotalEAD=sum(OUTSTANDING_BALANCE))
            
            total3<<-merge(total1[,c("CUSTOMER_ID","TotalHCadjColl")],total2[,c("CUSTOMER_ID","TotalEAD")])
            
            UnsecuredLGD<<-input$unsecured_lgd
            total3$LGD<<-UnsecuredLGD*(total3$TotalEAD-total3$TotalHCadjColl)/total3$TotalEAD
            
            # Correlation <- Correlation
            for(i in 1:nrow(total3)) {
              total3$LGD[i]<<-if(total3$TotalEAD[i]-total3$TotalHCadjColl[i]<=0) input$floor_lgd else total3$LGD[i]
              total3$LGD[i]<<-if(total3$LGD[i]<0.1) input$floor_lgd else total3$LGD[i]    ###Floor LGD at 10%
            }
            
            total3<-total3 %>% mutate_each(funs(round(.,2)), LGD, TotalHCadjColl,TotalEAD)
    
    #################   PiT PD   ##################
    #####Indicators###############
   # total3<-total3
    
    bullet<-c(1,rep(0,times=479))
    annual<-rep(c(1,rep(0,times=11)),40)
    semi_annual<-rep(c(1,rep(0,times=5)),80)
    quarterly<-rep(c(1,rep(0,times=2)),160)
    monthly<-rep(1,times=480)
    Indicator<<-cbind.data.frame(bullet,annual,semi_annual,quarterly,monthly)
    #colnames(Indicator)<-c("bullet","annual","semi_annual","quarterly","monthly")
    #Indicator<-read_excel('indicator.xlsx')
    #View(TTCPD)
    #View(CCI)
    #View(Correlation)
    
    # invnor<-matrix(data = 0,nrow = nrow(TTCPD),ncol = length(TTCPD))
    # for(i in 1 : nrow(TTCPD)){
    #   for(j in 1: length(TTCPD)) {
    #     invnor[i,j]= qnorm(as.numeric(TTCPD[i,j]))
    #   }
    # }
    PIT <<- data.frame(matrix(vector(), nrow= nrow(TTCPD), ncol = length(TTCPD)-1,
                             dimnames=list(c(), c("Segment", "Year", "Rating_1","Rating_2","Rating_3","Rating_4"))),stringsAsFactors=F)
    #PIT<-matrix(data=0,nrow = nrow(TTCPD),ncol = length(TTCPD)-1)
    PIT$Segment<<-TTCPD$Segments
    PIT$Year<<-TTCPD$`Year / DPD`
    PIT$concatenate<<-paste(TTCPD$`Year / DPD`,TTCPD$Segments,sep = "")
    #PIT<-as.data.frame(append(x = PIT1[,c(1,2)],values = TTCPD[,c(2,3)]))
    #PIT<-as.data.frame(replace(PIT1[,c(1,2)],PIT1[,c(1,2)],TTCPD[,c(2,3)]))
    #PIT[,c(1,2)]<-as.data.frame(TTCPD[,c(2,3)])
    for(i  in 1:nrow(TTCPD)){
      for(j in 4:length(TTCPD)){
        PIT[i,j-1]<<-pnorm(as.numeric((qnorm(as.numeric(TTCPD[i,j]))-sqrt(as.numeric(Correlation[i,j]))*CCI[i,3])/
                                       sqrt((matrix(1,nrow =nrow(TTCPD),ncol =length(TTCPD))[i,j]-as.numeric(Correlation[i,j])))))
      }
    }
    
    
    ##########    Final Data Set for ECL Calculation    ###################
    #total<-merge(total[,c("CUSTOMER_ID","COLLATERAL_TYPE","TotalCollateral")],haircuts[,c("COLLATERAL_TYPE","HAIRCUTS")])
    #Datainput1<-merge(total2[,c("CUSTOMER_ID","LGD")],stagesheet[,c("Customer_ID","Facility_ID","Final_Stage")],all.x = TRUE,all.y = TRUE)
    
    Datainput1<<-merge(facility[,c("CUSTOMER_ID","FACILITY_ID","SEGMENT","MIS_DATE","MATURITY_DATE","PRINCIPAL_PAYMENT_FREQUENCY","INTEREST_PAYMENT_FREQUENCY",
                                  "OUTSTANDING_BALANCE","UNDRAWN_COMMIT","INTEREST_RATE","DISCOUNT_RATE","DPD")],total3[,c("CUSTOMER_ID","LGD")],all.x = TRUE,all.y = TRUE)
    stage<-stagesheet[,c("FACILITY_ID","FINAL_STAGE")]
    #Datainput<-merge(Datainput1[,],stage[,"FINAL_STAGE"])
    Datainput<-merge(Datainput1,stage,by="FACILITY_ID")
    Datainput$LGD[is.na(Datainput$LGD)]<-UnsecuredLGD
    Datainput$PDRating<-numeric(length = nrow(Datainput))
    for (i in 1: nrow(Datainput)) {
      Datainput$PDRating[i]<- if(Datainput$DPD[i]==0) 1 else if (Datainput$DPD[i]<30) 2 else if (Datainput$DPD[i]<60) 3 else if (Datainput$DPD[i]<90) 4 else 5
    }
    Datainput$MIS_DATE<-as.Date(Datainput$MIS_DATE,origin='1970-1-1')
    Datainput$MATURITY_DATE<-as.Date(Datainput$MATURITY_DATE,origin='1970-1-1')
    for(i in 1:nrow(Datainput)){
    #   cat(stderr(),"index",i,as.Date(if((as.Date(Datainput$MIS_DATE[i]+365))<as.Date(Datainput$MATURITY_DATE[i])) as.Date(Datainput$MIS_DATE[i]+365) else Datainput$MATURITY_DATE[i],origin="1970-01-01")
    # )
      Datainput$Monthselect[i]<- if(Datainput$MIS_DATE[i]+365<Datainput$MATURITY_DATE[i]) Datainput$MIS_DATE[i]+365 else Datainput$MATURITY_DATE[i]
    }
    Datainput$Monthselect<-as.Date(Datainput$Monthselect,origin ='1970-1-1' )
    
    ######  ECL computation  ########
    #library(zoo)
    #as.Date(as.yearmon(as.Date(Datainput$MATURITY_DATE[2])) -(1/12), frac = 1)
    #Datainput$MATURITY_DATE[2]
    #for(i in 1:(Datainput$MATURITY_DATE[1]-Datainput$MIS_DATE)/30)
    #Datainput$MATURITY_DATE[1]-days_in_month(Datainput$MATURITY_DATE[1])
    Datainput<<-Datainput
    Datainput2<<-subset(x = Datainput,( (FINAL_STAGE==1  | FINAL_STAGE==2)&  MATURITY_DATE>MIS_DATE))
    #Datainput3<<-subset(x = Datainput,( FINAL_STAGE==2  &  MATURITY_DATE>MIS_DATE)) 
    stage3<<-subset(Datainput,FINAL_STAGE==3)
    stage_null<<-subset(Datainput,MATURITY_DATE==MIS_DATE & FINAL_STAGE==1)
    # Datainput2$Monthselect <- as.Date(Datainput2$Monthselect,origin = "1970-1-1")
    # Datainput3$Monthselect <- as.Date(Datainput3$Monthselect,origin = "1970-1-1")
    # stage3$Monthselect<-as.Date(stage3$Monthselect,origin = "1970-1-1")
    # stage_null$Monthselect<-as.Date(stage_null$Monthselect,origin = "1970-1-1")
    # 
    
    #observe(input$CustomerId,{
    output$lgd <- renderDataTable(
      total3, options = list(alengthMenu=c(1,2,3,4),iDisplatLength=5,bSortClasses=TRUE,bAutoWidth=FALSE,aoColumn = list(list(sWidth = "30px", sWidth = "30px",sWidth="30px",
                                                                                                                             sWidth = "30px", sWidth = "30px")),pageLength = 20, autoWidth = TRUE,
                             searching = TRUE,columnDefs = list(list(width = '200px', targets = "_all")))
    )
    
      #write.csv(as.data.frame(total3),"lgd.csv")
  })
  })
  
observeEvent(input$calc_ecl,{

  withProgress(message = "ECL Calculation in Progress", detail = "This may take a while....",{

    total3<-total3
    Datainput2<-Datainput2
    #Datainput3<-Datainput3
    stage3<-stage3
    stage_null<-stage_null
   
    # rm(CCI,collateral,facility,Datainput,Datainput1,haircuts,stage,total,total1,total2,Correlation)
    ##############################################################################################################
    #############   ECL Calculation  ###############################
    Datainput2$MIS_DATE<-as.Date.character(Datainput2$MIS_DATE)
    Datainput2$MATURITY_DATE<-as.Date.character(Datainput2$MATURITY_DATE)
   # Datainput3$MIS_DATE<-as.Date.character(Datainput3$MIS_DATE)
    #Datainput3$MATURITY_DATE<-as.Date.character(Datainput3$MATURITY_DATE)
   
    stage3$ECL<-stage3$OUTSTANDING_BALANCE*stage3$LGD
    if(nrow(stage_null)>0)  stage_null$ECL<-0  else stage_null<-0
    
    
    for (j in 1: nrow(Datainput2)) {
      
      #ecltable<-data.matrix()
      ecltable<-as.data.frame(as.Date(seq.Date(from = as.Date(Datainput2$MATURITY_DATE[j]),to = as.Date(Datainput2$MIS_DATE[j]),by = "-1 month")))
      if (ecltable[nrow(ecltable),]== Datainput2$MIS_DATE[j]) ecltable[nrow(ecltable),]<-Datainput2$MIS_DATE[j] else ecltable[nrow(ecltable)+1,]<-Datainput2$MIS_DATE[j]
      colnames(ecltable)<-"Dates"
      #dates<-as.data.frame(seq(as.Date(Datainput$MIS_DATE)[1],as.Date(Datainput$MATURITY_DATE)[1],by="month"))
      #ecltable$datediff<-numeric(length=nrow(ecltable))
      for (i in  nrow(ecltable) : 1) {
        ecltable$datediff[i]<-as.data.frame(ecltable$Dates[i]-ecltable$Dates[i-1])
      }
      #####Payment indicators
      if (Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==1) PriIndicator<-Indicator$annual else
        if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==2) PriIndicator<-Indicator$semi_annual else
          if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==12) PriIndicator<-Indicator$monthly else
            if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==4) PriIndicator<-Indicator$quarterly else PriIndicator<-Indicator$bullet
      
      if (Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==1) IntIndicator<-Indicator$annual else
        if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==2) IntIndicator<-Indicator$semi_annual else
          if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==12) IntIndicator<-Indicator$monthly else
            if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==4) IntIndicator<-Indicator$quarterly else IntIndicator<-Indicator$bullet
      
      #ecltable<-as.data.frame(dates[order(as.Date(dates$`seq(as.Date(Datainput$MIS_DATE)[1], as.Date(Datainput$MATURITY_DATE)[1], by = "month")`,"%m/%d/%Y"),decreasing = TRUE),])
      #ecltable<-dates
      #if (ecltable[nrow(ecltable),]== Datainput$MIS_DATE[1]) ecltable[nrow(ecltable),]<-Datainput$MIS_DATE[1] else ecltable[nrow(ecltable)+1,]<-Datainput$MIS_DATE[1]
      ecltable$datediff<-as.numeric(ecltable$datediff)
      for(i in 1: nrow(ecltable)-1){
        ecltable$priIndicator[i]<-PriIndicator[i]
        ecltable$intIndicator[i]<-IntIndicator[i]
        ecltable$priIndicator[nrow(ecltable)]<-0
        ecltable$intIndicator[nrow(ecltable)]<-0
      }
      sumpriindicator<-sum(ecltable$priIndicator)
      ##########################################   TEST
      #Datainput2$sum[j]<-sum(ecltable$intIndicator[1:nrow(ecltable)])
      
      ####################################################
      repayment<-numeric(length = nrow(ecltable))
      for(i in 1:nrow(ecltable)){
        repayment[i]<-ecltable$priIndicator[i]*Datainput2$OUTSTANDING_BALANCE[j]/sumpriindicator[]
      }
      ecltable$repayment<-repayment
      ecltable$repayment <- as.vector(ecltable$repayment)
      
      PrinEAD<-numeric(length = nrow(ecltable))
      PrinEAD[1]<-0
      for(i in 2:nrow(ecltable)){
        PrinEAD[i]<-Datainput2$OUTSTANDING_BALANCE[j]- sum(as.vector(ecltable$repayment[i:nrow(ecltable)]))
      }
      ecltable$PrinEAD <- PrinEAD
      
      accrint <- rep(0,nrow(ecltable))
      
      for (i in 1:nrow(ecltable)) {
        accrint[i] <- ecltable$datediff[i]*Datainput2$INTEREST_RATE[j]/-365*ecltable$PrinEAD[i]
      }
      
      ecltable$accrint <- accrint
      
      intpayment <- rep(0,nrow(ecltable))
      
      
      intpayment[nrow(ecltable)] <-0
      
      for (i in 1:nrow(ecltable)) {
        intpayment[nrow(ecltable)-i] <- (sum(ecltable$accrint[(nrow(ecltable)):(nrow(ecltable)+1-i)]) - sum(intpayment[(nrow(ecltable)):(nrow(ecltable)+1-i)]))*(ecltable$intIndicator[nrow(ecltable)-i])
      }
      intpayment[nrow(ecltable)] <- 0
      ecltable$intpayment <- intpayment
      intEAD <- rep(0,nrow(ecltable))
      
      for (i in 1:nrow(ecltable)) {
        intEAD[i] <-  sum(ecltable$accrint[(i):(nrow(ecltable))])-sum(intpayment[(i):(nrow(ecltable))])
      }
      
      ecltable$intEAD <- intEAD
      ecltable$intEAD[1] <- 0
      
      ecltable$totalEAD <- ecltable$PrinEAD + ecltable$intEAD
      
      if(Datainput2$UNDRAWN_COMMIT[j]>0)
        for(i in 1:nrow(ecltable)){
          ecltable$FinalEAD[i]<-if((Datainput2$MATURITY_DATE[j]-Datainput2$MIS_DATE[j])>365) ecltable$totalEAD[i] + Datainput2$UNDRAWN_COMMIT[j]*0.5 else ecltable$totalEAD[i] + Datainput2$UNDRAWN_COMMIT[j]*0.2
        }
      else for(i in 1:nrow(ecltable)) { ecltable$FinalEAD[i]<- ecltable$totalEAD[i] }
      
      ecltable$daysdiscount <- rep(0,nrow(ecltable))
      ecltable$daysdiscount[1] <- 0
      lifetime <- Datainput2$MATURITY_DATE[j] - Datainput2$MIS_DATE[j]
      lifetime
      ecltable$daysdiscount[2] <- lifetime
      
      if (nrow(ecltable)>2) {
        for (i in 3:nrow(ecltable)) {
          ecltable$daysdiscount[i] <- ecltable$daysdiscount[(-1+i)] + ecltable$datediff[i-1]
        }
      } else {
        ecltable$daysdiscount[2] <- lifetime
      }
      
      ecltable$segment <- rep(Datainput2$SEGMENT[j],nrow(ecltable))
      ecltable$PDrating <- rep(Datainput2$PDRating[j],nrow(ecltable))
      ecltable$PDyear <- ecltable$daysdiscount%/%365.001+1
      ecltable$concatenate <- paste(ecltable$PDyear,ecltable$segment,sep = "")
      
      
      if (Datainput2$PDRating[j]==1) {
        tempPIT <- PIT$Rating_1
      } else if (Datainput2$PDRating[j]==2) {
        tempPIT <- PIT$Rating_2
      } else if (Datainput2$PDRating[j]==3) {
        tempPIT <- PIT$Rating_3
      } else if (Datainput2$PDRating[j]==4) {
        tempPIT <- PIT$Rating_4
      } else {
        tempPIT <- 1
      }
      
      ecltable$YearPD <- match(ecltable$concatenate,PIT$concatenate)
      
      if (Datainput2$PDRating[j]==1) tempPIT<-PIT$Rating_1 else if(Datainput2$PDRating[j]==2) tempPIT<-PIT$Rating_2 else if(Datainput2$PDRating[j]==3) tempPIT<-PIT$Rating_3 else if(Datainput2$PDRating[j]==4) tempPIT<-PIT$Rating_4 else tempPIT<-1
      ecltable$YearPD <- tempPIT[match(ecltable$concatenate,PIT$concatenate)]
      
      ecltable$PD <- 1-(1-ecltable$YearPD)^(ecltable$datediff/-365)
      ecltable$PD[1] <- 0
      
      ecltable$LGD <- Datainput2$LGD[j]
      ecltable$disountrate <- 1/(1+Datainput2$DISCOUNT_RATE[j])^(ecltable$daysdiscount/365)
      ecltable$ECL <- ecltable$PD*ecltable$LGD*ecltable$FinalEAD*ecltable$disountrate
      
      # CumECL <- rep(0,nrow(ecltable))
      # for (i in 1:nrow(ecltable)) {
      #   CumECL[i] <- sum(ecltable$ECL[(i):(nrow(ecltable))])
      # }
      # CumECL[1] <- 0
      # ecltable$CumECL <- CumECL
      # return(Datainput2$Monthselect[j])
      # ecltable$M12select[i] <- if (Datainput2$Monthselect[j]<ecltable$Dates[i]) 0 else if (Datainput2$Monthselect[j]-ecltable$Dates[i]>abs(ecltable$datediff[i])) 1 else ((Datainput2$Monthselect[j]-ecltable$Dates[i])/abs(ecltable$datediff[i]))
      # ecltable$M12select[1] <- 0
      # ecltable$M12ECL[i] <- ecltable$M12select[i] * ecltable$ECL[i]
      # # 
      
      ecltable$M12select <- ifelse(Datainput2$Monthselect[j]<ecltable$Dates,0,ifelse(Datainput2$Monthselect[j]-ecltable$Dates>abs(ecltable$datediff),1,(Datainput2$Monthselect[j]-ecltable$Dates)/abs(ecltable$datediff)))
      ecltable$M12select[1] <- 0
      ecltable$M12ECL <- ecltable$M12select * ecltable$ECL
      
      # M12CumECL <- rep(0,nrow(ecltable))
      # 
      # for (i in 1:nrow(ecltable)) {
      #   M12CumECL[i] <- sum(ecltable$M12ECL[(i):(nrow(ecltable))])
      # }
      # 
      # ecltable$M12CumECL <- M12CumECL
      # ecltable$M12CumECL[1] <- 0
      
      Datainput2$ECL[j] <- if(Datainput2$FINAL_STAGE[j]==1) sum(ecltable$M12ECL) else sum(ecltable$ECL)
      #ecltable$CumECL[2]

    }
    
    ############################################################################
    if(stage_null==0) Report<-rbind(Datainput2,stage3)   else Report<-rbind(Datainput2,stage3,stage_null)
    
    show_ecl<-Report[,c("FACILITY_ID","CUSTOMER_ID","SEGMENT","MATURITY_DATE","PRINCIPAL_PAYMENT_FREQUENCY","INTEREST_PAYMENT_FREQUENCY","OUTSTANDING_BALANCE","UNDRAWN_COMMIT","INTEREST_RATE","LGD","FINAL_STAGE","ECL")]
    show_ecl<-show_ecl %>% mutate_each(funs(round(.,2)), OUTSTANDING_BALANCE, UNDRAWN_COMMIT,LGD,ECL)
    Report$PDRating<-NULL
    Report$Monthselect<-NULL
    Report<-Report %>% mutate_each(funs(round(.,2)), OUTSTANDING_BALANCE, UNDRAWN_COMMIT,LGD,ECL)
    # write_xlsx(Report,"Report_ODB_R.xlsx")
    table2<-Report %>% group_by(SEGMENT) %>% summarise(`Total EAD`=sum(OUTSTANDING_BALANCE),`Total ECL`=sum(ECL))
    table3<-Report %>% group_by(FINAL_STAGE) %>% summarise(`Total EAD`=sum(OUTSTANDING_BALANCE),`Total ECL`=sum(ECL))
    table4<-Report %>% group_by(SEGMENT,FINAL_STAGE) %>% summarise(`Number of Facilities`=n(),`Total EAD`=sum(OUTSTANDING_BALANCE),`Total ECL`=sum(ECL))
    
    # p6<-filter(table2,SEGMENT=="PF")
    # p7<-filter(table2,SEGMENT=="Micro")
    # p8<-filter(table2,SEGMENT=="Corporate")
    # p9<-filter(table2,SEGMENT=="Retail")
    # ggplot(table4,aes(FINAL_STAGE,`Total ECL`,fill=SEGMENT))+geom_bar(position = position_dodge(0.7),stat = "identity")+
    #   geom_text(aes(label=abs(`Total ECL`)),vjust=1.5)
    # output$show_ecl <- renderDataTable(
    #   Report, options = list(pageLength = 20, autoWidth = TRUE,
    #                          searching = TRUE)
    # )
    
    p6<-filter(table2,SEGMENT=="PF")
    p7<-filter(table2,SEGMENT=="Micro")
    p8<-filter(table2,SEGMENT=="Corporate")
    p9<-filter(table2,SEGMENT=="Retail")
    
    # output$plot3<-renderPlot({ggplot(table4,aes(FINAL_STAGE,`Total ECL`,fill=SEGMENT))+geom_bar(position = position_dodge(0.7),stat = "identity")+
    #     geom_text(aes(label=abs(`Total ECL`)),vjust=1.5)
    # })
    
    # ggplot(table4,aes(FINAL_STAGE,`Total ECL`,fill=SEGMENT))+geom_bar(position = position_dodge(0.7),stat = "identity")+
    #   geom_text(aes(label=abs(`Total ECL`)),vjust=1.5)
    # 
    # plot_ly(table3, x = ~FINAL_STAGE, y = ~`Total ECL`, type = 'bar', name = 'Total ECL') %>%
    #   add_trace(y = ~`Total EAD`, name = 'Total EAD') %>%
    #   layout(yaxis = list(title = 'Amount'), barmode = 'group') 
    
    totalECL<- sum(Report$ECL)
    stageECL<-Report %>% group_by(FINAL_STAGE) %>% summarise(value = sum(ECL)) %>% filter(value==max(value))
    segmentECL<-Report %>% group_by(SEGMENT) %>% summarise(value = sum(ECL)) %>% filter(value==max(value))
    accountECL <-Report %>% group_by(CUSTOMER_ID) %>% summarise(value=sum(ECL)) %>% filter(value==max(value))
    
      output$value1 <- renderValueBox({ 
        valueBox(
          formatC(totalECL, format="d", big.mark=',')
          ,'Total Expected Credit Loss'
          ,icon = icon("usd",lib='glyphicon')
          ,color = "green")  
      })
      
      output$value2 <- renderValueBox({
        valueBox(
          formatC(stageECL$value, format="d", big.mark=',')
          ,paste('Top ECL by Stage:',stageECL$FINAL_STAGE)
          ,icon = icon("stats",lib='glyphicon')
          ,color = "purple")  
      })
      
      output$value3 <- renderValueBox({
        valueBox(
          formatC(segmentECL$value, format="d", big.mark=',')
          ,paste('Top ECL by Segment:',segmentECL$SEGMENT)
          ,icon = icon("stats",lib='glyphicon')
          ,color = "yellow")   
      })
      
      output$value4 <- renderValueBox({
        valueBox(
          formatC(accountECL$value, format="d", big.mark=',')
          ,paste('Top ECL by Customer:',accountECL$CUSTOMER_ID)
          ,icon = icon("menu-hamburger",lib='glyphicon')
          ,color = "olive")   
      })

      # output$plot3<-renderPlot({ggplot(table4,aes(FINAL_STAGE,`Total ECL`,fill=SEGMENT))+geom_bar(position = position_dodge(0.7),stat = "identity")+
      #     geom_text(aes(label=abs(`Total ECL`)),vjust=1.5)
      #   })
      output$plot3<- renderPlotly({  plot_ly(table3, x = ~FINAL_STAGE, y = ~`Total EAD`, type = 'bar', name = 'Total EAD') %>%
          add_trace(y = ~`Total ECL`, name = 'Total ECL') %>%
          layout(yaxis = list(title = 'Amount'), barmode = 'group') })
      
      output$plot4<- renderPlotly({  plot_ly(table2, x = ~SEGMENT, y = ~`Total EAD`, type = 'bar', name = 'Total EAD') %>%
          add_trace(y = ~`Total ECL`, name = 'Total ECL') %>%
          layout(yaxis = list(title = 'Amount'), barmode = 'group') })

      output$ecl_report <- renderDataTable(
        Report, options = list(alengthMenu=c(1,2,3,4,5,6,7,8,9,10,11),iDisplatLength=5,bSortClasses=TRUE,bAutoWidth=FALSE,aoColumn = list(list(sWidth = "30px", sWidth = "30px",sWidth="30px",
          sWidth = "30px",sWidth = "30px",sWidth = "30px",sWidth = "30px",sWidth = "30px", sWidth = "30px",sWidth = "30px",sWidth = "30px", sWidth = "30px")), pageLength = 20, autoWidth = TRUE,
                               searching = TRUE,scrollX=TRUE,autoWidth=TRUE,columnDefs = list(list(width = '200px', targets = "_all")))
      )
      
      output$generate_report<-downloadHandler(
      filename=function(){
        paste("Report-",Sys.time(),".xlsx")
      },
      content=function(file){
        write_xlsx(Report,file)
      })
})
    
})

###################################################################################################################################################

observeEvent(input$audit,{
  #rowshow<<-Datainput[which(Datainput$FACILITY_ID==input$facility),c(1:14)]
  rowshow<<-reactive({Datainput[which(Datainput$FACILITY_ID==input$facility),] })
  # rowshow()$MIS_DATE <- as.Date(rowshow()$MIS_DATE)
  # rowshow()$MATURITY_DATE <- as.Date(rowshow()$MATURITY_DATE)  
  output$datainput<- renderTable({rowshow()[,c(1:14)]},digits = 2)
  #output$facility_ecl<- renderTable(rowshow()$FACILITY_ID*rowshow()$LGD) 

  ############################                  AUDIT                      ###############################################################
 # if(match(input$facility,stage3$FACILITY_ID)) return("Stage 3 Facility") else if(match(input$facility,stage_null$FACILITY_ID)) return("MIS DATE = MATURITY DATE")
  #if(FALSE) output$msg<-renderText(return(print("Enter Valid Facility ID")))
  if(any(stage3$FACILITY_ID==input$facility)) 
    output$msg<-renderText(return((print("Stage 3 Facility"))))  else if(any(stage_null$FACILITY_ID==input$facility))  
    output$msg<- renderText(return(print("MIS DATE = MATURITY DATE"))) 
  #else if(any(Datainput$FACILITY_ID!=input$facility)) 
  #  output$msg<-renderText(return(print("Enter correct Facility ID")))
  else if(any(Datainput2$FACILITY_ID==input$facility))
   # output$msg<-renderText(return(print("See ECL Computation below")))
   
  for (j in match(input$facility,Datainput2$FACILITY_ID):match(input$facility,Datainput2$FACILITY_ID)) {
    
    #ecltable<-data.matrix()
    ecltable<-as.data.frame(as.Date(seq.Date(from = as.Date(Datainput2$MATURITY_DATE[j]),to = as.Date(Datainput2$MIS_DATE[j]),by = "-1 month")))
    if (ecltable[nrow(ecltable),]== Datainput2$MIS_DATE[j]) ecltable[nrow(ecltable),]<-Datainput2$MIS_DATE[j] else ecltable[nrow(ecltable)+1,]<-Datainput2$MIS_DATE[j]
    colnames(ecltable)<-"Dates"
    #dates<-as.data.frame(seq(as.Date(Datainput$MIS_DATE)[1],as.Date(Datainput$MATURITY_DATE)[1],by="month"))
    #ecltable$datediff<-numeric(length=nrow(ecltable))
    for (i in  nrow(ecltable) : 1) {
      ecltable$datediff[i]<-as.data.frame(ecltable$Dates[i]-ecltable$Dates[i-1])
    }
    #####Payment indicators
    if (Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==1) PriIndicator<-Indicator$annual else
      if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==2) PriIndicator<-Indicator$semi_annual else
        if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==12) PriIndicator<-Indicator$monthly else
          if(Datainput2$PRINCIPAL_PAYMENT_FREQUENCY[j]==4) PriIndicator<-Indicator$quarterly else PriIndicator<-Indicator$bullet
    
    if (Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==1) IntIndicator<-Indicator$annual else
      if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==2) IntIndicator<-Indicator$semi_annual else
        if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==12) IntIndicator<-Indicator$monthly else
          if(Datainput2$INTEREST_PAYMENT_FREQUENCY[j]==4) IntIndicator<-Indicator$quarterly else IntIndicator<-Indicator$bullet
    
    #ecltable<-as.data.frame(dates[order(as.Date(dates$`seq(as.Date(Datainput$MIS_DATE)[1], as.Date(Datainput$MATURITY_DATE)[1], by = "month")`,"%m/%d/%Y"),decreasing = TRUE),])
    #ecltable<-dates
    #if (ecltable[nrow(ecltable),]== Datainput$MIS_DATE[1]) ecltable[nrow(ecltable),]<-Datainput$MIS_DATE[1] else ecltable[nrow(ecltable)+1,]<-Datainput$MIS_DATE[1]
    ecltable$datediff<-as.numeric(ecltable$datediff)
    for(i in 1: nrow(ecltable)-1){
      ecltable$priIndicator[i]<-PriIndicator[i]
      ecltable$intIndicator[i]<-IntIndicator[i]
      ecltable$priIndicator[nrow(ecltable)]<-0
      ecltable$intIndicator[nrow(ecltable)]<-0
    }
    sumpriindicator<-sum(ecltable$priIndicator)
    ##########################################   TEST
    #Datainput2$sum[j]<-sum(ecltable$intIndicator[1:nrow(ecltable)])
    
    ####################################################
    repayment<-numeric(length = nrow(ecltable))
    for(i in 1:nrow(ecltable)){
      repayment[i]<-ecltable$priIndicator[i]*Datainput2$OUTSTANDING_BALANCE[j]/sumpriindicator[]
    }
    ecltable$repayment<-repayment
    ecltable$repayment <- as.vector(ecltable$repayment)
    
    PrinEAD<-numeric(length = nrow(ecltable))
    PrinEAD[1]<-0
    for(i in 2:nrow(ecltable)){
      PrinEAD[i]<-Datainput2$OUTSTANDING_BALANCE[j]- sum(as.vector(ecltable$repayment[i:nrow(ecltable)]))
    }
    ecltable$PrinEAD <- PrinEAD
    
    accrint <- rep(0,nrow(ecltable))
    
    for (i in 1:nrow(ecltable)) {
      accrint[i] <- ecltable$datediff[i]*Datainput2$INTEREST_RATE[j]/-365*ecltable$PrinEAD[i]
    }
    
    ecltable$accrint <- accrint
    
    intpayment <- rep(0,nrow(ecltable))
    
    
    intpayment[nrow(ecltable)] <-0
    
    for (i in 1:nrow(ecltable)) {
      intpayment[nrow(ecltable)-i] <- (sum(ecltable$accrint[(nrow(ecltable)):(nrow(ecltable)+1-i)]) - sum(intpayment[(nrow(ecltable)):(nrow(ecltable)+1-i)]))*(ecltable$intIndicator[nrow(ecltable)-i])
    }
    intpayment[nrow(ecltable)] <- 0
    ecltable$intpayment <- intpayment
    intEAD <- rep(0,nrow(ecltable))
    
    for (i in 1:nrow(ecltable)) {
      intEAD[i] <-  sum(ecltable$accrint[(i):(nrow(ecltable))])-sum(intpayment[(i):(nrow(ecltable))])
    }
    
    ecltable$intEAD <- intEAD
    ecltable$intEAD[1] <- 0
    
    ecltable$totalEAD <- ecltable$PrinEAD + ecltable$intEAD
    
    if(Datainput2$UNDRAWN_COMMIT[j]>0)
      for(i in 1:nrow(ecltable)){
        ecltable$FinalEAD[i]<-if((Datainput2$MATURITY_DATE[j]-Datainput2$MIS_DATE[j])>365) ecltable$totalEAD[i] + Datainput2$UNDRAWN_COMMIT[j]*0.5 else ecltable$totalEAD[i] + Datainput2$UNDRAWN_COMMIT[j]*0.2
      }
    else for(i in 1:nrow(ecltable)) { ecltable$FinalEAD[i]<- ecltable$totalEAD[i] }
    
    ecltable$daysdiscount <- rep(0,nrow(ecltable))
    ecltable$daysdiscount[1] <- 0
    lifetime <- Datainput2$MATURITY_DATE[j] - Datainput2$MIS_DATE[j]
    lifetime
    ecltable$daysdiscount[2] <- lifetime
    
    if (nrow(ecltable)>2) {
      for (i in 3:nrow(ecltable)) {
        ecltable$daysdiscount[i] <- ecltable$daysdiscount[(-1+i)] + ecltable$datediff[i-1]
      }
    } else {
      ecltable$daysdiscount[2] <- lifetime
    }
    
    ecltable$segment <- rep(Datainput2$SEGMENT[j],nrow(ecltable))
    ecltable$PDrating <- rep(Datainput2$PDRating[j],nrow(ecltable))
    ecltable$PDyear <- ecltable$daysdiscount%/%365.001+1
    ecltable$concatenate <- paste(ecltable$PDyear,ecltable$segment,sep = "")
    
    
    if (Datainput2$PDRating[j]==1) {
      tempPIT <- PIT$Rating_1
    } else if (Datainput2$PDRating[j]==2) {
      tempPIT <- PIT$Rating_2
    } else if (Datainput2$PDRating[j]==3) {
      tempPIT <- PIT$Rating_3
    } else if (Datainput2$PDRating[j]==4) {
      tempPIT <- PIT$Rating_4
    } else {
      tempPIT <- 1
    }
    
    ecltable$YearPD <- match(ecltable$concatenate,PIT$concatenate)
    
    if (Datainput2$PDRating[j]==1) tempPIT<-PIT$Rating_1 else if(Datainput2$PDRating[j]==2) tempPIT<-PIT$Rating_2 else if(Datainput2$PDRating[j]==3) tempPIT<-PIT$Rating_3 else if(Datainput2$PDRating[j]==4) tempPIT<-PIT$Rating_4 else tempPIT<-1
    ecltable$YearPD <- tempPIT[match(ecltable$concatenate,PIT$concatenate)]
    
    ecltable$PD <- 1-(1-ecltable$YearPD)^(ecltable$datediff/-365)
    ecltable$PD[1] <- 0
    
    ecltable$LGD <- Datainput2$LGD[j]
    ecltable$disountrate <- 1/(1+Datainput2$DISCOUNT_RATE[j])^(ecltable$daysdiscount/365)
    ecltable$ECL <- ecltable$PD*ecltable$LGD*ecltable$FinalEAD*ecltable$disountrate
    
    CumECL <- rep(0,nrow(ecltable))
    for (i in 1:nrow(ecltable)) {
      CumECL[i] <- sum(ecltable$ECL[(i):(nrow(ecltable))])
    }
    CumECL[1] <- 0
    ecltable$CumECL <- CumECL
    # return(Datainput2$Monthselect[j])
    # ecltable$M12select[i] <- if (Datainput2$Monthselect[j]<ecltable$Dates[i]) 0 else if (Datainput2$Monthselect[j]-ecltable$Dates[i]>abs(ecltable$datediff[i])) 1 else ((Datainput2$Monthselect[j]-ecltable$Dates[i])/abs(ecltable$datediff[i]))
    # ecltable$M12select[1] <- 0
    # ecltable$M12ECL[i] <- ecltable$M12select[i] * ecltable$ECL[i]
    # #
    
    ecltable$M12select <- ifelse(Datainput2$Monthselect[j]<ecltable$Dates,0,ifelse(Datainput2$Monthselect[j]-ecltable$Dates>abs(ecltable$datediff),1,(Datainput2$Monthselect[j]-ecltable$Dates)/abs(ecltable$datediff)))
    ecltable$M12select[1] <- 0
    ecltable$M12ECL <- ecltable$M12select * ecltable$ECL
    
    M12CumECL <- rep(0,nrow(ecltable))

    for (i in 1:nrow(ecltable)) {
      M12CumECL[i] <- sum(ecltable$M12ECL[(i):(nrow(ecltable))])
    }

    ecltable$M12CumECL <- M12CumECL
    ecltable$M12CumECL[1] <- 0
    
   # Datainput2$ECL[j] <- if(Datainput2$FINAL_STAGE[j]==1) sum(ecltable$M12ECL) else sum(ecltable$ECL)
    #ecltable$CumECL[2]
    
  }
  else
    output$msg<-renderText(return(print("Enter correct Facility ID")))
  output$facility_ecl <- renderTable({
    ecltable}, options = list(aLengthMenu = c(12, 20, 40), iDisplayLength = 12)
  )
  ######################################################################################################
    

})
}  