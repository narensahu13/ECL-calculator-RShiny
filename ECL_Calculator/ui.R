setwd(Dir)
addResourcePath("pdf",Dir)


dashboardPage(
  dashboardHeader(title = "ECL Calculator",
                  tags$li(a(href = 'https://www.bis.org/',
                            img(src = 'pdf/logo.jpg',
                                title = "IFRS 9", height = "45px"),
                            style = "padding-top:4px; padding-bottom:4px;"),
                          class = "dropdown"),
                  tags$li(a(href= 'https://www.bis.org/',icon('send'),title="Visit Us"),class="dropdown")),
  dashboardSidebar(
    sidebarMenu(id="side_tab",
                tags$head(
                  tags$style(
                    HTML(".shiny-notification {
                         height: 100px;
                         width: 800px;
                         position:fixed;
                         top: calc(50% - 50px);;
                         left: calc(50% - 400px);;
                         }
                         "
                    )
                    )),
                menuItem("Introduction",tabName = "introduction",icon = icon("info")),
                menuItem("Data Upload",tabName = "data_upload",icon = icon("upload")),
                menuItem("User Inputs",tabName = "userinputs",icon = icon("user")),
                menuItem("Staging",tabName = "staging",icon = icon("folder-open"),
                         menuSubItem("Calculate Stage",tabName = "calculate_stage",icon = icon("calculator")),
                         menuSubItem("Manual Override",tabName = "manual_override",icon = icon("search-minus"))),
                menuItem("LGD",tabName = "LGD",icon = icon("search-minus")),
                menuItem("ECL",tabName = "ecl",icon = icon("folder-open"),
                         menuSubItem("Calculate ECL",tabName = "calculate_ecl",icon = icon("calculator")),
                         menuSubItem("Report",tabName = "report",icon = icon("files-o"))),
                menuItem("Audit Trail",tabName = "audittrail",icon=icon("searchengin"))
    )),
   dashboardBody(tabItems(
     tabItem(tabName ="introduction",tabBox(tabPanel("Info",id="info",icon = icon("info"),width = "100%",height="100%",
                                            h3('IFRS 9 and expected loss provisioning'),
                                            h5("The International Accounting Standards Board (IASB) and other accounting standard setters set out
                                                principles-based standards on how banks should recognise and provide for credit losses for financial
                                               statement reporting purposes. In July 2014, the IASB issued International Financial Reporting Standard 9
                                               - Financial Instruments (IFRS 9), which introduced an expected credit loss (ECL) framework for the
                                               recognition of impairment. This Executive Summary provides an overview of the ECL framework under
                                               IFRS 9 and its impact on the regulatory treatment of accounting provisions in the Basel capital framework"),
                                            h4("What's different about impairment recognition under IFRS 9?"),
                                            h5("Effective for annual periods beginning on or after 1 January 2018, IFRS 9 sets out how an entity should
                                              classify and measure financial assets and financial liabilities. Its scope includes the recognition of
                                               impairment. In the standard that preceded IFRS 9, the 'incurred loss' framework required banks to
                                               recognise credit losses only when evidence of a loss was apparent. Under IFRS 9's ECL impairment
                                               framework, however, banks are required to recognise ECLs at all times, taking into account past events,
                                               current conditions and forecast information, and to update the amount of ECLs recognised at each
                                               reporting date to reflect changes in an asset's credit risk. It is a more forward-looking approach than its
                                               predecessor and will result in more timely recognition of credit losses."),
                                            h4("Expected credit loss framework - scope of application"),
                                            h5("Under IFRS 9, financial assets are classified according to the business model for managing them and their
                                              cash flow characteristics. In essence, if (a) a financial asset is a simple debt instrument such as a loan,
                                               (b) the objective of the business model in which it is held is to collect its contractual cash flows (and
                                               generally not to sell the asset) and (c) those contractual cash flows represent solely payments of principal
                                               and interest, then the financial asset is held at amortised cost. The ECL framework is applied to those assets
                                               and any others that are subject to IFRS 9's impairment accounting, a group that includes lease receivables,
                                               loan commitments and financial guarantee contracts. For the sake of simplicity, the remainder of this
                                               Summary will focus on the ECL framework as it applies to loans. "),
                                            h4("Three stages of impairment"),
                                            h5("Impairment of loans is recognised - on an individual or collective basis - in three stages under IFRS 9: "),
                                            h5("Stage 1 - When a loan is originated or purchased, ECLs resulting from default events that are
                                               possible within the next 12 months are recognised (12-month ECL) and a loss allowance is established. On
                                               subsequent reporting dates, 12-month ECL also applies to existing loans with no significant increase in
                                               credit risk since their initial recognition. Interest revenue is calculated on the loan's gross carrying amount
                                               (that is, without deduction for ECLs).
                                               In determining whether a significant increase in credit risk has occurred since initial recognition,
                                               a bank is to assess the change, if any, in the risk of default over the expected life of the loan (that is, the
                                                change in the probability of default, as opposed to the amount of ECLs)." ),
                                            h5("Stage 2 - If a loan's credit risk has increased significantly since initial recognition and is no 
                                              considered low, lifetime ECLs are recognised. The calculation of interest revenue is the same as for
                                               Stage 1."),
                                            h5("Stage 3 - If the loan's credit risk increases to the point where it is considered credit-impaired,
                                              interest revenue is calculated based on the loan's amortised cost (that is, the gross carrying amount less
                                               the loss allowance). Lifetime ECLs are recognised, as in Stage 2. "),
                                            h4("Twelve-month versus lifetime expected credit losses"),
                                            h5("ECLs reflect management's expectations of shortfalls in the collection of contractual cash flows.
                                              Twelve-month ECL is the portion of lifetime ECLs associated with the possibility of a loan
                                               defaulting in the next 12 months. It is not the expected cash shortfalls over the next 12 months but the
                                               effect of the entire credit loss on a loan over its lifetime, weighted by the probability that this loss will
                                               occur in the next 12 months. It is also not the credit losses on loans that are forecast to actually default in
                                               the next 12 months. If an entity can identify such loans or a portfolio of such loans that are expected to
                                               have increased significantly in credit risk since initial recognition, lifetime ECLs are recognised.
                                               Lifetime ECLs are an expected present value measure of losses that arise if a borrower defaults
                                               on its obligation throughout the life of the loan. They are the weighted average credit losses with the
                                               probability of default as the weight. Because ECLs also factor in the timing of payments, a credit loss (or
                                               cash shortfall) arises even if the bank expects to be paid in full but later than when contractually due."),
                                            h4("Disclosure"),
                                            h5("Banks subject to IFRS 9 are required to disclose information that explains the basis for their ECL calculations
                                              and how they measure ECLs and assess changes in credit risk. They must also provide a reconciliation of
                                               the opening and closing ECL amounts and carrying values of the associated assets separately for different
                                               categories of ECL (for example, 12-month and lifetime loss amounts) and by asset class"),
                                            h4("Regulatory treatment of accounting provisions"),
                                            h5("The timely recognition of, and provision for, credit losses promote safe and sound banking systems and
                                              play an important role in bank supervision. Since Basel I, the Basel Committee on Banking Supervision
                                               (BCBS) has recognised that there is a close relationship between capital and provisions. This is reflected in
                                               the regulatory treatment of accounting provisions under the Basel capital framework.
                                               In October 2016, the BCBS released for public comment a consultative document and a discussion
                                               paper on the policy considerations related to the regulatory treatment of accounting provisions under the
                                               Basel capital framework, in light of the shift to ECL by both the IASB and US Financial Accounting Standards
                                               Board. Given the diversity of accounting and supervisory policies in respect of provisioning and capital
                                               across jurisdictions, coupled with uncertainty about the capital effects of the change to an ECL accounting
                                               framework, the BCBS decided to retain - for an interim period - the current regulatory treatment of
                                               provisions as applied under both the standardised approach and internal ratings-based approaches. The
                                               BCBS will consider the longer-term regulatory capital treatment of provisions further, including
                                               undertaking analysis based on quantitative impact assessments.
                                               The BCBS has also set out optional transitional arrangements for the impact of ECL accounting
                                               on regulatory capital and the corresponding Pillar 3 disclosure requirements should individual jurisdictions
                                               choose to implement such transitional arrangements.
                                               This Executive Summary and related tutorials are also available in FSI Connect, the online learning tool of
                                               the Bank for International Settlements")
                                            ),                       
     
                                            tabPanel(id="user_manual","User Manual",tags$iframe(style="height:800px;width:200%;scrolling=yes",
                                                                                               src="pdf/intro.pdf"),width = "100%",height="800px"))),
       
    tabItem(tabName = "data_upload",
            box(fileInput("bank_raw_data","Upload Bank Raw Data",accept = c(".xlsx"))),
            # box(fileInput("collateral_file","Upload Collateral Data",accept = c(".xlsx"))),
            box(fileInput("haircut_file","Upload Haircut Data",accept = ".xlsx")),
            box(fileInput("pd_table","Upload TTC PD File",accept = c(".xlsx"))),
            # box(sliderInput("unsecured_lgd","Select Unsecured LGD %:",min = 0,max=100,post = "%",value=75)),
            # box(sliderInput("floor_lgd","Select Floored LGD %:",min = 0,max=100,post = "%",value=10)),
            actionButton("upload", "Upload Data",icon = icon("upload"))),
    
    tabItem(tabName = "userinputs",
            fluidRow(
              
              box(sliderInput("unsecured_lgd","Select Unsecured LGD :",min = 0,max=1,value=0.75),width = 6,solidHeader = TRUE),
              box(sliderInput("floor_lgd","Select Floored LGD :",min = 0,max=1,value=0.1),width = 6,status = "success",solidHeader = TRUE),
              box(numericInput(inputId = "stage2dpd",label = "Select Stage 2 DPD Thereshold",value = 30,min = 20,max = 45,step = 5),width = 6,solidHeader = TRUE),
              box(numericInput(inputId = "stage3dpd",label = "Select Stage 3 DPD Thereshold",value = 90,min = 90,max = 180,step = 30),width = 6,solidHeader = TRUE),
              box(title="Update haircut values",editableDTUI("hc"))
            )),
    
    
    tabItem(tabName = "calculate_stage",
            fluidRow(column(6,box(title ="Stagewise distribution of Facilities", actionButton("run_staging","Calculate Stage",icon = icon("refresh")),width = NULL,height = 500,plotOutput("plot1",height = 300))),
                     column(6,box(title = "Segment wise Staging",width=NULL,height=500,selectInput("Segment","Select Segment",list("Micro"="Micro","PF"="PF","Retail"="Retail",
                                                                                                                                   "Corporate"="Corporate")),plotOutput("plot2",height = 300))))),
    tabItem(tabName = "manual_override",
            fluidRow(
              div(style = 'overflow-x: scroll',editableDTUI("table1"))
              # verbatimTextOutput("test")
            )),
    tabItem(tabName = "LGD",
             fluidRow(dataTableOutput("lgd",width = NULL,height = "500px"),width = NULL)),
    tabItem(tabName = "calculate_ecl",
            actionButton("calc_ecl","Calculate ECL",icon = icon("calculator"),color="orange"),
            fluidRow( valueBoxOutput("value1"),valueBoxOutput("value2") ,valueBoxOutput("value3"),valueBoxOutput("value4")),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"),box(title="ECL by Stage",plotlyOutput("plot3",height = 300),height = 400,width=NULL),
             box(title = "ECL by Segment",plotlyOutput("plot4",height = 300),height = 400,width=NULL)))),
    
    tabItem(tabName = "report",
            fluidRow(dataTableOutput("ecl_report")),
            downloadButton("generate_report","Generate Report")),
    tabItem(tabName = "audittrail",
            textInput("facility","Enter Unique ID",placeholder = "Enter Facility ID"),
            actionButton("audit","Check ECL computation"),
            fluidRow(column(12,box(width=12,status = "primary",div(style='overflow-x: scroll',tableOutput("datainput")))),
                     column(12,box(width=4,textOutput("msg"))),
                     column(12,box(width=12, status = "primary",div(style='overflow-x: scroll', tableOutput("facility_ecl"))))))

  
)))
