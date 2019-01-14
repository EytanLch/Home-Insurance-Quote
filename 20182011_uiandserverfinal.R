#rm(list = ls())
library(shiny)
library(leaflet)
library(data.table)
library(shinyjs)
library(rpart)
library(shinydashboard)
library(ggvis)
library(ggplot2)
library(viridis)
library(stringr)
library(knitr)


load('dep')
load('zonier')
#district = readRDS('district.rds')
PostalCode= readRDS('PostalCode.rds')

part_cat_nat=1-0.89286
taxe_INC=0.3
taxe_RC=0.09
taxe_PJ=0.09
taux_HE_INC=0.03
taux_HE_TGN=0.2
taux_HE_DEB=0.01
taxe_ATT=4.3


ui <- fluidPage( style="padding-left: 0px;",style="padding-right: 0px;",
                 absolutePanel(htmlOutput("frame"),
                               
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                             draggable = TRUE, top = 80, left = 1080, right = 20, bottom = 60,
                                             width = 400, height = 300,tags$style(type='text/css', ".selectize-input { font-size: 10px; line-height: 10px;} .selectize-dropdown { font-size: 10px; line-height: 10px; } .selectsize-slider { font-size: 10px; line-height: 10px; }" ,"{margin-top: 0px;}"),
                                             #fluidRow(       
                                             #column(6,     
                                             #wellPanel(style = "font-size: 10px; padding: 10px 10px;margin-top:-2em",
                                             #  h3("Get your quote")
                                             # ,
                                             # sliderInput(
                                             #  "age",
                                             # "Age",
                                             #min = 18,
                                             #max = 80,
                                             #value = 50
                                             #)
                                             #,
                                             #selectInput("district", "District",district, multiple=F,selected='Central')
                                             #,
                                             #                                   selectInput("ZipCode", "Zip Code", PostalCode, multiple=F,selected='189969')
                                             #                                   ,
                                             # selectInput(
                                             #   "salutation",
                                             #   label = "Salutation",
                                             #   choices = list(
                                             #     "MR" = "Mister",
                                             #     "MS" = "Miss",
                                             #     "MRS" = "Mrs"
                                             #   ),
                                             #   selected = "MR"
                                             # )
                                             # ,
                                             #  selectInput(
                                             #    "Marital Status",
                                             #    label = "Marital Status",
                                             #    choices = list("Single", "Married", "Divorced","Widowed"),
                                             #    selected = "true"
                                             #  )
                                             #  ,
                                             #  selectInput(
                                             #    "Occupation",
                                             #    label = "Occupation",
                                             #    choices = list(
                                             #      "Artist",
                                             #      "Engineer",
                                             #      "Aircraft Pilot  / Cabin Crew" ,
                                             #      "Civil Servant",
                                             #      "Diplomat/Politician/Snr. Mgmt.",
                                             #      "Journalist",
                                             #      "Lawyer / Legal Profession",
                                             #      "Lecturer / Teacher / Tutor",
                                             #      "Medical/ Researcher/ Scientist",
                                             #      "National Serviceman",
                                             #      "Tradesman / Skilled Workers",
                                             #      "Outdoor Sales / Staff",
                                             #      "Housewife / Retiree / Student",
                                             #               "Dancer/Model/Musician/Sports",
                                             #               "Other"
                                             #             ),
                                             #             selected = "Engineer"
                                             #           )
                                             # ,
                                             #selectInput(
                                             # "Occupancy Status",
                                             #label = "Occupancy Status",
                                             #choices = list(
                                             # "Owner (Living In)",
                                             #"Landlord (Renting Out)",
                                             #"Tenant"),
                                             #selected = "Owner (Living In)"
                                             #)
                                             #)
                                             #)  
                                             #,
                                             wellPanel(
                                               fluidRow(
                                                 column(6,style = "font-size: 10px ; margin-top:-2em",
                                                        h3("Get Your Quote"),
                                                        sliderInput(
                                                          "HomeInsurancePlan",
                                                          "Home Insurance Plan (in Months)",
                                                          min = 12,
                                                          max = 36,
                                                          value = 24
                                                        ),
                                                        selectInput(
                                                          "Type_of_Habitation",
                                                          label = "Dwelling Type",
                                                          choices = list(
                                                            "HDB" = 1,
                                                            "Condos and Other Appartments"= 2,
                                                            "Landed Properties"= 3,
                                                            "Others"= 4
                                                          )
                                                        ),
                                                        conditionalPanel(condition="input.Type_of_Habitation != '3'",
                                                                         selectInput(
                                                                           "Number of Rooms",
                                                                           label="Number of Rooms", 
                                                                           choices = list(
                                                                             "1 Room",
                                                                             "2 Rooms",
                                                                             "3 Rooms",
                                                                             "4 Rooms",
                                                                             "5 Rooms",
                                                                             "Other")
                                                                         )
                                                        )
                                                 )
                                               ),
                                               style = "font-size: 10px; padding: 10px 10px;margin-top:-2em",
                                               actionButton("buttonExtract", "Get a Quote"),
                                               br(),
                                               br(),
                                               conditionalPanel(style = "font-size: 10px; padding: 0px 0px;margin-top:-2em",condition = ("input.buttonExtract!=0"),
                                                                fluidRow(
                                                                  column(6,
                                                                         h3('Your Quote'),
                                                                         uiOutput('price_Intern')
                                                                         
                                                                  )
                                                                )
                                               ),
                                               strong(em("This pricing is illustrative and based on a standard Home Insurance Pricing Model (excluding Renovations and Home contents).This data capture bot can be plugged to any Home Insurance Pricing Engine.",style = "color:black"))
                                             )
                               )
                 )
)


server <- function(input, output) {
  observe({ 
    map <<- paste0("http://localhost/Home_Insurance_Benchmark/")
  })
  output$frame <- renderUI({
    my_map <- tags$iframe(src=map,width=1560,height=723)
    print(my_map)
    my_map
  })
  output$price_Intern <- renderUI({
    if (input$buttonExtract == 0){
      return()
    }
    input$buttonExtract
    isolate(
      dat <- data.table(
        #input$age,
        #tranche_age = ifelse((as.numeric(input$age) >= 16 & as.numeric(input$age) <= 24),"16-24",ifelse((as.numeric(input$age) > 24 & as.numeric(input$age) <= 30),"25-30",ifelse((as.numeric(input$age) > 30 & as.numeric(input$age) <= 40),"31-40",ifelse((as.numeric(input$age) > 40 & as.numeric(input$age) <= 50),"41-50",ifelse((as.numeric(input$age) > 50 & as.numeric(input$age) <= 65),"51-65",ifelse(as.numeric(input$age)>65,"66+","NULL")))))), 
        #coef_age = as.numeric(ifelse((as.numeric(input$age) >= 16 & as.numeric(input$age) <= 24),100,
        #                            ifelse((as.numeric(input$age) > 24 & as.numeric(input$age) <= 30),100,
        #                                  ifelse((as.numeric(input$age) > 30 & as.numeric(input$age) <= 40),100,
        #                                        ifelse((as.numeric(input$age) > 40 & as.numeric(input$age) <= 50), 095 ,
        #                                              ifelse((as.numeric(input$age) > 50 & as.numeric(input$age) <= 65),090,
        #                                                    ifelse((as.numeric(input$age) > 65) ,090,"NULL")))))))/100,
        tranche_area = input$`Number of Rooms`,
        type_of_habitation = input$`Type_of_Habitation`,
        coef_area = as.numeric(ifelse(input$`Number of Rooms`=="1 Room",2040,
                                      ifelse(input$`Number of Rooms`=="2 Rooms",3020,
                                             ifelse(input$`Number of Rooms`=="3 Rooms",4060,
                                                    ifelse(input$`Number of Rooms`=="4 Rooms",6880,
                                                           ifelse(input$`Number of Rooms`=="5 Rooms",7500,
                                                                  ifelse(input$`Number of Rooms`=="Other",15000,"NULL")))))))/10000,
        #coef_zone = as.numeric(ifelse(input$district %in% c("Ang Mo Kio","Hougang","North-Eastern Islands","Punggol","Seletar","Sengkang","Serangoon"),90,
        #                              ifelse(input$district %in% c("Central Water Catchment","Lim Chu Kang","Mandai","Sembawang","Simpang","Sungei Kadut","Woodlands","Yishun"),95,
        #                                     ifelse(input$district %in% c("Boon Lay","Bukit Batok","Bukit Panjang","Choa Chu Kang","Clementi","Jurong East","Jurong West","Pioneer","Tengah","Tuas","Western Islands","Western Water Catchment"),98,
        #                                            ifelse(input$district %in% c("Bedok","Changi","Changi Bay","Pasir Ris","Paya Lebar","Tampines"),97,100))))) / 100,
        coef_type_property = as.numeric(ifelse(input$`Type_of_Habitation`=="1" & input$`Number of Rooms`=="1 Room",0.56,
                                               ifelse(input$`Type_of_Habitation`=="1" & input$`Number of Rooms`=="2 Rooms",0.8,
                                                      ifelse(input$`Type_of_Habitation`=="1" & input$`Number of Rooms`=="3 Rooms",0.89,
                                                             ifelse(input$`Type_of_Habitation`=="1" & input$`Number of Rooms`=="4 Rooms",1,
                                                                    ifelse(input$`Type_of_Habitation`=="1"& input$`Number of Rooms`=="5 Rooms",1.2,
                                                                           ifelse(input$`Type_of_Habitation`=="3",2.95,												 
                                                                                  ifelse(input$`Type_of_Habitation`=="2" & input$`Number of Rooms`=="1 Room",0.45,
                                                                                         ifelse(input$`Type_of_Habitation`=="2" & input$`Number of Rooms`=="2 Rooms",0.58,
                                                                                                ifelse(input$`Type_of_Habitation`=="2" & input$`Number of Rooms`=="3 Rooms",0.67,
                                                                                                       ifelse(input$`Type_of_Habitation`=="2" & input$`Number of Rooms`=="4 Rooms",1.45,
                                                                                                              ifelse(input$`Type_of_Habitation`=="2" & input$`Number of Rooms`=="5 Rooms",1.65,
                                                                                                                     ifelse(input$`Type_of_Habitation`=="4",0.96,1
                                                                                                                     )))))))))))))
      )
    )
    str1 <-
      paste0(
        "12 Months premium : <b>",format(
          as.numeric(zonier$`12 Months premium`[1])*dat$coef_type_property + 
            dat$coef_area*
            #(1-dat$coef_age)*
            #dat$coef_zone*
            taxe_INC* (1-part_cat_nat)* taxe_ATT
          , decimal.mark = ",",digits = 3,nsmall = 2
        )," S$</b>"
      )
    str2 <-
      paste0(
        "24 Months premium : <b>",format(
          as.numeric(zonier$`24 Months premium`[1])*dat$coef_type_property +
            dat$coef_area*
            #dat$coef_age*
            #dat$coef_zone*
            taxe_INC* (1-part_cat_nat)* taxe_ATT
          , decimal.mark = ",",digits = 3,nsmall = 2
        )," S$</b>"
      )
    str3 <-
      paste0(
        "36 Months premium : <b>",format(
          as.numeric(zonier$`36 Months premium`[1])*dat$coef_type_property + 
            dat$coef_area*
            #dat$coef_age*
            #dat$coef_zone*
            (1-part_cat_nat)*taxe_INC*taxe_ATT, decimal.mark = ",",digits = 3,nsmall = 2
        )," S$</b>"
      )
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
}

shinyApp(ui, server)