#budget v4, now with ability to enter price values
library(shiny)
library(tidyr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(readxl)
library(stargazer)
prices<-read_excel("data/prices.xlsx")#can use this to replace manual input below
pesticide<-read_excel("data/pesticides.xlsx")
#can change to reading in original crop budget values to update annually
prices<-prices%>%
  select(-UNIT)%>%
  pivot_wider(names_from=ITEM,values_from=PRICE)  

ui <- fluidPage(
  
  # App title ----
  titlePanel(includeMarkdown("header.md")),
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Analysis by bushel",tableOutput("table1")),
              tabPanel("Summary table", tableOutput("table2"))),
  hr(),
  fluidRow(
    
    column(3,
           h4('Farm Specific Inputs'),
           textInput("field","Field Name"),
           sliderInput("yield", "Yield goal (bu/ac):",
                       min = 0, max = 100,
                       value = 50),
           sliderInput("price", "Price goal ($/bu):",
                       min = 0, max = 20,
                       value = 9.17,step=0.01),
           helpText("Enter a value of 1 acre to view",
                    "results on per acre basis"),       
           sliderInput("acres", "Total acres farmed:",
                       min = 0, max = 1000,
                       value = 500),
           downloadButton("report", "Generate report")),
    column(3,
           h4('Variable inputs'),
           helpText("Enter soybean variety information"),
           textInput("variety","Soybean variety"),
           selectInput("soybean_variety","Herbicide tolerance",c("RoundUp Ready","Liberty Link","Liberty Link GT27",
                                                                 "Xtend","XtendFlex","Enlist E3","Conventional"),selected="RoundUp Ready"),
           uiOutput("seed.ui"),
           checkboxInput("resistance","Check here if you have herbicide resistant weeds",
                         value=FALSE),
           numericInput("SOYBEAN", "Seeding rate (x1000 seeds/ac):", 150),
           numericInput("SOIL_TEST","Soil testing ($/ac)",0.30),
           numericInput("PHOSPHORUS", "Phosphorus (lbs/ac):", 45),
           numericInput("P_cost","Cost ($/lb)",prices$PHOSPHORUS),
           numericInput("POTASH", "Potassium (lbs/ac):", 40),
           numericInput("K_cost","Cost ($/lb)",prices$POTASH),
           numericInput("LIME", "Lime (tons/ac):", 0.5),
           numericInput("Lime_cost","Cost ($/ton)",prices$LIME),
           helpText("Specify interest rate on operating capital"),
           numericInput("INTEREST","Interest rate (%)",8.5),
           helpText("Enter crop insurance rate"),
           numericInput("INSURANCE", "Crop insurance ($/ac)",prices$`CROP INSURANCE - Soybeans`)),
    column(3,
           h4('Pest Management'),
           helpText("Preplant burndown herbicides:"), 
           selectInput("herb1","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                       selected="2,4-D amine"),
           uiOutput("rate1"),
           uiOutput("cost1"),
           selectInput("herb2","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                       selected="Gramoxone"),
           uiOutput("rate2"),
           uiOutput("cost2"),
           selectInput("herb3","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                       selected="none"),
           uiOutput("rate3"),
           uiOutput("cost3"),
           helpText("PRE/residual herbicides at planting:"),
           selectInput("herb4","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                       selected="Dual II Magnum"),
           uiOutput("rate4"),
           uiOutput("cost4"),
           selectInput("herb5","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                       selected="Sharpen"),
           uiOutput("rate5"),
           uiOutput("cost5"),
           selectInput("herb6","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                       selected="none"),
           uiOutput("rate6"),
           uiOutput("cost6"),
           helpText("Postemergence herbicides"),
           uiOutput("herb7"),
           uiOutput("rate7"),
           uiOutput("cost7"),
           uiOutput("herb8"),
           uiOutput("rate8"),
           uiOutput("cost8"),
           uiOutput("herb9"),
           uiOutput("rate9"),
           uiOutput("cost9"),
           helpText("Insecticide"),
           selectInput("insect1","Insecticide",pesticide$name[pesticide$insecticide==1],
                       selected="Lamba Cy/Taiga/Warrior"),
           uiOutput("rate_ins1"),
           uiOutput("cost_ins1"),
           selectInput("insect2","Insecticide",pesticide$name[pesticide$insecticide==1],
                       selected="none"),
           uiOutput("rate_ins2"),
           uiOutput("cost_ins2"),
           selectInput("other","Other",pesticide$name[pesticide$other==1],
                       selected="none"),
           uiOutput("rate_other"),
           uiOutput("cost_other")),
    column(3,
           h4('Fixed inputs'),
           helpText("Enter fixed/overhead costs",
                    "custom rates are used as proxy",
                    "for field operation costs"),
           numericInput("FERT_APP","Fertilizer Application ($/ac)",prices$`FERTILIZER SPREADING`),
           numericInput("PLANTING","Planting - No-Till ($/ac)",prices$`SOYBEAN - NoTill`),
           numericInput("PESTICIDE SPRAYING","Pesticide Application ($/ac)",10.23),
           numericInput("PEST_APP","Number of Pesticide Applications",3),
           numericInput("HARVEST","Harvesting ($/ac)",34.94),#amount listed as broadcast seeding on spreadsheet
           numericInput("HAUL","Hauling ($/ac)",prices$HAULING*50),#this one is per 50 bu/per acre, might need to adjust
           helpText("Specify loan interest rate on",
                    "spring custom charges"),
           numericInput("SPRING_INT","Interest rate (%)",8.5),
           helpText("Enter any land rental charge"),
           numericInput("LAND_CHARGE","Land Charge ($/ac)",98)
    )
  )
)


server <- function(input, output) {
  #dynamic user interface for average seed cost by herbicide tolerance traits
  output$seed.ui<-renderUI({
    switch(input$soybean_variety,
           "RoundUp Ready" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN RR II'*140),
           "Liberty Link" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN LL'*140),
           "Xtend" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN XTEND'*140),
           "XtendFlex" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN XTENDFLEX'*140),
           "Enlist E3" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN ENLIST'*140),
           "Liberty Link GT27" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN LLGT27'*140),
           "Conventional" = numericInput("seed_cost","Seed cost ($/unit)",prices$'SOYBEAN CONV'*140))
  })
  
  #dynamic user interface for burndown herbicides
  output$rate1<-renderUI({numericInput("rate1",paste("Rate (",pesticide$unit[pesticide$name==input$herb1],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb1])
    
  })
  output$cost1<-renderUI({numericInput("cost1",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb1],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb1])
    
  })
  output$rate2<-renderUI({numericInput("rate2",paste("Rate (",pesticide$unit[pesticide$name==input$herb2],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb2])
    
  })
  output$cost2<-renderUI({numericInput("cost2",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb2],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb2])
    
  })
 
  output$rate3<-renderUI({numericInput("rate3",paste("Rate (",pesticide$unit[pesticide$name==input$herb3],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb3])
    
  })
  output$cost3<-renderUI({numericInput("cost3",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb3],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb3])
    
  })
  #dynamic user interface for PRE herbicides
  output$rate4<-renderUI({numericInput("rate4",paste("Rate (",pesticide$unit[pesticide$name==input$herb4],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb4])
    
  })
  output$cost4<-renderUI({numericInput("cost4",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb4],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb4])
    
  })
  output$rate5<-renderUI({numericInput("rate5",paste("Rate (",pesticide$unit[pesticide$name==input$herb5],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb5])
    
  })
  output$cost5<-renderUI({numericInput("cost5",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb5],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb5])
    
  })
  output$rate6<-renderUI({numericInput("rate6",paste("Rate (",pesticide$unit[pesticide$name==input$herb6],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb6])
    
  })
  output$cost6<-renderUI({numericInput("cost6",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb6],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb6])
    
  })
  
  #dynamic user interface for POST products specific to tolerance traits
  output$herb7<-renderUI({switch(input$soybean_variety,
    "RoundUp Ready"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$roundup_ready==1],
              selected="RoundUp"),
    "Liberty Link"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$liberty_link==1],
                               selected="Liberty"),
    "Liberty Link GT27"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$liberty_link_GT27==1],
                               selected="Liberty"),
    "Xtend"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$xtend==1],
                        selected="Xtendimax"),
    "XtendFlex"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$xtendflex==1],
                        selected="Xtendimax"),
    "Enlist E3"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$enlist==1],
                        selected="Enlist One"),
    "Conventional"=selectInput("herb7","Post-emergence herbicide",pesticide$name[pesticide$conventional==1],
                        selected="Poast")
  )
    })
  #repeat this for other two postemergent herbicides, but keep as "none" also add other soy herb tolerance traits
  #"RoundUp Ready","Liberty Link","Liberty Link GT27","Xtend","Xtendflex","Enlist E3","Conventional"
  output$rate7<-renderUI({numericInput("rate7",paste("Rate (",pesticide$unit[pesticide$name==input$herb7],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb7])
    
  })
  output$cost7<-renderUI({numericInput("cost7",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb7],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb7])
    
  })
  output$herb8<-renderUI({switch(input$soybean_variety,
                                 "RoundUp Ready"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$roundup_ready==1],
                                                             selected="none"),
                                 "Liberty Link"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$liberty_link==1],
                                                            selected="none"),
                                 "Liberty Link GT27"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$liberty_link_GT27==1],
                                                                 selected="none"),
                                 "Xtend"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$xtend==1],
                                                     selected="none"),
                                 "XtendFlex"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$xtendflex==1],
                                                         selected="none"),
                                 "Enlist E3"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$enlist==1],
                                                      selected="none"),
                                 "Conventional"=selectInput("herb8","Post-emergence herbicide",pesticide$name[pesticide$conventional==1],
                                                            selected="none")
  )
  })
  #repeat this for other two postemergent herbicides, but keep as "none" also add other soy herb tolerance traits
  #"RoundUp Ready","Liberty Link","Liberty Link GT27","Xtend","Xtendflex","Enlist E3","Conventional"
  output$rate8<-renderUI({numericInput("rate8",paste("Rate (",pesticide$unit[pesticide$name==input$herb8],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb8])
    
  })
  output$cost8<-renderUI({numericInput("cost8",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb8],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb8])
    
  })
  output$herb9<-renderUI({switch(input$soybean_variety,
                                 "RoundUp Ready"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$roundup_ready==1],
                                                             selected="none"),
                                 "Liberty Link"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$liberty_link==1],
                                                            selected="none"),
                                 "Liberty Link GT27"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$liberty_link_GT27==1],
                                                                 selected="none"),
                                 "Xtend"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$xtend==1],
                                                     selected="none"),
                                 "XtendFlex"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$xtendflex==1],
                                                         selected="none"),
                                 "Enlist E3"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$enlist==1],
                                                      selected="none"),
                                 "Conventional"=selectInput("herb9","Post-emergence herbicide",pesticide$name[pesticide$conventional==1],
                                                            selected="none")
  )
  })
  #repeat this for other two postemergent herbicides, but keep as "none" also add other soy herb tolerance traits
  #"RoundUp Ready","Liberty Link","Liberty Link GT27","Xtend","Xtendflex","Enlist E3","Conventional"
  output$rate9<-renderUI({numericInput("rate9",paste("Rate (",pesticide$unit[pesticide$name==input$herb9],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb9])
    
  })
  output$cost9<-renderUI({numericInput("cost9",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb9],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb9])
    
  })  
  
  #Dynamic user inferface output for insecticide and "other" category
  output$rate_ins1<-renderUI({numericInput("rate_ins1",paste("Rate (",pesticide$unit[pesticide$name==input$insect1],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$insect1])
    
  })
  output$cost_ins1<-renderUI({numericInput("cost_ins1",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$insect1],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$insect1])
    
    
  })
  output$rate_ins2<-renderUI({numericInput("rate_ins2",paste("Rate (",pesticide$unit[pesticide$name==input$insect2],
                                                             ")",sep=""),pesticide$rate[pesticide$name==input$insect2])
    
  })
  output$cost_ins2<-renderUI({numericInput("cost_ins2",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$insect2],
                                                             ")",sep=""),pesticide$price_bulk[pesticide$name==input$insect2])
    
  })
  output$rate_other<-renderUI({numericInput("rate_other",paste("Rate (",pesticide$unit[pesticide$name==input$other],
                                                             ")",sep=""),pesticide$rate[pesticide$name==input$other])
    
  })
  output$cost_other<-renderUI({numericInput("cost_other",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$other],
                                                             ")",sep=""),pesticide$price_bulk[pesticide$name==input$other])
  })
  
  #need to create reactive values that take into account price with user-defined rates and prices
  HERB1<-reactive({input$cost1/pesticide$convert[pesticide$name==input$herb1]*input$rate1
  })
  HERB2<-reactive({input$cost2/pesticide$convert[pesticide$name==input$herb2]*input$rate2
  })
  HERB3<-reactive({input$cost3/pesticide$convert[pesticide$name==input$herb3]*input$rate3
  })
  HERB4<-reactive({input$cost4/pesticide$convert[pesticide$name==input$herb4]*input$rate4
  })
  HERB5<-reactive({input$cost5/pesticide$convert[pesticide$name==input$herb5]*input$rate5
  })
  HERB6<-reactive({input$cost6/pesticide$convert[pesticide$name==input$herb6]*input$rate6
  })
  HERB7<-reactive({input$cost7/pesticide$convert[pesticide$name==input$herb7]*input$rate7
  })
  HERB8<-reactive({input$cost8/pesticide$convert[pesticide$name==input$herb8]*input$rate8
  })
  HERB9<-reactive({input$cost9/pesticide$convert[pesticide$name==input$herb9]*input$rate9
  })
  INS1<-reactive({input$cost_ins1/pesticide$convert[pesticide$name==input$insect1]*input$rate_ins1
  })
  INS2<-reactive({input$cost_ins2/pesticide$convert[pesticide$name==input$insect2]*input$rate_ins2
  })
  OTHER<-reactive({input$cost_other/pesticide$convert[pesticide$name==input$other]*input$rate_other
  })
  
  
    #tabulates all inputs that are then charged interest 
  inputs<-reactive({
    (input$SOIL_TEST+input$SOYBEAN*(input$seed_cost/140)+input$PHOSPHORUS*input$P_cost+
       input$POTASH*input$K_cost+input$LIME*input$Lime_cost+HERB1()+HERB2()+HERB3()+HERB4()+
       HERB5()+HERB6()+HERB7()+HERB8()+HERB9()+INS1()+INS2()+OTHER())*
      input$acres
  })
  #calculates variable inputs as inputs plus finance charge
  variable<-reactive({
    inputs()+inputs()*0.5*(input$INTEREST/100)+(input$INSURANCE*input$acres)
  })
  
  #creates dataframe of variable inputs to pass to RMarkdown
  var_input<-reactive({
    a<-data.frame(
      Input=c("Seed cost","Soil testing","Phosphorus","Potassium","Lime","Interest rate",
              "Crop insurance"),
      Rate=c(input$SOYBEAN*1000,input$SOIL_TEST,input$PHOSPHORUS,input$POTASH,input$LIME,input$INTEREST,
             input$INSURANCE),
      #Unit=c("seeds/ac","dollar/ac","lbs/ac","lbs/ac","tons/ac","%","dollar/ac"),
      Cost=round(c(input$SOYBEAN*(input$seed_cost/140),input$SOIL_TEST,input$PHOSPHORUS*input$P_cost,
                     input$POTASH*input$K_cost,input$LIME*input$Lime_cost,
                   inputs()*0.5*(input$INTEREST/100)/input$acres,input$INSURANCE),digits=2))
    a$Rate[1] <- round(a$Rate[1], digits = 0)
    a$Rate[2:7] <- round(a$Rate[2:7], digits = 2)
    a$Rate<-as.character(a$Rate)
    a
  })
  #dataframe of pesticide inputs to pass to RMarkdown 
  pest_input<-reactive({
    a<-tibble(
      Input=c(input$herb1,input$herb2,input$herb3,input$herb4,input$herb5,input$herb6,input$herb7,
              input$herb8,input$herb9,input$insect1,input$insect2,input$other),
      Group=c(pesticide$group[pesticide$name==input$herb1],pesticide$group[pesticide$name==input$herb2],
              pesticide$group[pesticide$name==input$herb3],pesticide$group[pesticide$name==input$herb4],
              pesticide$group[pesticide$name==input$herb5],pesticide$group[pesticide$name==input$herb6],
              pesticide$group[pesticide$name==input$herb7],pesticide$group[pesticide$name==input$herb8],
              pesticide$group[pesticide$name==input$herb9],pesticide$group[pesticide$name==input$insect1],
              pesticide$group[pesticide$name==input$insect2],pesticide$group[pesticide$name==input$other]),
      Rate=c(input$rate1,input$rate2,input$rate3,input$rate4,input$rate5,input$rate6,input$rate7,
             input$rate8,input$rate9,input$rate_ins1,input$rate_ins1,input$rate_other),
      Unit=c(pesticide$unit[pesticide$name==input$herb1],pesticide$unit[pesticide$name==input$herb2],
             pesticide$unit[pesticide$name==input$herb3],pesticide$unit[pesticide$name==input$herb4],
             pesticide$unit[pesticide$name==input$herb5],pesticide$unit[pesticide$name==input$herb6],
             pesticide$unit[pesticide$name==input$herb7],pesticide$unit[pesticide$name==input$herb8],
             pesticide$unit[pesticide$name==input$herb9],pesticide$unit[pesticide$name==input$insect1],
             pesticide$unit[pesticide$name==input$insect2],pesticide$unit[pesticide$name==input$other]),
      Cost=round(c(HERB1(),HERB2(),HERB3(),HERB4(),HERB5(),HERB6(),HERB7(),HERB8(),HERB9(),INS1(),INS2(),OTHER())
      ,digits=2))
    a<-filter(a,Input!="none")
    a
  })
  #recommendation for managing herbicide resistant weeds if box is checked (passed to markdown)
  resistance<-reactive({if(input$resistance==TRUE){"**Any fields with herbicide resistant
    weeds should reconsider post application products"}
    else{NULL}
  })
  #calculates spring planting finance charge 
  spring<-reactive({
    (input$FERT_APP+input$PLANTING+input$PEST_APP*input$'PESTICIDE SPRAYING')*input$acres
  })
  #reactive variable of fixed costs 
  fixed<-reactive({
    spring()+spring()*0.5*(input$SPRING_INT/100)+((input$HARVEST+input$HAUL+input$LAND_CHARGE)*input$acres)
  })
  
  #dataframe of fixed inputs to be passed to RMarkdown 
  fix_input<-reactive({
    a<-data.frame(
      Input=c("Fertilizer application","Planting","Pesticide applications","Harvesting","Hauling",
              "Interest rate","Land charge"),
      Rate=c(input$FERT_APP,input$PLANTING,input$'PESTICIDE SPRAYING',input$HARVEST,input$HAUL,input$SPRING_INT,
             input$LAND_CHARGE
      ),
      #Unit=c("dollar/ac","dollar/ac","dollar/ac","dollar/ac","dollar/ac","%","dollar/ac"),
      Cost=round(c(input$FERT_APP,input$PLANTING,input$PEST_APP*input$'PESTICIDE SPRAYING',input$HARVEST,input$HAUL,
                   spring()*0.5*(input$SPRING_INT/100)/input$acres,input$LAND_CHARGE),digits=2)
    )
    a
  })
  #reactive variables that produce sensitivity analysis 
  x<-reactive({
    seq(input$yield*.75,input$yield*1.25,1)
  })
  y<-reactive({
    x()*input$acres*input$price-variable()-fixed()
  })
  #reactive dataframe that produces table of inputs per bushel 
  per.unit<-reactive({
    data.frame(
      Variable=c("Breakeven","Variable costs per unit","Overhead cost per unit","Total cost per unit",
                 "Profit per unit"),
      Value=round(c((variable()+fixed())/(input$yield*input$acres),
                    variable()/(input$yield*input$acres),
                    fixed()/(input$yield*input$acres),
                    (variable()+fixed())/(input$yield*input$acres),
                    input$price-(variable()+fixed())/(input$yield*input$acres)),digits=2)
    )
  })
  #reactive dataframe creating table for sensitivity analysis 
  sens<-reactive({
    
    a<-data.frame(
      yields=c(0.75*input$yield,input$yield,1.25*input$yield),
      low.price=round(c(input$price*0.88*input$acres*input$yield*0.75-variable()-fixed(),
                        input$price*0.88*input$acres*input$yield-variable()-fixed(),
                        input$price*0.88*input$acres*input$yield*1.25-variable()-fixed()),digits=2),
      avg.price=round(c(min(y()),mean(y()),max(y())),digits=2),
      high.price=round(c(input$price*1.12*input$acres*input$yield*0.75-variable()-fixed(),
                         input$price*1.12*input$acres*input$yield-variable()-fixed(),
                         input$price*1.12*input$acres*input$yield*1.25-variable()-fixed()),digits=2)
    )
    colnames(a)<-c("yield (bu/ac)",
                   paste("price: $",round(input$price*0.88,digits=2),sep=""),
                   paste("price: $",input$price,sep=""),
                   paste("price: $",round(input$price*1.12,digits=2),sep=""))
    a
  })

  output$table1<-renderTable({
    per.unit()
  })
  
  output$table2<- renderTable({
    sens()
    
  })
  
  
  
  output$report <- downloadHandler(
    
    filename = "Budget_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "budget report.Rmd")
      file.copy("budget report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(field=input$field,
                     variety=input$variety,
                     yield=input$yield,
                     summary=sens(),
                     acres=input$acres,
                     price=input$price,
                     tolerance=input$soybean_variety,
                     inputs=per.unit(),
                     variable=var_input(),
                     fixed=fix_input(),
                     pest=pest_input(),
                     resist=resistance())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,output_file=file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)


