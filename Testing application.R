# Load R packages
library(shiny)
library(shinythemes)
library(RCurl)
library(randomForest)

#https://github.com/wildcatjayhawk/DATA-824.git


#Read Data
stock_dat <- read.csv(text = getURL("https://raw.githubusercontent.com/wildcatjayhawk/DATA-824/main/spy_all_data_6.7.csv"))

model1 <- lm(OC_per_change ~ prev_OC_per_change + p_rel_vol_wk_3mo, data = stock_dat)

#stock_dat
# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                #Page Header
                headerPanel("Predict % Change of Stock for Tomorrow"),
                
                #Input Values
                sidebarPanel(
                    HTML("<h3>Input parameters</h4>"),
                    sliderInput(inputId = "prev_OC_per_change",
                                label = "Previous Open to Close % Change", 
                                value = 1, min = min(round(stock_dat$prev_OC_per_change,3)), 
                                max = max(round(stock_dat$prev_OC_per_change),3)),
                    
                    sliderInput(inputId = "p_rel_vol_wk_3mo",
                                label = "Previous Relative Volume Value",
                                value = 1, min = min(round(stock_dat$p_rel_vol_wk_3mo),3), 
                                max = max(round(stock_dat$p_rel_vol_wk_3mo,3))),
                    
                    actionButton("submitbutton", "Submit", class ="btn btn-primary")
                ),
                
                mainPanel(
                    tags$label(h3('Status/Output')), # status/output Text box
                    verbatimTextOutput('contents'), 
                    tableOutput('tabledata') # Prediction results table
                )
)
############
# SERVER 
############

# Define server function  
server <- function(input, output, session) {
    
    #Input Data
    datasetInput <- reactive({
        #This portion pulls in all the data from your web app that the user inputs
       
        df <- data.frame(
            Name = c("Date","Symbol", "Open", "High", "Low", "Close", "Volume",
                     "RSI", "ATR", "ATRMA", "VWAP", "wk_avg_vol_3mo", "rel_vol_wk_3mo", 
                     "p_rel_vol_wk_3mo", "c_rel_vol_signal", "p_rel_vol_signal", "dp_date",
                     "dp_count", "dp_volume", "dp_spot_avg", "dp_notional_value", "prev_dp_count",
                     "prev_dp_volume", "rel_low_VWAP_week", "ATR_15day_avg", "rel_ATR_15day_avg",
                     "prev_RSI_15day", "prev_rel_low_VWAP_week", "prev_rel_ATR_15day_avg", "HL_change",
                     "OC_Change", "HL_per_change", "OC_per_change", "prev_HL_per_change", 
                     "prev_OC_per_change", "Cc_Cp", "close_change_v_prev", "prev_OC_per_change"),
            Value = as.character(c(input$p_rel_vol_wk_3mo,
                                   input$prev_OC_per_change)),
            stringsAsFactors = FALSE)
        
        #Date <- 0
        #df <- rbind(df, Date)
        input <- transpose(df)
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
        
        Output <- data.frame(Prediction=predict(model1,test))
        print(Output)
    
    })
    
    #Status / output text box
    output$contents <- renderPrint({
        if(input$submitbutton > 0){
            isolate("Calculation Complete.")
        } else {
            return("Server is ready for calculation")
        }
    })
    
    #Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton > 0) {
            isolate(datasetInput())
        }
    })
    
}

############
# Create Shiny object - application 
############
shinyApp(ui = ui, server = server)
