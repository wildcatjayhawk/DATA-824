# Load R packages
library(shiny)
library(shinythemes)
library(RCurl)
library(randomForest)
library(VGAM)
library(mclust)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kernlab)

#Location of Github for this author of this application. 
#https://github.com/wildcatjayhawk/DATA-824.git

#Read Data
stock_dat <- read.csv(text = getURL("https://raw.githubusercontent.com/wildcatjayhawk/DATA-824/main/spy_all_data_6.7.csv"))

#Rename dataset for use in data exploration portion
dataset <- stock_dat

#Define model for use in prediction portion
model1 <- lm(OC_per_change ~ prev_OC_per_change + p_rel_vol_wk_3mo, data = stock_dat)

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
        navbarPage("Stock Price Movement Application", #Main Header
        
#########################################################################
#######       Background on Application Tab Section - Abstract etc  ##### 
#########################################################################                     
                   
        tabPanel("Background",
            
            mainPanel(
             h2("Abstract"),
            
                h4("Stock price movement can be chaotic and subject to unclear market signals 
                   and over reactions causing difficulty in short-term trading and investing, 
                   and is the primary reason most financial literature for individuals suggest 
                   only viewing the market in a long term perspective. However, hedge funds, 
                   portfolio managers, and day traders are able to realize large short-term 
                   financial gains regularly. It’s well known that professional traders use a 
                   combination of subjective assessment of the market via technical indicators 
                   and sentiment in addition to macro level influences that affect global markets. 
                   We attempt to show that a simple, objective, decisioning system underlies all 
                   of this and attempt to capture this in an ordinal cumulative logistic 
                   regression model."),
                   
                   h4("We were unable to capture a full model that was significant, 
                   but were able to discern important variables that contribute significantly 
                   more than others. We show that these variables have a strong inverse 
                   influence on market changes over a short term perspective. The significance of 
                   our research attempts help build a more comprehensive understanding of simple, 
                   but powerful effects that can act as primary decisioning mechanisms on their own, 
                   or in a less naive model as is popular through decision trees, machine learning, 
                   and other advanced model training procedures."),
                
            h2("Data Collection"),
                
                h4("Stock price data and technical analysis was collected from My Barchart 
                    Premier (barchart, 2021), and FlowAlgo.com (FlowAlgo LLC, 2021). We first reviewed 
                    the highest average volume stocks over the previous 52 weeks, from May 2019 to 
                    May 2020 for ETFs, Mega-Caps, Large-Caps, and Mid-Cap stocks and selected the top 
                    three from each market capitalization category. Then pulled all data for the 
                    respective Stock over the past 3 years, May 2018 to May 2021. Some of the technical
                    analysis metrics needed to be calculated manually as it was not available from
                    Barchart. Additionally all dark pool data was retrieved from FlowAlgo.com under 
                    the same time intervals for each stock, there was also some manual calculation 
                    for this data. Full explanations of the metrics can be found on the authors github,
                    in the original research."),
                
                h4("After all data was collected it became apparent that some of the stocks we selected 
                    had recent IPOs, and or were strong momentum stocks that contained very skewed data 
                    points where the stock value surged over a 1 week to 2 month period and then retraced
                    50-80%. In order to create a robust model we then narrowed our selection of potential 
                    stocks to the most highly traded overall by volume. Due to time constraints and time 
                    of analysis for each stock symbol we decided to only use the SPDR S&P 500 Trust ETF 
                    (ticker symbol: SPY) for our analysis. While we would like to use this same research 
                    approach on other stocks later, this is reasonable preliminary choice as this stock 
                    is an indexed ETF that references the S&P 500, one of the largest indexes in the world 
                    and a frequent point of comparison for minimum achievable yield for portfolio mangers, 
                    hedge funds, and signs of economic productivity. Therefore a model that could reasonably 
                    fit this data would potentially have application elsewhere in the market.")
            ), # mainpanel 
        ), #tabPanel Background 
        
#########################################################################
#######         AAbout Application - Description section tab        ##### 
#########################################################################        
        tabPanel("About Application",
                 
                 mainPanel(
                     h2("Purpose and Scope of Application"),
                     h4("The purpose of this application is to provide an extension of an exisiting project
                        attempting to model stock price performance. There are great deal of potential variables
                        in this project from our dataset. So the purpose of this is mutlifold."),
                     
                        h3("Purpose/Scope:"),
                        
                            h5("1) We aim to provide an interactive tool for easy visualization of variable interactions"),
                            h5("2) We aim to provide a quick refrence tool for distributions of different variables of interest"),
                            h5("3) We aim to provide a prediction/simulation tool for the model of intrest for backtesting"),
                     
                     h4("This application is limited in it's scope due to the data collected for model building, 
                        and is limited to a specific linear model with two input paramters and having both quantitative 
                        inputs and outputs. This is due to intrest in quantitative outcomes and inputs after finding a 
                        moderately weak and unclear cummulative logit odds model in previous testing and design. This model
                        is slight modification of the previous methods so we will not go into full detail 
                        regarding all methods."),
                     
                        h3("Expectations:"),
                            h5("This application is meant to help understand the dataset and relationship between parameters
                               for this large p dataset. The dimensions in this dataset are known to likely suffer multicolinearity
                               and covariance due to thier nature of derviation from only a few key dimensions. This application is 
                               not meant to be assumed to have a sound statistical model that would provide any sort of insight into
                               stock picking and market performance. All information gathered from this application is strictly for entertainment
                               purposes and should not be considered investment or trading advice whatsoever."),
                     
                            h5("The application will provide an opportunity to explore interdynamics between dimensions and look at 
                               alternative grouping and agglomerations that may be worth further insight. The current prediction model
                               is expected to have very little if any statistical use. "),
                     
                     
                     h5("*More info on the original study and design here: https://github.com/wildcatjayhawk/DATA-824.git")
                 ), # mainPanel
        ), # tabPanel About Application 
        
#########################################################################
#######         All Variables from Dataset 3-way explorer           ##### 
#########################################################################  
                   
        tabPanel("Stock Data Explorer", 
                h3("View and explore all the values of the dataset from our sample with drop-downs below."),    
                plotOutput('plot'),
                            
                hr(),
                            
                    fluidRow(
                        column(3,
                            h4("Stock Data Explorer"),
                            h4("Instructions:"),
                            h5("1) Select up to three variables of intrest in the drop downs below, by individually 
                               selecting a value for each drop down."),
                            h5("2) Next, select the sample size you desirve for the visulization."),
                            h5("3) Choose to Jitter the points for easier visualization, or/additonally, choose the Smooth
                               option to add a regression line to the visulization."),
                            sliderInput('sampleSize', 'Sample Size', 
                                        min=1, max=nrow(dataset),
                                        value=min(1, nrow(dataset)), 
                                        step=10, round=0),
                br(),
                                       checkboxInput('jitter', 'Jitter the Points'),
                                       checkboxInput('smooth', 'Smooth (overlayed lm/loess regression based on inputs)')
                        ), # column first
                    
                        column(4, offset = 1,
                                selectInput('x', 'X', names(dataset)),
                                selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                                selectInput('color', 'Color', c('None', names(dataset)))
                                ) # column mid
                    ), # fluidRow
            
                h5("*Note: Not all of these dimensions are used in the model in the following parts of the application")
        ), # tabPanel 
        
#########################################################################
#######         Univariate Histograms for Selected Vars             ##### 
#########################################################################  
               
        tabPanel("Explore Data",
                h3("Univariate Statistics for Explanatory and Output Variables of interest"),
                    h4("Instructions:"),
                        h5("1) Select the variable of intrest from the multiple button list."),
                        h5("2) Next, select the number of bins (columnour bars) for how you would like the 
                            histogram grouped."),
                h5("The Histograms are updated automatically upon changing either 1 or 2, and thier 
                    univariate frequency of observation can be seen on the visulizations."),
                 #Pick columns and groupings for histogram 
                 sidebarPanel(
                     radioButtons("z", "Select column of Stock Data Dataset:", 
                                  list("Previous Open-Close % Change "='a', 
                                       "Previous Relative Volume (Compared to 3mo daily avg volume)"='b',
                                       "Next Day Open-Close Change" = 'c')),
                     sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10)
                     
                 ), #sidebarPanel
                 
                 mainPanel(plotOutput("distPlot")), # mainPanel
                 
        ), #TabPanel

#########################################################################
#######         Least squares Model Prediction                      ##### 
#########################################################################                            
                
        tabPanel("Prediction Tool", # Navbar Tab Name
                #Input Values
                h3("Least Squares Regression Prediction tool for expected value of stock price % change based on selected input 
                    parameter"),
                
                h3("Background:"),
                h4("From previous research, we conducted a step-wise backwards regression analysis with possible interactions 
                    on more than 9 possible predictors. From this analysis, the most parsimonious model was found to only have two
                   predictors while having a large reduction in residual deviance. This model took the form of multinomial cummulative logit
                   with response of Open-Close % Change, and predictors of Previous Open-Close % Change, and Previous Relative Volume Signal."),
                   
                  h4("The model employed in this prediction application is nearly identical except that all response and predictor variables are quantitative,
                   as opposed to ordinal(categorical) types in the original model. It was suggested that perhaps we evaluate the same predictors by de-transforming
                   them from thier ordinal groupings back into numeric values and then test for model signficance."),
                  
                  h4("For this reason, the preiction application has been built on these same 3 dimensions, all in a quantitative variable class."),
                  
                    h4("Instructions:"),
                        h5("1) Move both of the sliders to the appropriate values based on observation of intrest"),
                        h5("2) Once both sliders have been positioned as desired, hit the Submit button"),
                h6("All Tool Output is considered to be for time (t + 1), and all tool input parmaters 
                    are considered to be for time (t). Example: If looking to predict stock price % change 
                    (outputs) for Thursday (time = t + 1), then previous relative volume, and % Open-Close 
                    change inputs would be from Wednesday (time = t)"),
                sidebarPanel(
                    HTML("<h3>Input parameters</h4>"),
                    
                    sliderInput(inputId = "prev_OC_per_change",
                                label = "Previous Open to Close % Change", 
                                value = 1, min = 100 * min(round(stock_dat$prev_OC_per_change,3)), 
                                max = 100 * max(round(stock_dat$prev_OC_per_change,3))),
                    
                    sliderInput(inputId = "p_rel_vol_wk_3mo",
                                label = "Previous Relative Volume Value",
                                value = 1, min = min(round(stock_dat$p_rel_vol_wk_3mo,3)), 
                                max = max(round(stock_dat$p_rel_vol_wk_3mo,3))),
                    
                    actionButton("submitbutton", "Submit", class ="btn btn-primary")
                ), # sidebarPanel
                
                mainPanel(
                    tabsetPanel(
                        tabPanel(  #Prediction results table
                            tags$label(h3('Status/Output')), # status/output Text box
                            verbatimTextOutput('contents'), 
                            h5("The expected price movement as a percentage for the stock is:"),
                            tableOutput('tabledata'),
                            h5("This prediction based on the values you gave, utilized this least squares regression formula."),
                            verbatimTextOutput('lmmodelformula')
                        ),
                        tabPanel(
                            tags$label(h3("Statistics")),
                            verbatimTextOutput('summstats'),
                            h5("The summary statistics for the least squares regression formula don't show profoundly confident results.
                               The individual beta parameters such as the intercept show us a decent p-value along with beta_1, however beta_2
                               does not appear significant in this model. The R-sq value is barely larger than zero indicating almost no correlation. 
                               Additionally, the residual deviance and AIC values as compared to our previous study are only slightly improved, despite
                               a significant F-Test. More exploration is needed to validate any assumptions produced by the linear regression model, but
                               it doesn't appear to improve our prediction outcomes much beyond random chance.")
                            
                        )
                    ) # tabsetPanel 
                ), # mainPanel
        ), # tabPanel Predict

        tabPanel("Visualizations of Linear Model Fitted",
            h3("Some visulizatons of the least squares regression model used in prediction tab."),
            mainPanel( # Linear model visualization panel
                tabsetPanel(
                    tabPanel(
                        tags$label(h4("Plot of Y ~ X1 + X2")),
                        plotOutput('lmggplot'),
                        h4("There is not a clear distinction in relationship between these variables.
                           Additonally, some increasing variance with increasing values of predictiors
                           appears present across multiple condtions here.")
                    ), # tabPanel first
                    
                    tabPanel(
                        tags$label(h4("QQ Plot of Residulas for Y_hat")),
                        plotOutput('qqlmplot'),
                        h4("The QQ Plot shows that the data does not satisfy uniformity of residuals as we would like to see.")
                    ) # tabPanel 2nd
                ) # tabsetPanel 
            ), # mainPanel
        ), # tabPanel
        
#########################################################################
#######         K Means Clustering                                  #####
#########################################################################

        tabPanel("K-means clustering example",
            h3("Two way K Means clustering for exploratory analysis"),
            h5("*Note: This clustering method within this application only works for some variables, namely
               all non-ordinal and/or categorical types. "),
            
            sidebarPanel(
                selectInput('dataset_k','Select a dataset:',c("stock_dat","iris")),
                uiOutput("varselect1"),
                uiOutput("varselect2"),
                numericInput('k', 'Number of clusters', value = 3,
                             min=1, step=1, max = 20),
                selectInput('kernel','Type of kernel:',c("linear","radial (RBF)"="RBF"))
                
            ), # sidebarPanel
            
            mainPanel(
                h2("Instructions:"),
                p("1) Select variables to be used for clustering."),
                p("2) Indicate the desired number of clusters."),
                p("3) And the type of kernel to be used."),
                h2("Result:"),
                plotOutput('plot_k')
                
            ), # mainPanel   
            
            h5("Any potenital K-means groupings that are clearly distinct from the related groups,
               should be noted. As they make be helpful in determing what main effects and interaction
               terms are telling of underlying relationships. This would be a jumping off point for further 
               exploration into a variety of different modeling techniques from linear regression, to ensemble methods, SVMs
               and unsupervised analysis methods.")
        
        # to do
        # - remove shinyUI which is no longer needed
        # - put the inputs in reactive() and check the values there
        # - make dataset reactive
        # http://stackoverflow.com/questions/26460585/r-shiny-selectinput-doesnt-reflect-the-variable-selection-when-multiple-true
        
        ) # tabPanel
        
    ) # navbarPage
) # fluidPage

##########################################################################################
########                                                                    ##############
########                            SERVER                                  ##############
########                                                                    ##############
##########################################################################################

# Define server function  
server <- function(input, output, session) {
    
    #Interactive Data Explorer
    
    dataset <- reactive({
        stock_dat[sample(nrow(stock_dat), input$sampleSize),]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point() + scale_color_gradient(low="blue", high="red")
        
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        
        if (input$jitter)
            p <- p + geom_jitter()
        if (input$smooth)
            p <- p + geom_smooth()
        
        print(p)
        
    })
    
    # Histograms plotted from first navbar
    output$distPlot <- renderPlot({     
        if(input$z=='a'){i<- c("prev_OC_per_change")}
        if(input$z=='b'){i<- c("p_rel_vol_wk_3mo")}
        if(input$z=='c'){i<- c("OC_per_change")}     
        #if(input$x=='d'){i<-4} 
        z <- stock_dat[,i]
        bins <- seq(min(z), max(z), length.out = input$bins + 1)
        hist(z, breaks = bins, col = 'blue', border = 'grey', xlab = paste("VALUES: ", i),
             main = paste("Histogram of", i))
    })
    
    #Input Data used in prediciton function from 2nd Nav bar
    datasetInput <- reactive({
        #This portion pulls in all the data from your web app that the user input
        
        df <- data.frame(
            Name = c("prev_OC_per_change",
                     "p_rel_vol_wk_3mo"),
            Value = as.character(c(input$prev_OC_per_change*.01,
                                   input$p_rel_vol_wk_3mo*.01)),
            stringsAsFactors = FALSE)
        
        input <- transpose(df)
        
        #input
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test_dat <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
        Output1 <- data.frame(Prediction=predict(model1,test_dat))
        Output1 <- paste(round(Output1[1]*100,2),"%")
        print(Output1)
        
    })
    
    #Status / output text box
    output$contents <- renderPrint({
        if(input$submitbutton > 0){
            isolate("Calculation Complete.")
        } else {
            return("Server is ready for calculation.")
        }
    })
    
    #Prediction results table - shows the actual value from pred formula from earlier
    output$tabledata <- renderTable({
        if (input$submitbutton > 0) {
            isolate(datasetInput())
        }
    })
    
    # Show the LM formula on the app
    output$lmmodelformula <- renderPrint({
        if (input$submitbutton > 0) {
            isolate(model1$call)
        }
    })
    
    # Shows the summary stats and information regarding LM Model
    output$summstats <- renderPrint({
        if (input$submitbutton > 0) {
            isolate(summary(model1))
        }
    })

    ###############################################################
    ###### For the ggplots of the Linear Model #####
    ###############################################################

    #Output from LM Model 
    output$lmggplot <- renderPlot({
        data_lm = stock_dat
        ggplot(data=data_lm, 
               aes(data_lm$prev_OC_per_change, 
                    data_lm$OC_per_change, 
                    fill = data_lm$p_rel_vol_signal)) +
            labs(x = "Prevous Open-Close % Change") +
            labs(y = "Next Open - Close % Change") +
            labs(fill = "Prevous Relative Volume Signal", title = "Scatterplot across 3 dimensions") +
            geom_point() +
            geom_smooth(method = lm)
    })  
    
    #QQPlot for LM model 
    output$qqlmplot <- renderPlot({
        data_lm = stock_dat
        model1_lm <- lm(OC_per_change ~ prev_OC_per_change + p_rel_vol_wk_3mo, data = data_lm)
        qqPlot(model1_lm, dist = "norm", data = data_lm)
    })
    
    
    ###############################################################
   ###### For the clustering portion of the application only #####
    ###############################################################
    
    output$varselect1 <- renderUI({
        selectInput("var1", label="Select first variable for clustering:",
                    choices=names(dataset_k()), selected=names(dataset_k())[1])  
    })
    output$varselect2 <- renderUI({
        selectInput("var2", label="Select second variable for clustering:",
                    choices=names(dataset_k()), selected=names(dataset_k())[2])  
    })
    
    dataset_k <- reactive({
      if (input$dataset_k=="stock_dat") {
            num_cols <- unlist(lapply(data, is.numeric))
           data(stock_dat <- stock_dat[, num_cols])
            stock_dat[,-38]
        } else if (input$dataset=="dat1") {
            read.table("data/self_test.data", skip=1, col.names = c("x","y")) 
            }
        else data.frame()
    })
    
    compute <- reactive({
        
        data_k   <- subset(dataset_k(), select=c(input$var1,input$var2))
        colnames(data_k) <- c("x","y")
        
        if(input$k>nrow(unique(data_k))) updateNumericInput(session,"k", value = nrow(unique(data_k)))
        if(input$k<1)                  updateNumericInput(session,"k", value = 1)
        
        if (input$kernel=="linear") {
            Kclust <- kmeans(data_k ,input$k)
            list(kmean.result = data.frame(data_k, cluster=as.factor(Kclust$cluster)),
                 centers = as.data.frame(Kclust$centers))
        } else if (input$kernel=="RBF") {
            Kclust <- kkmeans(as.matrix(data_k), input$k, kernel="rbfdot")
            list(kmean.result = data.frame(data_k, cluster=as.factor(Kclust@.Data)),
                 centers = data.frame(x=Kclust@centers[,1],
                                      y=Kclust@centers[,2]))
        }
    })
    
    output$plot_k <- renderPlot({
        data_k=compute()
        ggplot(data=data_k$kmean.result, aes(x=x, y=y, color=cluster)) +
            geom_point(size=3) + geom_point(data=data_k$centers,
                                            aes(x=x, y=y, color='Center'), pch=17, size=7) +
            ggtitle("Clustering result") + xlab(input$var1) + ylab(input$var2)
    })
    
    
    ###############################################################

    
}

############
# Create Shiny object - application 
############
shinyApp(ui = ui, server = server)

