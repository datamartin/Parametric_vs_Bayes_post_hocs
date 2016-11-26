shinyUI(fluidPage(
        headerPanel("Parametric vs Bayes post hoc tests"),
        
        sidebarPanel(
                #Selector for file upload
                fileInput('datafile', 'Choose CSV file',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                selectInput("dataset", "Choose a dataset:", 
                            choices = c("Example data1", "Example data2","own")),
                
                selectInput("posthocs", "Choose a post hoc test:", 
                            choices = c("LSD", "HSD", "Duncan")),
                numericInput("pvalue", "Set p-value cut off:", min = 0, max = 1, value = 0.05, step = 0.01),
                numericInput("BF", "Set BayesFactor cut off:", min = 0, max = 10, value = 0, step = 0.1),
                #These column selectors are dynamically created when the file is loaded
                uiOutput("Factor1"),
                uiOutput("Factor2"),
                uiOutput("Outcome"),
                #actionButton("inter", "Show interaction structure"),
                actionButton("f1results", "Show Factor 1 results"),
                actionButton("f2results", "Show Factor 2 results"),
                actionButton("intresults", "Show interaction results"),
                actionButton("reset1", "Reset")
                
        ),
        mainPanel(
                tabsetPanel(tabPanel("Intro", 
                                     p("The aim of the app is to give students and researchers a tool to balance experimental groups before starting actual experiment. Prior to an experiment researchers can use the app to balance groups both in terms of measured variables and possible confounding covariates. This procedure helps to minimize the effect of confounding variables and misinterpretation of the results. The app user should be able to load his dataset, specify the experimental design and variables to be balanced between experimental groups. The app runs Bayesian analogues to t-test and ANOVA’s."),
                                     textOutput("introtext")),
                            tabPanel("Usage", 
                                     p("User can upload his/her own the data;"),
                                     p("User can specify the design of his/her experiment – maximum number of experimental groups is 8 (three factor design with 2 levels for each of the experimental groups);"),
                                     p("User can either provide the assignment of subjects between experimental groups or let the app assign the subjects between experimental conditions at random (we expect balanced design in terms of sizes of experimental groups);"),
                                     p("User can specify the significance level for parametric analysis and Bayes Factor value for Bayesian hypothesis testing. Those cut-off values are used to indicate the significant differences on figures comparing different experimental groups."),
                                     p("The app presents the comparisons between experimental groups graphically as shown on example figure below. Two sets of figures are provided: firstly, for parametric analysis and secondly for Bayesian analysis. This helps the user to gain intuition on how different cut-off values for hypothesis testing behave/compare between parametric and Bayesian analysis. Especially so in case if the data that is non-normally distributed or has data points with high leverage or influence values."),
                                     textOutput("introtext2")),
                            tabPanel("Example data intro",
                                     p("coming soon..."))
                ),
                verbatimTextOutput("summary"),
                tableOutput("interaction"),
                tabsetPanel(
                        tabPanel("Post Hoc resutls F1",textOutput("phtestf1"),tableOutput("resultsf1out"),plotOutput("plotf1out")),
                        tabPanel("Post Hoc plot F2",textOutput("phtestf2"),tableOutput("resultsf2out"),plotOutput("plotf2out")),
                        tabPanel("Post Hoc plot interaction",textOutput("phtestint"),tableOutput("intresultsout"),plotOutput("plotinterout"))
                )
        )
))


# "" })

# 
# output$introtext2 <- renderText({"User can upload his/her own the data;
# User can specify the design of his/her experiment – maximum number of experimental groups is 8 (three factor design with 2 levels for each of the experimental groups);
# User can either provide the assignment of subjects between experimental groups or let the app assign the subjects between experimental conditions at random (we expect balanced design in terms of sizes of experimental groups);
# User can specify the significance level for parametric analysis and Bayes Factor value for Bayesian hypothesis testing. Those cut-off values are used to indicate the significant differences on figures comparing different experimental groups.
# The app presents the comparisons between experimental groups graphically as shown on example figure below. Two sets of figures are provided: firstly, for parametric analysis and secondly for Bayesian analysis. This helps the user to gain intuition on how different cut-off values for hypothesis testing behave/compare between parametric and Bayesian analysis. Especially so in case if the data that is non-normally distributed or has data points with high leverage or influence values."})
