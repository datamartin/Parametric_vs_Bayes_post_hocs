library(shiny)
library(datasets)
library(ggplot2)
library(car)
library(agricolae)
library(BayesFactor)
library(cowplot)
library(ggthemes)
library(stringr)
library(shinyjs)
library(psych)#for datasets
source('C:/Users/Martin/ownCloud/Projects/R_APP16/app/Parametric_vs_Bayes_post_hocs/Demo2HDI/worker_functionsAppHDI.R', local = TRUE)

shinyServer(function(input, output, session) {

        # Return the requested dataset
        datasetInput <- reactive({
                switch(input$dataset,
                       "Example data1" = sat.act,
                       "Example data2" = affect,
                       "own" = )
        })
        
        # observe({
        #         input$reset1
        #         print("resetting")
        #         output$phtestf1 <- renderText({NULL})
        #         output$resultsf1out <- renderTable({NULL})
        #         output$plotf1out <- renderPlot({NULL})
        #         
        #         output$phtestf2 <- renderText({NULL})
        #         output$resultsf2out <- renderTable({NULL})
        #         output$plotf2out <- renderPlot({NULL})
        #         
        #         output$intresults <- renderText({NULL})
        #         output$intresultsout <- renderTable({NULL})
        #         output$plotinterout <- renderPlot({NULL})
        # })
        # Return the post hoc request
        postHoctype <- reactive({
                switch(input$posthocs,
                       "LSD" = "LSD.test",
                       "HSD" = "HSD.test",
                       "Duncan" = "duncan.test"
                       )
        })
        filedata <- reactive({
                infile <- input$datafile
                if (is.null(infile)) {
                        # User has not uploaded a file yet
                        return(NULL)
                }
                read.csv(infile$datapath)[1:63,]
        })
        
        #The following set of functions populate the FACTOR1 selectors
        output$Factor1 <- renderUI({
                # df <-filedata()
                # if (is.null(df)) return(NULL)
                
                if (input$dataset == "own"){
                        df <-filedata()
                        items=names(df)
                        names(items)=items
                        selectInput("Factor1", "Factor1:",items)
                }
                else{
                        df <-datasetInput()
                        items=names(df)
                        names(items)=items
                        selectInput("Factor1", "Factor1:",items)
                }
        })
        #The following set of functions populate the FACTOR2 selectors
        output$Factor2 <- renderUI({
                # df <-filedata()
                # if (is.null(df)) return(NULL)
                # 
                # items=names(df)
                # names(items)=items
                # selectInput("Factor2", "Factor2:",items)
                
                if (input$dataset == "own"){
                        df <-filedata()
                        items=names(df)
                        names(items)=items
                        selectInput("Factor2", "Factor2:", items)
                }
                else{
                        df <-datasetInput()
                        items=names(df)
                        names(items)=items
                        selectInput("Factor2", "Factor2:", items)
                }
        })
        
        #The following set of functions populate the OUTCOME selectors
        output$Outcome <- renderUI({
                # df <-filedata()
                # if (is.null(df)) return(NULL)
                # 
                # items=names(df)
                # names(items)=items
                # selectInput("Outcome", "Outcome:",items)
                
                if (input$dataset == "own"){
                        df <-filedata()
                        items=names(df)
                        names(items)=items
                        selectInput("Outcome", "Outcome:",items)
                }
                else{
                        df <-datasetInput()
                        items=names(df)
                        names(items)=items
                        selectInput("Outcome", "Outcome:",items)
                }
        })
        a <- reactive({
                if (input$dataset == "own"){
                        df <-filedata()
                        f1 <- as.character(input$Factor1)
                        f2 <- as.character(input$Factor2)
                        var <- as.character(input$Outcome)
                        df[,f1] <- as.character(df[,f1])
                        df[,f2] <- as.character(df[,f2])
                        df[,var] <- as.numeric(df[,var])
                        a <- interaction.maker(df = df ,factor1 = f1, factor2 = f2, value = var, credvalue = input$CM)
                        return(a)
                }else{
                        df <-datasetInput()
                        f1 <- as.character(input$Factor1)
                        f2 <- as.character(input$Factor2)
                        var <- as.character(input$Outcome)
                        df[,f1] <- as.character(df[,f1])
                        df[,f2] <- as.character(df[,f2])
                        df[,var] <- as.numeric(df[,var])
                        a <- interaction.maker(df = df ,factor1 = f1, factor2 = f2, value = var, credvalue = input$CM)
                        return(a)
                }

        })
        aa <- reactive({
                if (input$dataset == "own"){
                        dfb <-filedata()
                        f1b <- as.character(input$Factor1)
                        f2b <- as.character(input$Factor2)
                        varb <- as.character(input$Outcome)
                        dfb[,f1b] <- as.character(dfb[,f1b])
                        dfb[,f2b] <- as.character(dfb[,f2b])
                        dfb[,varb] <- as.numeric(dfb[,varb])
                        ab <- interaction.maker(df = dfb ,factor1 = f1b, factor2 = f2b, value = varb, credvalue = input$CM)
                        aa <- post_hoc(ab, post.hoc.type = postHoctype())
                        return(aa)
                }else{
                        dfb <-datasetInput()
                        f1b <- as.character(input$Factor1)
                        f2b <- as.character(input$Factor2)
                        varb <- as.character(input$Outcome)
                        dfb[,f1b] <- as.character(dfb[,f1b])
                        dfb[,f2b] <- as.character(dfb[,f2b])
                        dfb[,varb] <- as.numeric(dfb[,varb])
                        ab <- interaction.maker(df = dfb ,factor1 = f1b, factor2 = f2b, value = varb, credvalue = input$CM)
                        aa <- post_hoc(ab, post.hoc.type = postHoctype())
                        return(aa)
                }

        })
        aaa <- reactive({
                if (input$dataset == "own"){
                        dfc <-filedata()
                        f1c <- as.character(input$Factor1)
                        f2c <- as.character(input$Factor2)
                        varc <- as.character(input$Outcome)
                        dfc[,f1c] <- as.character(dfc[,f1c])
                        dfc[,f2c] <- as.character(dfc[,f2c])
                        dfc[,varc] <- as.numeric(dfc[,varc])
                        ac <- interaction.maker(df = dfc ,factor1 = f1c, factor2 = f2c, value = varc, credvalue = input$CM)
                        aac <- post_hoc(ac, post.hoc.type = postHoctype())
                        aaa <- post_hoc_plot(post.hoc.object = aac,interaction.object = ac, p.val.criteria = input$pvalue, BF.criteria = input$BF)
                        return(aaa)
                }else{
                        dfc <-datasetInput()
                        f1c <- as.character(input$Factor1)
                        f2c <- as.character(input$Factor2)
                        varc <- as.character(input$Outcome)
                        dfc[,f1c] <- as.character(dfc[,f1c])
                        dfc[,f2c] <- as.character(dfc[,f2c])
                        dfc[,varc] <- as.numeric(dfc[,varc])
                        ac <- interaction.maker(df = dfc ,factor1 = f1c, factor2 = f2c, value = varc, credvalue = input$CM)
                        aac <- post_hoc(ac, post.hoc.type = postHoctype())
                        aaa <- post_hoc_plot(post.hoc.object = aac,interaction.object = ac, p.val.criteria = input$pvalue,BF.criteria = input$BF)
                        return(aaa)
                }

        })
        

        
        # Generate a summary of the dataset
        output$summary <- renderPrint({
                if(input$dataset == "own"){
                        dataset <- filedata()
                        str(dataset)                        
                }else{
                        dataset <- datasetInput()
                        str(dataset)   
                }

        })
        

        # # Show interaction structure - just for testing - not needed
        # observeEvent(input$inter,{
        #         output$interaction <- renderTable({
        #                 head(a()@df)
        #         })
        # })

        # Factor 1 results
        f1posthoctype <- eventReactive(input$f1results, {
                f1type <- paste("Post hoc test type for parametric analysis: ", aa()@post.hoc.type, sep="")
                f1type
        })

        output$F1testout <- renderText({
                 f1posthoctype()
        })

        f1comp <- eventReactive(input$f1results, {
                tablef1 <- aa()@f1.comps
                tablef1
        })
        output$F1tableout <- renderTable({
                f1comp()
        })
        f1plot <- eventReactive(input$f1results, {
                progress <- Progress$new(session, min=1, max=15)
                on.exit(progress$close())
                progress$set(message = 'Calculation in progress',
                             detail = 'This may take a while...')
                pp <- aaa()@plot.f1
                pp
        })
        output$F1plotout <- renderPlot({

                f1plot()
        })
        #observe(updateTextInput(session, "Post Hoc resutls F1", value = f1results()))
        
        # observe(input$f1results, { 
        #         output$phtestf1 <- renderText({
        #                 aa()@post.hoc.type
        #         })
        #         output$resultsf1out <- renderTable({
        #                 aa()@f1.comps
        #         })
        #         output$plotf1out <- renderPlot({
        #                 progress <- Progress$new(session, min=1, max=15)
        #                 on.exit(progress$close())
        #                 
        #                 progress$set(message = 'Calculation in progress',
        #                              detail = 'This may take a while...')
        #                 
        #                 for (i in 1:15) {
        #                         progress$set(value = i)
        #                         Sys.sleep(0.1)
        #                 }
        #                 print(aaa()@plot.f1)
        #         })
        # })

        # Factor 2 results
        f2posthoctype <- eventReactive(input$f2results, {
                f2type <- paste("Post hoc test type for parametric analysis: ",aa()@post.hoc.type, sep="")
                f2type
        })
        
        output$F2testout <- renderText({
                f2posthoctype()
        })
        
        f2comp <- eventReactive(input$f2results, {
                tablef2 <- aa()@f2.comps
                tablef2
        })
        output$F2tableout <- renderTable({
                f2comp()
        })
        f2plot <- eventReactive(input$f2results, {
                progress <- Progress$new(session, min=1, max=15)
                on.exit(progress$close())
                progress$set(message = 'Calculation in progress',
                             detail = 'This may take a while...')
                plotf2 <- aaa()@plot.f2
                plotf2
        })
        output$F2plotout <- renderPlot({
                f2plot()
        })
        # observeEvent(input$f2results, {
        #         output$phtestf2 <- renderText({
        #                 aa()@post.hoc.type
        #         })
        #         output$resultsf2out <- renderTable({
        #                 aa()@f2.comps
        #         })
        #         output$plotf2out <- renderPlot({
        #                 progress <- Progress$new(session, min=1, max=15)
        #                 on.exit(progress$close())
        #                 
        #                 progress$set(message = 'Calculation in progress',
        #                              detail = 'This may take a while...')
        #                 
        #                 for (i in 1:15) {
        #                         progress$set(value = i)
        #                         Sys.sleep(0.1)
        #                 }
        #                 print(aaa()@plot.f2)
        #         })
        # })
        
        # Interaction results
        intposthoctype <- eventReactive(input$intresults, {
                f2type <- paste("Post hoc test type for parametric analysis: ",aa()@post.hoc.type,sep="")
                f2type
        })
        
        output$INTtestout <- renderText({
                intposthoctype()
        })
        
        intcomp <- eventReactive(input$intresults, {
                tableint <- aa()@interaction.comps
                tableint
        })
        output$INTtableout <- renderTable({
                intcomp()
        })
        intplot <- eventReactive(input$intresults, {
                progress <- Progress$new(session, min=1, max=15)
                on.exit(progress$close())
                progress$set(message = 'Calculation in progress',
                             detail = 'This may take a while...')
                plotint <- aaa()@plot.inter
                plotint
        })
        output$INTplotout <- renderPlot({

                intplot()
        })
        # observeEvent(input$intresults, {
        #         output$phtestint <- renderText({
        #                 aa()@post.hoc.type
        #         })
        #         output$intresultsout <- renderTable({
        #                 aa()@interaction.comps
        #         })
        #         output$plotinterout <- renderPlot({
        #                 progress <- Progress$new(session, min=1, max=15)
        #                 on.exit(progress$close())
        #                 
        #                 progress$set(message = 'Calculation in progress',
        #                              detail = 'This may take a while...')
        #                 
        #                 for (i in 1:15) {
        #                         progress$set(value = i)
        #                         Sys.sleep(0.1)
        #                 }
        #                 print(aaa()@plot.inter)
        #         })
        # })
})