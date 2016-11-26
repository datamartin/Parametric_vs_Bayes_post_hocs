library(shiny)
library(datasets)
library(ggplot2)
library(car)
library(agricolae)
library(BayesFactor)
library(cowplot)
library(ggthemes)
library(stringr)
library(psych)#for datasets
source('C:/Users/Martin/ownCloud/Projects/R_APP16/app/CodesMartin/PBcomp_demo2/PBcomp_demo2/worker_functionsApp.R', local = TRUE)

shinyServer(function(input, output) {

        # Return the requested dataset
        datasetInput <- reactive({
                switch(input$dataset,
                       "Example data1" = sat.act,
                       "Example data2" = affect,
                       "own" = )
        })
        observe({
                input$reset1
                print("resetting")
                output$phtestf1 <- renderText({NULL})
                output$resultsf1out <- renderTable({NULL})
                output$plotf1out <- renderPlot({NULL})
                
                output$phtestf2 <- renderText({NULL})
                output$resultsf2out <- renderTable({NULL})
                output$plotf2out <- renderPlot({NULL})
                
                output$intresults <- renderText({NULL})
                output$intresultsout <- renderTable({NULL})
                output$plotinterout <- renderPlot({NULL})
        })
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
                        a <- interaction.maker(df = df ,factor1 = f1, factor2 = f2, value = var)
                        return(a)
                }else{
                        df <-datasetInput()
                        f1 <- as.character(input$Factor1)
                        f2 <- as.character(input$Factor2)
                        var <- as.character(input$Outcome)
                        df[,f1] <- as.character(df[,f1])
                        df[,f2] <- as.character(df[,f2])
                        df[,var] <- as.numeric(df[,var])
                        a <- interaction.maker(df = df ,factor1 = f1, factor2 = f2, value = var)
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
                        ab <- interaction.maker(df = dfb ,factor1 = f1b, factor2 = f2b, value = varb)
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
                        ab <- interaction.maker(df = dfb ,factor1 = f1b, factor2 = f2b, value = varb)
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
                        ac <- interaction.maker(df = dfc ,factor1 = f1c, factor2 = f2c, value = varc)
                        aac <- post_hoc(ac, post.hoc.type = postHoctype())
                        aaa <- post_hoc_plot(post.hoc.object = aac,interaction.object = ac, p.val.criteria = 0.05,BF.criteria = 0)
                        return(aaa)
                }else{
                        dfc <-datasetInput()
                        f1c <- as.character(input$Factor1)
                        f2c <- as.character(input$Factor2)
                        varc <- as.character(input$Outcome)
                        dfc[,f1c] <- as.character(dfc[,f1c])
                        dfc[,f2c] <- as.character(dfc[,f2c])
                        dfc[,varc] <- as.numeric(dfc[,varc])
                        ac <- interaction.maker(df = dfc ,factor1 = f1c, factor2 = f2c, value = varc)
                        aac <- post_hoc(ac, post.hoc.type = postHoctype())
                        aaa <- post_hoc_plot(post.hoc.object = aac,interaction.object = ac, p.val.criteria = 0.05,BF.criteria = 0)
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
        observeEvent(input$f1results, { 
                output$phtestf1 <- renderText({
                                aa()@post.hoc.type
                })
                output$resultsf1out <- renderTable({
                                aa()@f1.comps
                })
                output$plotf1out <- renderPlot({
                                print(aaa()@plot.f1)
                })
        })

        # Factor 2 results
        observeEvent(input$f2results, {
                output$phtestf2 <- renderText({
                                aa()@post.hoc.type
                })
                output$resultsf2out <- renderTable({
                                aa()@f2.comps
                })
                output$plotf2out <- renderPlot({
                                print(aaa()@plot.f2)
                })
        })
        
        # Interaction results
        observeEvent(input$intresults, {
                output$phtestint <- renderText({
                                aa()@post.hoc.type
                })
                output$intresultsout <- renderTable({
                                aa()@interaction.comps
                })
                output$plotinterout <- renderPlot({
                                print(aaa()@plot.inter)
                })
        })
})