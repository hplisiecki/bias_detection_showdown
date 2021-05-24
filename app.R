
source('setup.R')
source('simulation_functions/carter_sim.R')
source('simulation_functions/renkewitz_sim.R')

###############################################################################################
#                              The Display Part of the App
#                             #############################
#   This is the part of code which sets the user interface (UI) of the app. It displays it's
#   content in four tabs, one for introduction, two additional ones for the two different
#   simulation techniques, and one for conclusion. 

ui <- navbarPage("The Bias Detection Showdown",   # title

    # set theme (choose theme at: https://bootswatch.com/3/ )
    theme = bs_theme(version = 4, bootswatch = "darkly"),


    #                  First panel - the summary
    #
    #   Displays an RMarkdown file with the introductory information

    tabPanel("Introduction",
             fluidPage(column(8,
                              offset = 4,
                              align="left",
                             includeMarkdown('markdown_files/introduction.Rmd')))
         ),


    #              Second panel - the Carter et al., 2019 simulation
    #
    #    Displays the interface for manipulating the simulation process, and shows
    #    the output of two bias detection techniques (pcurve & zcurve). Uses the simulation
    #    technique from the Carter and colleagues study (2019).

    tabPanel("Carter et al., 2019",
              fluidRow(             # fluid row for simulation control

                  column(4,
                         offset = 1,
                         style='padding:0px;',
                         sliderInput("studies",
                                     "Number of studies to be plotted:",
                                     min = 1,
                                     max = 1000,
                                     value = 100),
                         sliderInput("effect",
                                     "The size of the effect:",
                                     min = 0.0,
                                     max = 1.0,
                                     value = 0.2),
                         sliderInput("sd",
                                     "The standard deviation around the true effect:",
                                     min = 0.0,
                                     max = 3.0,
                                     value = 3.0,
                                     step = 0.1)
                  ),
                  column(2,
                         offset = 4,
                         sliderInput("N",
                                     "Sample size per group:",
                                     min = 10,
                                     max = 100,
                                     value = 20),
                         selectInput("qrp",
                                     "The severity of QRP:",
                                     choices = c("none","low","medium","high")),
                         selectInput("bias",
                                     "The severity of publication bias",
                                     choices = c("none","med","high")),
                         verbatimTextOutput('power_c')
                  )
              ),


              fluidRow(            # fluid row for bias detection techniques display
                column(5,
                      plotOutput("z_plot_c")%>% withSpinner(color="#0dc5c1"),  # the z-curve plot
                      verbatimTextOutput("z_summary_c")),                      # the z-curve text output
                column(5,
                       offset = 2,
                       plotOutput("p_plot_c")%>% withSpinner(color="#0dc5c1"), # the p-curve plot
                       verbatimTextOutput("p_summary_c"))                      # the p-curve text output
                     )
    ),


    #         Third panel - the Renkewitz & Keiner, 2019 simulation
    #
    #    Displays the interface for manipulating the simulation process, and shows
    #    the output of two bias detection techniques (pcurve & zcurve). Uses the simulation
    #    technique from Renkewitz & Keiner's study (2019).

    tabPanel("Renkewitz & Keiner, 2019",
             fluidRow(                    # fluid row for simulation control

               column(4,
                      offset = 1,
                      sliderInput("k",
                                  "Number of studies to be plotted:",
                                  min = 10,
                                  max = 1000,
                                  value = 100),
                      sliderInput("Tau",
                                  "Heterogeneity",
                                  min = 0.0,
                                  max = 1.0,
                                  value = 0.2),
                      sliderInput("n_min",
                                  "Minimum sample size",
                                  min = 10,
                                  max = 50,
                                  value = 20),
                      sliderInput("n_max",
                                  "Maximum sample size",
                                  min = 30,
                                  max = 100,
                                  value = 40)
                      ),
               column(4,
                      sliderInput("m1g",
                                  "The size of the effect",
                                  min = 0.0,
                                  max = 1.0,
                                  value = 0.2),
                      sliderInput("sd1",
                                  "SD of the experimental group",
                                  min = 0.0,
                                  max = 3.0,
                                  value = 1.0,
                                  step = 0.1),
                      sliderInput("sd2",
                                  "SD of the control group",
                                  min = 0.0,
                                  max = 3.0,
                                  value = 1.0,
                                  step = 0.1),
                      verbatimTextOutput("power_r")
                      ),
               column(2,
                      h4("Type of the simulation"),
                      offset = 0,
                      selectInput("r_sim_type",
                                  "Choose the simulation type:",
                                  choices = c("no_bias", "full_bias", "optional_stopping",
                                              "two_step_bias", "ninety_percent_bias")),
                      sliderInput("nn",
                                  "Increase of the sample size under optional stopping",
                                  min = 1,
                                  max = 5,
                                  value = 1)
               )
             ),

             fluidRow(            # fluid row for bias detection techniques display
               column(5,
                      offset = 0,
                      plotOutput("z_plot_r")%>% withSpinner(color="#0dc5c1"),  # the z-curve plot
                      verbatimTextOutput("z_summary_r")),                      # the z-curve text output
               column(5,
                      offset = 2,
                      plotOutput("p_plot_r")%>% withSpinner(color="#0dc5c1"),  # the p-curve plot
                      verbatimTextOutput("p_summary_r"))                       # the p-curve text output
        )
    ),


    #                  Fourth panel - the summary
    #
    #   Displays the conclusion in relation to Dr. Schimmack's criticism

    tabPanel("Conclusion",
             fluidPage(column(8,
                              offset = 4,
                              align="left",
                              includeMarkdown('markdown_files/conclusion1.Rmd')),
                       column(3,
                              offset = 4,
                              allign = "left",
                              verbatimTextOutput("conclusion_c")%>% withSpinner(color="#0dc5c1")),
                       column(8,
                              offset = 4,
                              align="left",
                              includeMarkdown('markdown_files/conclusion2.Rmd')),
                       column(3,
                              offset = 4,
                              allign = "left",
                              verbatimTextOutput("conclusion_r")%>% withSpinner(color="#0dc5c1"))
                       )
    )
)



#####################################################################################################
#                              The Reactive Part of the App
#                             #############################
#   This is the part of code which reacts to the changes in the user input (simulation specification),
#   and adjusts the output of the bias detection techniques accordingly. It makes calls to functions
#   used in two simulation studies (Carter et al., 2018; Renkewitz & Keiner, 2019).


server = function(input, output, session){


    #                    The Carter and colleagues (2019) simulation


        # First the simulation function is set

    carter = reactive({

        simMA(k = input$studies, delta = input$effect,
              tau = input$sd, qrpEnv =  as.character(input$qrp),
              censorFunc = as.character(input$bias), fixed.n = input$N)

    })

        # The aforementioned function is used in the output objects to get the simulated data,
        # this data is then subjected to bias detection techniques.


        # the z-curve plot output object

    output$z_plot_c = renderPlot({

        mydata = carter()                                          # get the data

        z = zcurve(p = mydata$p, method = 'EM', bootstrap = 100)   # apply the z-curve

        plot(z, annotation = TRUE, CI = TRUE, x_text = 0)          # plot the z-curve

    })


        # the z-curve description output object

    output$z_summary_c <- renderPrint({

        mydata = carter()                                           # get the data

        summary(zcurve(p = mydata$p,                     # get the description
                       method = 'EM', bootstrap = 100))

    })


        # the p-curve plot output object

    output$p_plot_c <- renderPlot({

        mydata = carter()                                           # get the data

        mydata = data.frame("TE" = mydata$d,                        # Reformat the data
                          "seTE" = mydata$se,                       #   so that it fits the
                          "studlab" = rownames(mydata))             #   required format

        pcurve(mydata)                                              # plot the p-curve

    })


        # the p-curve description output object
    output$p_summary_c <- renderPrint({

        mydata = carter()                                           # get the data

        mydata = data.frame("TE" = mydata$d,                        # Reformat the data so
                          "seTE" = mydata$se,                       #    that it fits the
                          "studlab" = rownames(mydata))             #    required format

        pcurve(mydata)                                              # plot the p-curve

    })


    output$power_c <- renderPrint({

      mydata = carter()                                             # get the data

      mydata = mydata[which(mydata$p <= 0.05),]                     # select significant studies

      power = mean(mydata$pow)                                      # calculate the power estimate

      cat("Power: ", power)                                         # print

    })


    #                     The Renkewitz & Keiner (2019) simulation



        # The simulation function. It changes depending on the type of bias that is being modelled.
        #    For more specific explanation of different types of bias see Renkewitz & Keiner (2019).
        #    The function returns simulated data.

    renkewitz = reactive({
      current = input$r_sim_type
      simed = switch(current,
                 no_bias = {
                   renk_no_bias(m = 1, input$k, input$n_min, input$n_max, input$m1g,
                                input$Tau, input$sd1, m2 = 0, input$sd2)
                 },
                 full_bias = {
                   renk_100prcnt_bias(m = 1, input$k, input$n_min, input$n_max, input$m1g,
                                      input$Tau, input$sd1, m2 = 0, input$sd2, sigS = 0.05)
                 },
                 optional_stopping = {
                   renk_optional_stopping(m = 1, input$k, input$n_min, input$nn, input$n_max, input$m1g,
                                          input$Tau, input$sd1, m2 = 0, input$sd2, sigS = 0.05)
                 },
                 two_step_bias = {
                   renk_two_step_bias(m = 1, input$k, input$n_min, input$n_max, input$m1g,
                                      input$Tau, input$sd1, m2 = 0, input$sd2, sigS = 0.05,
                                      AsigS = 1, nsigS = 0.1, AnsigS_1 = 0.2, AnsigS_2 = 0)
                 },
                 ninety_percent_bias = {
                   renk_90prcnt_bias(m = 1, input$k, input$n_min, input$n_max, input$m1g,
                                     input$Tau, input$sd1, m2 = 0, input$sd2, sigS = 0.05,
                                     Asig = 0.9)
             })
      simed <- as.data.frame(simed)
      return(simed)
    })


         # The z-curve plot output object

    output$z_plot_r = renderPlot({

      mydata = renkewitz()                                      # get the data

      z = zcurve(p = mydata$p.Wert_2tail,                       # apply the z-curve
                 method = 'EM', bootstrap = 100)

      plot(z, annotation = TRUE, CI = TRUE, x_text = 0)         # plot the z-curve

    })


         # The z-curve description output object

    output$z_summary_r <- renderPrint({

      mydata = renkewitz()                                      # get the data

      summary(zcurve(p = mydata$p.Wert_2tail,                   # get the z-curve description
                     method = 'EM', bootstrap = 100))

    })


         # The p-curve plot output object

    output$p_plot_r <- renderPlot({

      mydata = renkewitz()                                       # get the data

      mydata = data.frame("TE" = mydata$d,                       # Reformat the data so
                          "seTE" = mydata$se,                    #   that it fits the
                          "studlab" = rownames(mydata))          #   required format

      pcurve(mydata)                                             # plot the p-curve

    })


         # The p-curve description output object

    output$p_summary_r <- renderPrint({

      mydata = renkewitz()                                       # get the data

      mydata = data.frame("TE" = mydata$d,                       # Reformat the data so
                          "seTE" = mydata$se,                    #    that it fits the
                          "studlab" = rownames(mydata))          #    required format

      pcurve(mydata)                                             # get the p-curve description

    })

    output$power_r<- renderPrint({

      mydata = renkewitz()                                       # get the data

      mydata = mydata[which(mydata$p.Wert_2tail <= 0.05),]       # select significant studies

      power = mean(mydata$power)                                 # calculate the power estimate

      cat('Power: ', power)                                      # print

    })

    output$conclusion_c <- renderPrint({

      mydata = carter()                                           # get the data

      p_data = data.frame("TE" = mydata$d,                        # Reformat the data so
                          "seTE" = mydata$se,                       #    that it fits the
                          "studlab" = rownames(mydata))             #    required format

      p = pcurve(p_data)                                          # calculate the p-curve

      z = summary(zcurve(p = mydata$p,                            # calculate the z-curve
                        method = 'EM', bootstrap = 100))

      mydata = mydata[which( mydata$p <= 0.05),]                  # select significant studies

      power = mean(mydata$pow)                                    # calculate the power estimate


      cat('Real estimated power: ', '\n',power, '\n\n')           # print
      cat("P-curve estimated power:\n")
      print(p$Power)
      cat('\n')
      cat("Z-curve estimated power:\n")
      print(z$coefficients)
    })

    output$conclusion_r <- renderPrint({

      mydata = renkewitz()                                        # get the data

      p_data = data.frame("TE" = mydata$d,                        # Reformat the data so
                          "seTE" = mydata$se,                     #    that it fits the
                          "studlab" = rownames(mydata))           #    required format

      p = pcurve(p_data)

      z = summary(zcurve(p = mydata$p.Wert_2tail,                 # calculate the z-curve
                         method = 'EM', bootstrap = 100))

      mydata = mydata[which( mydata$p.Wert_2tail <= 0.05),]       # select significant studies

      power = mean(mydata$power)                                  # calculate the power estimate


      cat('Real estimated power: ', '\n',power, '\n\n')           # print
      cat("P-curve estimated power:\n")
      print(p$Power)
      cat('\n')
      cat("Z-curve estimated power:\n")
      print(z$coefficients)
    })


}


shinyApp(ui = ui, server = server)       # The line which starts the app

