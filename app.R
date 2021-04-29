library(shiny)
library(ggplot2)
library(zcurve)
library(dmetar)
source('setup.R')
source('carter_sim.R')
source('renkewitz_sim.R')
library(thematic)
library(shinycssloaders)
library(bslib)

ui <- navbarPage("The Bias Detection Showdown",
    theme = bs_theme(version = 4, bootswatch = "darkly"),
    tabPanel("Summary",
             fluidPage(column(8,
                              offset = 4,
                              align="left",
                             includeMarkdown('Welcome.Rmd')))

         ),
    tabPanel("Carter et al., 2019",
              fluidRow(

                  column(4,
                         offset = 1,
                         style='padding:0px;',
                         sliderInput("studies",
                                     "Number of studies to be plotted:",
                                     min = 1,
                                     max = 200,
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
                         selectInput("qrp",
                                     "The severity of QRP:",
                                     choices = c("none","low","medium","high")),
                         selectInput("bias",
                                     "The severity of publication bias",
                                     choices = c("none","med","high"))
                  )
              ),


              fluidRow(
                column(5,
                      plotOutput("z_plot_c")%>% withSpinner(color="#0dc5c1"),
                      verbatimTextOutput("z_summary_c")),
                column(5,
                       offset = 2,
                       plotOutput("p_plot_c")%>% withSpinner(color="#0dc5c1"),
                       verbatimTextOutput("p_summary_c"))
                     )
    ),
    tabPanel("Renkewitz & Keiner, 2019",
             fluidRow(

               column(4,
                      offset = 1,
                      sliderInput("k",
                                  "Number of studies to be plotted:",
                                  min = 10,
                                  max = 200,
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
                                  step = 0.1)
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
             fluidRow(
               column(5,
                      offset = 0,
                      plotOutput("z_plot_r")%>% withSpinner(color="#0dc5c1"),
                      verbatimTextOutput("z_summary_r")),
               column(5,
                      offset = 2,
                      plotOutput("p_plot_r")%>% withSpinner(color="#0dc5c1"),
                      verbatimTextOutput("p_summary_r"))
        )
    )
)







server = function(input, output, session){
    ####################################################################
                          # Carter

    carter = reactive({
        simMA(k = input$studies, delta = input$effect, tau = input$sd, qrpEnv =  as.character(input$qrp), censorFunc = as.character(input$bias))
    })

    output$z_plot_c = renderPlot({
        mydata = carter()
        z = zcurve(p = mydata$p, method = 'EM', bootstrap = 100)
        plot(z, annotation = TRUE, CI = TRUE, x_text = 0)
    })

    output$z_summary_c <- renderPrint({

      mydata = renkewitz()

      summary(zcurve(p = mydata$p.Wert_2tail, method = 'EM', bootstrap = 100))
    })

    output$p_plot_c <- renderPlot({
        mydata = carter()
        mydata = data.frame("TE" = mydata$d,
                          "seTE" = mydata$se,
                          "studlab" = rownames(mydata))
        pcurve(mydata)
    })
    output$p_summary_c <- renderPrint({
      mydata = carter()
      mydata = data.frame("TE" = mydata$d,
                          "seTE" = mydata$se,
                          "studlab" = rownames(mydata))
      pcurve(mydata)
    })
    #####################################################################
                          # Renkewitz

    renkewitz = reactive({
      current = input$r_sim_type
      print(current)
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


    output$z_plot_r = renderPlot({

      mydata = renkewitz()

      z = zcurve(p = mydata$p.Wert_2tail, method = 'EM', bootstrap = 100)
      plot(z, annotation = TRUE, CI = TRUE, x_text = 0)
    })

    output$z_summary_r <- renderPrint({

      mydata = renkewitz()

      summary(zcurve(p = mydata$p.Wert_2tail, method = 'EM', bootstrap = 100))
    })

    output$p_plot_r <- renderPlot({

      mydata = renkewitz()

      mydata = data.frame("TE" = mydata$d,
                          "seTE" = mydata$se,
                          "studlab" = rownames(mydata))
      pcurve(mydata)
    })

    output$p_summary_r <- renderPrint({
      mydata = renkewitz()
      mydata = data.frame("TE" = mydata$d,
                          "seTE" = mydata$se,
                          "studlab" = rownames(mydata))
      pcurve(mydata)
    })



}
shinyApp(ui = ui, server = server)
