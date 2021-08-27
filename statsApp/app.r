## app.R ##
library(shinydashboard)
library(tidyverse)
library(plotly)
library(here)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", icon = icon("chart-area"), tabName = "intro"),
    menuItem("Bernoulli Baseball", icon = icon("baseball-ball"), tabName = "bernoulli"),
    menuItem("Poisson Emails", tabName = "poisson", icon = icon("envelope-square")),
    menuItem("Exponential Homework", icon = icon("scroll"), tabName = "exponential"),
    menuItem("Normal Heights", icon = icon("male"), tabName = "normal")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro",
            h2("Welcome!"),
            fluidRow(
              box("This dashboard was made to demonstrate the use of conjugate families in prior and posterior distributions. Material for this dashboard came from Nate's Math 392 class and Probability and Statistics, Fourth Edition by Degroot and Schervish."),
              box(imageOutput("image"),
                  height = 900)
            )),
    tabItem(tabName = "poisson",
            h2("Poisson/Gamma Conjugate Family"),
            
            fluidRow(
              tabBox(tabPanel("Sample Data", plotlyOutput("plot2", height = 250)),
                     tabPanel("Info", "Your emails recieved per day are Poisson distributed, which means you can use Gamma prior and posteriors to estimate the rate at which you recieve emails per day. Here, you have a sample of Nate's emails for 30 days. Adjust the prior to see how it can affect the posterior distribution!"),
                     tabPanel("Math", withMathJax(),
                              helpText('If our \\(X\\)s are Poisson distributed with unknown parameter \\(\\theta\\) with a Gamma \\((\\alpha , \\beta )\\) prior, then the posterior distribution of \\(\\theta\\) is a Gamma distribution with parameters: $$\\alpha + \\sum x_i, \\beta + n$$'))),
              
              box(
                sliderInput(inputId = "slider_alpha",
                            label = "Gamma Prior Alpha",
                            min = 0, max = 50,
                            value = 25,
                            step = 1)
              ),
              box(
                sliderInput(inputId = "slider_beta",
                            label = "Gamma Prior Beta",
                            min = 0, max = 10,
                            value = 2,
                            step = 0.25)
              )
            ),
            fluidRow(
              box(plotlyOutput("plot1", height = 250)),
              box(plotlyOutput("plot3", height = 250))
            )
    ),
    
    tabItem(tabName = "bernoulli",
            h2("Bernoulli/Beta Conjugate Family"),
            fluidRow(
              tabBox(tabPanel("Balls hit", sliderInput(inputId = "slider_balls",
                                                       label = "How many balls did Phil Hatt hit out of ten?",
                                                       min = 0, max = 10,
                                                       value = 2,
                                                       step = 1)),
                     tabPanel("Info", "Phil Hatt is trying out for your baseball team, and you want to know his batting average, so you let him have ten tries. Decide your prior distribution for his batting average and how many balls Phil manages to hit, and see how the psoterior distribution of his batting average changes."),
                     tabPanel("Math", withMathJax(),
                              helpText('If our \\(X\\)s has a Bernoulli distribution with unknown parameter \\(\\theta\\) with a Beta \\((\\alpha , \\beta )\\) prior, then the posterior distribution of \\(\\theta\\) is a Beta distribution with parameters: $$\\alpha + \\sum x_i, \\beta + n - \\sum x_i$$'))),
              
              box(
                sliderInput(inputId = "slider_alpha_beta",
                            label = "Beta Prior Alpha",
                            min = 0, max = 150,
                            value = 10,
                            step = 1)
              ),
              box(
                sliderInput(inputId = "slider_beta_beta",
                            label = "Beta Prior Beta",
                            min = 0, max = 150,
                            value = 50,
                            step = 1)
              )
            ),
            fluidRow(
              box(plotlyOutput("plot1_bern", height = 250)),
              box(plotlyOutput("plot3_bern", height = 250))
            )
    ),
    
    tabItem(tabName = "exponential",
            h2("Exponential/Gamma Conjugate Family"),
            fluidRow(
              tabBox(tabPanel("Homework and hours", sliderInput(inputId = "slider_hw",
                                                                label = "How many problems did you do?",
                                                                min = 0, max = 15,
                                                                value = 6,
                                                                step = 1),
                              sliderInput(inputId = "slider_hours",
                                          label = "How many hours did it take?",
                                          min = 0, max = 15,
                                          value = 3,
                                          step = 1)),
                     tabPanel("Info", "You believe that the distribution of time to complete your stats homework problems is exponential, so you decide to work for a few hours and record how many problems you get through. Decide your prior, the hours worked, and the problems completed to see how they affect the posterior distribution of the rate."),
                     tabPanel("Math", withMathJax(),
                              helpText('If our \\(X\\)s are exponentially distributed with unknown parameter \\(\\theta\\) with a Gamma \\((\\alpha , \\beta )\\) prior, then the posterior distribution of \\(\\theta\\) is a Gamma distribution with parameters: $$\\alpha + n, \\beta + \\sum x_i$$')
                     )),
              
              box(
                sliderInput(inputId = "slider_alpha_expo_gamma",
                            label = "Beta Prior Alpha",
                            min = 0, max = 20,
                            value = 10,
                            step = 1)
              ),
              box(
                sliderInput(inputId = "slider_beta_expo_gamma",
                            label = "Beta Prior Beta",
                            min = 0, max = 20,
                            value = 5,
                            step = 1)
              )
            ),
            
            fluidRow(
              box(plotlyOutput("plot1_exp_gamma", height = 250)),
              box(plotlyOutput("plot3_exp_gamma", height = 250)))
            
    ),
    tabItem(tabName = "normal",
            h2("Normal/Normal Conjugate Family"),
            
            fluidRow(
              tabBox(tabPanel("Sample Data", plotlyOutput("plot2_normal", height = 250)),
                     tabPanel("True Variance",
                              sliderInput(inputId = "slider_true_var",
                                          label = "",
                                          min = 0, max = 60,
                                          value = 36,
                                          step = 1)),
                     tabPanel("Info", "Let's assume height is normally distributed and you want to find out what the average height is. Here are the heights of some of Olek's friends(perhaps a biased sample), and you can adjust the prior, as well as the true variance of height, which is needed for this conjugate family."),
                     tabPanel("Math", withMathJax(),
                              helpText('If our \\(X\\)s are normally distributed with unknown mean \\(\\mu\\) and known variance \\(\\sigma^2\\) with a Normal \\((\\mu_0, \\nu^2)\\) prior, then the posterior distribution of \\(\\mu\\) is a Normal distribution with parameters: $$\\frac{\\sigma^2 \\mu_0 + n \\nu^2 \\bar{x}}{\\sigma^2 + n \\nu^2}, \\frac{\\sigma^2 \\mu^2}{\\sigma^2 + n \\nu^2}$$'))),
              
              box(
                sliderInput(inputId = "slider_mean",
                            label = "Prior Mean",
                            min = 50, max = 90,
                            value = 76,
                            step = 1)
              ),
              box(
                sliderInput(inputId = "slider_var",
                            label = "Prior Variance",
                            min = 0, max = 60,
                            value = 36,
                            step = 1)
              )
            ),
            fluidRow(
              box(plotlyOutput("plot1_normal", height = 250)),
              box(plotlyOutput("plot3_normal", height = 250))
            )
    )
    
  ))


ui <- dashboardPage(
  dashboardHeader(title = "Math 392 Dashboard"),
  sidebar,
  body
)


server <- function(input, output) {
  
  output$image <- renderImage({
    filename <- here("monkeys.jpg")
    list(
      src = filename, 
      height = 900
    )
  }, deleteFile = FALSE)
  
  #gamma
  gamma_x <- seq(from = 0,
                 to = 50,
                 by = 0.1)
  
  sample<-c(16, 13, 26, 22, 18, 23, 26, 25, 25, 18, 27, 16, 26,
            18, 18, 14, 20, 18, 24, 13, 30, 22, 13, 19, 17, 24, 27, 25, 22, 14)
  
  sample_data <- data.frame(email_number = 0:30,
                            count = 0)
  
  sample_data_two <- data.frame(email_number = sample) %>%
    group_by(email_number) %>%
    summarize(count = n())
  
  sample_data <- bind_rows(sample_data, sample_data_two) %>%
    group_by(email_number) %>%
    summarize(count = sum(count))
  
  prior <- reactive({
    gamma_prior_y <- dgamma(gamma_x,
                            shape = input$slider_alpha,
                            rate = input$slider_beta)
    
    data.frame(gamma_x,
               gamma_prior_y)
  })
  
  posterior <- reactive({
    alpha_posterior <- input$slider_alpha + sum(sample)
    beta_posterior <- input$slider_beta + length(sample)
    
    gamma_posterior_y <- dgamma(gamma_x,
                                shape = alpha_posterior,
                                rate = beta_posterior)
    
    data.frame(gamma_x, gamma_posterior_y)
  })
  
  
  
  output$plot2 <- renderPlotly({
    plot_ly(sample_data,
            y = ~count,
            x = ~email_number,
            name = "Given Data",
            type = "bar") %>%
      layout(title = "Sample of Emails",
             xaxis = list(title = "Number of Emails in a day"))
  })
  
  output$plot1 <- renderPlotly({
    plot_ly(data = prior(),
            x = ~gamma_x, y = ~gamma_prior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 30),
                          title = ""),
             yaxis = list(range = c(0, 1),
                          title = ""),
             title = "Prior Distribution")
  })
  output$plot3 <- renderPlotly({
    plot_ly(data = posterior(),
            x = ~gamma_x, y = ~gamma_posterior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 30),
                          title = ""),
             yaxis = list(range = c(0, 1),
                          title = ""),
             title = "Posterior Distribution")
  })
  
  #benoulli
  
  beta_x <- seq(from = 0,
                to = 1,
                by = 0.01)
  
  prior_beta <- reactive({
    beta_prior_y <- dbeta(beta_x,
                          shape1 = input$slider_alpha_beta,
                          shape2 = input$slider_beta_beta)
    
    data.frame(beta_x,
               beta_prior_y)
  })
  
  posterior_beta <- reactive({
    alpha_posterior_beta <- input$slider_alpha_beta + input$slider_balls
    beta_posterior_beta <- input$slider_beta_beta + 10 - input$slider_balls
    
    beta_posterior_y <- dbeta(beta_x,
                              shape1 = alpha_posterior_beta,
                              shape2 = beta_posterior_beta)
    
    data.frame(beta_x, beta_posterior_y)
  })
  
  
  output$plot1_bern <- renderPlotly({
    plot_ly(data = prior_beta(),
            x = ~beta_x, y = ~beta_prior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 1),
                          title = ""),
             yaxis = list(range = c(0, 15),
                          title = ""),
             title = "Prior Distribution")
  })
  output$plot3_bern <- renderPlotly({
    plot_ly(data = posterior_beta(),
            x = ~beta_x, y = ~beta_posterior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 1),
                          title = ""),
             yaxis = list(range = c(0, 15),
                          title = ""),
             title = "Posterior Distribution")
  })
  
  #exponential
  
  gamma_exp_x <- seq(from = 0,
                     to = 10,
                     by = 0.05)
  
  prior_exp_gamma <- reactive({
    expo_gamma_prior_y <- dgamma(gamma_exp_x,
                                 shape = input$slider_alpha_expo_gamma,
                                 rate = input$slider_beta_expo_gamma)
    
    data.frame(gamma_exp_x,
               expo_gamma_prior_y)
  })
  
  posterior_exp_gamma <- reactive({
    exp_alpha_posterior <- input$slider_alpha_expo_gamma + input$slider_hours
    exp_beta_posterior <- input$slider_beta_expo_gamma + input$slider_hw
    
    exp_gamma_posterior_y <- dgamma(gamma_exp_x,
                                    shape = exp_alpha_posterior,
                                    rate = exp_beta_posterior)
    
    data.frame(gamma_exp_x, exp_gamma_posterior_y)
  })
  
  output$plot1_exp_gamma <- renderPlotly({
    plot_ly(data = prior_exp_gamma(),
            x = ~gamma_exp_x, y = ~expo_gamma_prior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 3),
                          title = ""),
             yaxis = list(range = c(0, 3),
                          title = ""),
             title = "Prior Distribution")
  })
  output$plot3_exp_gamma <- renderPlotly({
    plot_ly(data = posterior_exp_gamma(),
            x = ~gamma_exp_x, y = ~exp_gamma_posterior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(0, 3),
                          title = ""),
             yaxis = list(range = c(0, 3),
                          title = ""),
             title = "Posterior Distribution")
  })
  
  #normal
  normal_x <- seq(from = 50,
                  to = 90,
                  by = 0.1)
  
  sample_heights <- c(76, 69, 70, 67, 62, 65, 74, 71, 72, 70, 69, 70)
  
  sample_data_heights <- data.frame(height_inches = 0:12,
                                    count = 0)
  
  sample_data_two_heights <- data.frame(height_inches = sample_heights) %>%
    group_by(height_inches) %>%
    summarize(count = n())
  
  sample_data_heights <- bind_rows(sample_data_heights, sample_data_two_heights) %>%
    group_by(height_inches) %>%
    summarize(count = sum(count))
  
  prior_normal <- reactive({
    normal_prior_y <- dnorm(normal_x,
                            mean = input$slider_mean,
                            sd = sqrt(input$slider_var))
    
    data.frame(normal_x,
               normal_prior_y)
  })
  
  posterior_normal <- reactive({
    mean_posterior <- ((input$slider_true_var * input$slider_mean) + (12 * input$slider_var * mean(sample_heights)))/(input$slider_true_var + (12 * input$slider_var))
    var_posterior <- (input$slider_true_var * input$slider_var) / (input$slider_true_var + (12 * input$slider_var))
    
    normal_posterior_y <- dnorm(normal_x,
                                mean = mean_posterior,
                                sd = sqrt(var_posterior))
    
    data.frame(normal_x, normal_posterior_y)
  })
  
  
  
  output$plot2_normal <- renderPlotly({
    plot_ly(sample_data_heights,
            y = ~count,
            x = ~height_inches,
            name = "Given Data",
            type = "bar") %>%
      layout(title = "Sample of Heights",
             xaxis = list(title = "Height(inches)",
                          range = c(50, 90)))
  })
  
  output$plot1_normal <- renderPlotly({
    plot_ly(data = prior_normal(),
            x = ~normal_x, y = ~normal_prior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(50, 90),
                          title = ""),
             yaxis = list(range = c(0, 0.5),
                          title = ""),
             title = "Prior Distribution")
  })
  output$plot3_normal <- renderPlotly({
    plot_ly(data = posterior_normal(),
            x = ~normal_x, y = ~normal_posterior_y,
            type = 'scatter',
            mode = 'lines') %>% 
      layout(xaxis = list(range = c(50, 90),
                          title = ""),
             yaxis = list(range = c(0, 0.5),
                          title = ""),
             title = "Posterior Distribution")
  })
}

shinyApp(ui, server)