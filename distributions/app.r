library(shinydashboard)
library(plotly)
library(shiny)

#bernoulli
#binomial
#hypergeometric
#poisson
#negative binomial
#normal
#gamma
#exponential
#beta



#menu items
menu_welcome <- menuItem("Welcome!",
                          tabName = "welcome",
                          icon = icon("star"))

menu_bernoulli <- menuItem("Bernoulli",
                          tabName = "bernoulli",
                          icon = icon("balance-scale"))

menu_binomial <- menuItem("Binomial",
                          tabName = "binomial",
                          icon = icon("coins"))

menu_hypergeom <- menuItem("Hypergeometric",
                           tabName = "hypergeom",
                           icon = icon("dot-circle"))

menu_geometric <- menuItem("Geometric",
                           tabName = "geometric",
                           icon = icon("check-circle"))

menu_negativebinom <- menuItem("Negative Binomial",
                                tabName = "negativebinom",
                                icon = icon("check-double"))

menu_poisson <- menuItem("Poisson",
                         tabName = "poisson",
                         icon = icon("tint"))

menu_normal <- menuItem("Normal",
                        tabName = "normal",
                        icon = icon("meh"))

menu_exponential <- menuItem("Exponential",
                             tabName = "exponential",
                             icon = icon("bus"))

menu_beta <- menuItem("Beta",
                      tabName = "beta",
                      icon = icon("signature"))

menu_gamma <- menuItem("Gamma",
                       tabName = "gamma",
                       icon = icon("react"))
 
#create sidebar
sidebar <- dashboardSidebar(sidebarMenu(menu_welcome,
                                        menu_bernoulli,
                                        menu_binomial,
                                        menu_hypergeom,
                                        menu_geometric,
                                        menu_negativebinom,
                                        menu_poisson,
                                        menu_normal,
                                        menu_exponential,
                                        menu_beta,
                                        menu_gamma))

#tab items

#welcome
tab_item_welcome <-tabItem(tabName = "welcome",
                           h2("Hello"),
                           h2("hi")
                           )

#bernoulli
tab_item_bernoulli <-tabItem(tabName = "bernoulli",
                             h2("Bernoulli"),
                             fluidRow(
                               box(withMathJax(),
                               helpText("A random variable \\(X\\) is said to have the Bernoulli distribution with
                                parameter \\(p\\) if \\(P(X = 1) = p\\) and \\(P(X = 0) = 1 - p\\),
                                         where \\(0 < p < 1\\)")),
                               box(
                                 sliderInput(inputId = "slider_bernoulli_p",
                                             label = helpText("\\(p\\)"),
                                             min = 0, max = 1,
                                             value = 0.5,
                                             step = 0.1)
                               )
                             ),
                             fluidRow(
                               box(plotlyOutput("plotly_bernoulli", height = 250))
                             ),
                             fluidRow(
                               box(actionButton("bernoulli_random_button",
                                                label = "Generate Random Values!"),
                                   hr(),
                                   verbatimTextOutput("bernoulli_random_output"))
                             )
                             )

#binomial
tab_item_binomial <-tabItem(tabName = "binomial",
                            h2("Binomial"),
                            fluidRow(
                              box(withMathJax(),
                              helpText("If \\(X\\) is a random variable with a Binomial distribution, then the PMF
                                        of \\(X\\) is \\( P(X = k) = \\binom{n}{k} p^k (1 - p)^{n - k}\\)")),
                               box(
                                 sliderInput(inputId = "slider_binomial_p",
                                             label = helpText("\\(p\\)"),
                                             min = 0, max = 1,
                                             value = 0.5,
                                             step = 0.1),
                                 sliderInput(inputId = "slider_binomial_n",
                                             label = helpText("\\(n\\)"),
                                             min = 0, max = 15,
                                             value = 10,
                                             step = 1)
                               )
                             ),
                             fluidRow(
                               box(plotlyOutput("plotly_binomial_PMF", height = 250)),
                               box(plotlyOutput("plotly_binomial_CDF", height = 250))
                             ),
                             fluidRow(
                               box(actionButton("binomial_random_button",
                                                label = "Generate Random Values!"),
                                   hr(),
                                   verbatimTextOutput("binomial_random_output"))
                             )
)

#hypergeom
tab_item_hypergeom <-tabItem(tabName = "hypergeom",
                             h2("Hypergeometric"),
                             fluidRow(
                               box(withMathJax(),
                               helpText("If \\(X\\) is a random variable with a Binomial distribution, then the PMF
                                        of \\(X\\) is \\( P(X = k) = \\frac{\\binom{w}{k} \\binom{b}{n - k}}
                                        {\\binom{w + b}{n}}\\)")),
                               box(
                                sliderInput(inputId = "slider_hypergeom_w",
                                            label = helpText("\\(w\\)"),
                                            min = 0, max = 10,
                                            value = 5,
                                            step = 1),
                                sliderInput(inputId = "slider_hypergeom_b",
                                            label = helpText("\\(b\\)"),
                                            min = 0, max = 10,
                                            value = 5,
                                            step = 1),
                                sliderInput(inputId = "slider_hypergeom_n",
                                            label = helpText("\\(n\\)"),
                                            min = 0, max = 10,
                                            value = 5,
                                            step = 1)
                              )
                            ),
                            fluidRow(
                              box(plotlyOutput("plotly_hypergeom_PMF", height = 250)),
                              box(plotlyOutput("plotly_hypergeom_CDF", height = 250))
                            ),
                            fluidRow(
                              box(actionButton("hypergeom_random_button",
                                               label = "Generate Random Values!"),
                                  hr(),
                                  verbatimTextOutput("hypergeom_random_output"))
                            )
)

#geometric
tab_item_geometric <-tabItem(tabName = "geometric",
                             h2("Geometric"),
                             fluidRow(
                               box(withMathJax(),
                                   helpText("If \\(X\\) is a random variable with a Geometric distribution, then the PMF
                                        of \\(X\\) is \\( P(X = k) = (1 - p)^k p \\)")),
                               box(
                                 sliderInput(inputId = "slider_geometric_p",
                                             label = helpText("\\(p\\)"),
                                             min = 0, max = 1,
                                             value = 0.5,
                                             step = 0.1)
                               )
                             ),
                             fluidRow(
                               box(plotlyOutput("plotly_geometric_PMF", height = 250)),
                               box(plotlyOutput("plotly_geometric_CDF", height = 250))
                             ),
                             fluidRow(
                               box(actionButton("geometric_random_button",
                                                label = "Generate Random Values!"),
                                   hr(),
                                   verbatimTextOutput("geometric_random_output"))
                             )
)

#negativebinom
tab_item_negativebinom <-tabItem(tabName = "negativebinom",
                             h2("Negative Binomial"),
                             fluidRow(
                               box(withMathJax(),
                                   helpText("If \\(X\\) is a random variable with a Negative Binomial distribution, then the PMF
                                        of \\(X\\) is \\( P(X = k) = \\binom{n + r - 1}{r - 1}p^r (1 - p)^n \\)")),
                               box(
                                 sliderInput(inputId = "slider_negativebinom_r",
                                             label = helpText("\\(r\\)"),
                                             min = 0, max = 10,
                                             value = 2,
                                             step = 1),
                                 sliderInput(inputId = "slider_negativebinom_p",
                                             label = helpText("\\(p\\)"),
                                             min = 0, max = 1,
                                             value = 0.5,
                                             step = 0.1)
                               )
                             ),
                             fluidRow(
                               box(plotlyOutput("plotly_negativebinom_PMF", height = 250)),
                               box(plotlyOutput("plotly_negativebinom_CDF", height = 250))
                             ),
                             fluidRow(
                               box(actionButton("negativebinom_random_button",
                                                label = "Generate Random Values!"),
                                   hr(),
                                   verbatimTextOutput("negativebinom_random_output"))
                             )
)

#geometric
tab_item_poisson <-tabItem(tabName = "poisson",
                             h2("Poisson"),
                             fluidRow(
                               box(withMathJax(),
                                   helpText("If \\(X\\) is a random variable with a Poisson distribution, then the PMF
                                        of \\(X\\) is \\( P(X = k) = ")),
                               box(
                                 sliderInput(inputId = "slider_poisson_lambda",
                                             label = helpText("\\(\\lambda\\)"),
                                             min = 0, max = 9,
                                             value = 2,
                                             step = 1)
                               )
                             ),
                             fluidRow(
                               box(plotlyOutput("plotly_poisson_PMF", height = 250)),
                               box(plotlyOutput("plotly_poisson_CDF", height = 250))
                             ),
                             fluidRow(
                               box(actionButton("poisson_random_button",
                                                label = "Generate Random Values!"),
                                   hr(),
                                   verbatimTextOutput("poisson_random_output"))
                             )
)

#normal
tab_item_normal <-tabItem(tabName = "normal",
                            h2("Normal"),
                            fluidRow(
                              box(withMathJax(),
                                  helpText("If \\(X\\) is a random variable with a Normal distribution, then the PDF
                                        of \\(X\\) is \\( P(X = k) = ")),
                              box(
                                sliderInput(inputId = "slider_normal_mu",
                                            label = helpText("\\(\\mu\\)"),
                                            min = -5, max = 5,
                                            value = 0,
                                            step = 0.5),
                                sliderInput(inputId = "slider_normal_sigma",
                                            label = helpText("\\(\\sigma^2\\)"),
                                            min = 1, max = 3,
                                            value = 1,
                                            step = 0.5)
                              )
                            ),
                            fluidRow(
                              box(plotlyOutput("plotly_normal_PDF", height = 250)),
                              box(plotlyOutput("plotly_normal_CDF", height = 250))
                            ),
                            fluidRow(
                              box(actionButton("normal_random_button",
                                               label = "Generate Random Values!"),
                                  hr(),
                                  verbatimTextOutput("normal_random_output"))
                            )
)

#exponential
tab_item_exponential <-tabItem(tabName = "exponential",
                          h2("Exponential"),
                          fluidRow(
                            box(withMathJax(),
                                helpText("If \\(X\\) is a random variable with an Exponential distribution, then the PDF
                                        of \\(X\\) is \\( P(X = k) = ")),
                            box(
                              sliderInput(inputId = "slider_exponential_lambda",
                                          label = helpText("\\(\\mu\\)"),
                                          min = 0, max = 3,
                                          value = 1,
                                          step = 0.25)
                            )
                          ),
                          fluidRow(
                            box(plotlyOutput("plotly_exponential_PDF", height = 250)),
                            box(plotlyOutput("plotly_exponential_CDF", height = 250))
                          ),
                          fluidRow(
                            box(actionButton("exponential_random_button",
                                             label = "Generate Random Values!"),
                                hr(),
                                verbatimTextOutput("exponential_random_output"))
                          )
)

#beta
tab_item_beta <-tabItem(tabName = "beta",
                         h2("beta"),
                         fluidRow(
                           box(withMathJax(),
                               helpText("If \\(X\\) is a random variable with a Beta distribution, then the PDF
                                        of \\(X\\) is \\( P(X = k) = ")),
                           box(
                             sliderInput(inputId = "slider_beta_a",
                                         label = helpText("\\(a\\)"),
                                         min = 0, max = 5,
                                         value = 2,
                                         step = 0.5),
                             sliderInput(inputId = "slider_beta_b",
                                         label = helpText("\\(b\\)"),
                                         min = 0, max = 5,
                                         value = 2,
                                         step = 0.5)
                           )
                         ),
                         fluidRow(
                           box(plotlyOutput("plotly_beta_PDF", height = 250)),
                           box(plotlyOutput("plotly_beta_CDF", height = 250))
                         ),
                         fluidRow(
                           box(actionButton("beta_random_button",
                                            label = "Generate Random Values!"),
                               hr(),
                               verbatimTextOutput("beta_random_output"))
                         )
)

#gamma
tab_item_gamma <-tabItem(tabName = "gamma",
                          h2("Gamma"),
                          fluidRow(
                            box(withMathJax(),
                                helpText("If \\(X\\) is a random variable with a Gamma distribution, then the PDF
                                        of \\(X\\) is \\( P(X = k) = ")),
                            box(
                              sliderInput(inputId = "slider_gamma_a",
                                          label = helpText("\\(\\mu\\)"),
                                          min = 0, max = 5,
                                          value = 1,
                                          step = 0.5),
                              sliderInput(inputId = "slider_gamma_lambda",
                                          label = helpText("\\(\\sigma^2\\)"),
                                          min = 0, max = 5,
                                          value = 1,
                                          step = 0.5)
                            )
                          ),
                          fluidRow(
                            box(plotlyOutput("plotly_gamma_PDF", height = 250)),
                            box(plotlyOutput("plotly_gamma_CDF", height = 250))
                          ),
                          fluidRow(
                            box(actionButton("gamma_random_button",
                                             label = "Generate Random Values!"),
                                hr(),
                                verbatimTextOutput("gamma_random_output"))
                          )
)

#create body
body <- dashboardBody(tabItems(tab_item_welcome,
                               tab_item_bernoulli,
                               tab_item_binomial,
                               tab_item_hypergeom,
                               tab_item_geometric,
                               tab_item_negativebinom,
                               tab_item_poisson,
                               tab_item_normal,
                               tab_item_exponential,
                               tab_item_beta,
                               tab_item_gamma))

#create ui
ui <- dashboardPage(
  dashboardHeader(title = "Distributions"),
  sidebar,
  body
)

#create server

server <- function(input, output){
  
  #BERNOULLI
  
  #PMF plot
  bernoulli_reactive <- reactive({
    p <- input$slider_bernoulli_p
    
    data.frame(result = c("0", "1"),
               p = c(1 - p, p))
  })
  
  output$plotly_bernoulli <- renderPlotly(
    plot_ly(bernoulli_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~p,
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Bernoulli PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  bernoulli_random_reactive <- reactive({
    input$bernoulli_random_button
    
    p <- input$slider_bernoulli_p
    
    rbinom(n = 5,
           size = 1,
           prob = p)
  })
  
  output$bernoulli_random_output <- renderPrint(bernoulli_random_reactive())
  
  #BINOMIAL
  
  #PMF plot
  
  binomial_reactive <- reactive({
    p_slider <- input$slider_binomial_p
    n_slider <- input$slider_binomial_n
    
    data.frame(result = 0:9) %>%
      mutate(p = dbinom(x = result,
                        size = n_slider,
                        prob = p_slider)) %>%
      mutate(d = pbinom(q = result,
                        size = n_slider,
                        prob = p_slider)) %>%
      mutate(result = as.character(result))
  })
  
  output$plotly_binomial_PMF <- renderPlotly(
    plot_ly(binomial_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~round(p, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Binomial PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_binomial_CDF <- renderPlotly(
    plot_ly(binomial_reactive(),
            y = ~d,
            x = ~result,
            type = "bar",
            text = ~round(d, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Binomial CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  binomial_random_reactive <- reactive({
    input$binomial_random_button
    
    p_slider <- input$slider_binomial_p
    
    n_slider <- input$slider_binomial_n
    
    rbinom(n = 5,
           size = n_slider,
           prob = p_slider)
  })
  
  output$binomial_random_output <- renderPrint(binomial_random_reactive())
  
  
  #HYPERGEOMETRIC
  
  #PMF plot
  
  hypergeom_reactive <- reactive({
    w_slider <- input$slider_hypergeom_w
    b_slider <- input$slider_hypergeom_b
    n_slider <- input$slider_hypergeom_n
    
    data.frame(result = 0:9) %>%
      mutate(p = dhyper(x = result,
                        m = w_slider,
                        n = b_slider,
                        k = n_slider)) %>%
      mutate(d = phyper(q = result,
                        m = w_slider,
                        n = b_slider,
                        k = n_slider)) %>%
      mutate(result = as.character(result))
  })
  
  output$plotly_hypergeom_PMF <- renderPlotly(
    plot_ly(hypergeom_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~round(p, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Hypergeometric PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_hypergeom_CDF <- renderPlotly(
    plot_ly(hypergeom_reactive(),
            y = ~d,
            x = ~result,
            type = "bar",
            text = ~round(d, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Hypergeometric CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  hypergeom_random_reactive <- reactive({
    input$hypergeom_random_button
    
    w_slider <- input$slider_hypergeom_w
    b_slider <- input$slider_hypergeom_b
    n_slider <- input$slider_hypergeom_n
    
    rhyper(nn = 5,
           m = w_slider,
           n = b_slider,
           k = n_slider)
  })
  
  output$hypergeom_random_output <- renderPrint(hypergeom_random_reactive())
  
  #GEOMETRIC
  
  #PMF plot
  
  geometric_reactive <- reactive({
    p_slider <- input$slider_geometric_p
    
    data.frame(result = 0:9) %>%
      mutate(p = dgeom(x = result,
                       prob = p_slider)) %>%
      mutate(d = pgeom(q = result,
                       prob = p_slider)) %>%
      mutate(result = as.character(result))
  })
  
  output$plotly_geometric_PMF <- renderPlotly(
    plot_ly(geometric_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~round(p, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Geometric PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_geometric_CDF <- renderPlotly(
    plot_ly(geometric_reactive(),
            y = ~d,
            x = ~result,
            type = "bar",
            text = ~round(d, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Geometric CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  geometric_random_reactive <- reactive({
    input$geometric_random_button
    
    p_slider <- input$slider_geometric_p
    
    rgeom(n = 5,
          prob = p_slider)
  })
  
  output$geometric_random_output <- renderPrint(geometric_random_reactive())
  
  #NEGATIVE BINOMIAL
  
  #PMF plot
  
  negativebinom_reactive <- reactive({
    r_slider <- input$slider_negativebinom_r
    p_slider <- input$slider_negativebinom_p
    
    data.frame(result = 0:9) %>%
      mutate(p = dnbinom(x = result,
                         size = r_slider,
                         prob = p_slider)) %>%
      mutate(d = pnbinom(q = result,
                         size = r_slider,
                         prob = p_slider)) %>%
      mutate(result = as.character(result))
  })
  
  output$plotly_negativebinom_PMF <- renderPlotly(
    plot_ly(negativebinom_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~round(p, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Negative Binomial PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_negativebinom_CDF <- renderPlotly(
    plot_ly(negativebinom_reactive(),
            y = ~d,
            x = ~result,
            type = "bar",
            text = ~round(d, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Negative Binomial CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  negativebinom_random_reactive <- reactive({
    input$negativebinom_random_button
    
    r_slider <- input$slider_negativebinom_r
    p_slider <- input$slider_negativebinom_p
    
    rnbinom(n = 5,
            prob = p_slider,
            size = r_slider)
  })
  
  output$negativebinom_random_output <- renderPrint(negativebinom_random_reactive())
  
  #POISSON
  
  #PMF plot
  
  poisson_reactive <- reactive({
    lambda_slider <- input$slider_poisson_lambda
    
    data.frame(result = 0:9) %>%
      mutate(p = dpois(x = result,
                       lambda = lambda_slider)) %>%
      mutate(d = ppois(q = result,
                       lambda = lambda_slider)) %>%
      mutate(result = as.character(result))
  })
  
  output$plotly_poisson_PMF <- renderPlotly(
    plot_ly(poisson_reactive(),
            y = ~p,
            x = ~result,
            type = "bar",
            text = ~round(p, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Poisson PMF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_poisson_CDF <- renderPlotly(
    plot_ly(poisson_reactive(),
            y = ~d,
            x = ~result,
            type = "bar",
            text = ~round(d, digits = 2),
            textposition = "outside",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Poisson CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  poisson_random_reactive <- reactive({
    input$poisson_random_button
    
    lambda_slider <- input$slider_poisson_lambda
    
    rpois(n = 5,
          lambda = lambda_slider)
  })
  
  output$poisson_random_output <- renderPrint(poisson_random_reactive())
  
  #NORMAL
  
  #PDF plot
  
  normal_reactive <- reactive({
    mu_slider <- input$slider_normal_mu
    sigma_slider <- input$slider_normal_sigma
    
    data.frame(result = seq(from = -6, to = 6, by = 0.01)) %>%
      mutate(p = dnorm(x = result,
                       mean = mu_slider,
                       sd = sqrt(sigma_slider))) %>%
      mutate(d = pnorm(q = result,
                       mean = mu_slider,
                       sd = sqrt(sigma_slider)))
  })
  
  output$plotly_normal_PDF <- renderPlotly(
    plot_ly(normal_reactive(),
            y = ~p,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Normal PDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_normal_CDF <- renderPlotly(
    plot_ly(normal_reactive(),
            y = ~d,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Normal CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  normal_random_reactive <- reactive({
    input$normal_random_button
    
    mu_slider <- input$slider_normal_mu
    
    sigma_slider <- input$slider_normal_sigma
    
    rnorm(n = 5,
          mean = mu_slider,
          sd = sqrt(sigma_slider))
  })
  
  output$normal_random_output <- renderPrint(round(normal_random_reactive(), 
                                                   digits = 2))
  
  
  #EXPONENTIAL
  
  #PDF plot
  
  exponential_reactive <- reactive({
    lambda_slider <- input$slider_exponential_lambda
    
    data.frame(result = seq(from = 0, to = 9, by = 0.01)) %>%
      mutate(p = dexp(x = result,
                      rate = lambda_slider)) %>%
      mutate(d = pexp(q = result,
                      rate = lambda_slider))
  })
  
  output$plotly_exponential_PDF <- renderPlotly(
    plot_ly(exponential_reactive(),
            y = ~p,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Exponential PDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_exponential_CDF <- renderPlotly(
    plot_ly(exponential_reactive(),
            y = ~d,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Exponential CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  exponential_random_reactive <- reactive({
    input$exponential_random_button
    
    lambda_slider <- input$slider_exponential_lambda
    
    rexp(n = 5,
         rate = lambda_slider)
  })
  
  output$exponential_random_output <- renderPrint(round(exponential_random_reactive(),
                                                       digits = 2))
  
  #BETA
  
  #PDF plot
  
  beta_reactive <- reactive({
    a_slider <- input$slider_beta_a
    b_slider <- input$slider_beta_b
    
    data.frame(result = seq(from = 0, to = 1, by = 0.01)) %>%
      mutate(p = dbeta(x = result,
                       shape1 = a_slider,
                       shape2 = b_slider)) %>%
      mutate(d = pbeta(q = result,
                       shape1 = a_slider,
                       shape2 = b_slider))
  })
  
  output$plotly_beta_PDF <- renderPlotly(
    plot_ly(beta_reactive(),
            y = ~p,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Beta PDF",
        yaxis = list(range = c(0, 5),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_beta_CDF <- renderPlotly(
    plot_ly(beta_reactive(),
            y = ~d,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Beta CDF",
        yaxis = list(range = c(0, 1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  beta_random_reactive <- reactive({
    input$beta_random_button
    
    a_slider <- input$slider_beta_a
    b_slider <- input$slider_beta_b
    
    rbeta(n = 5,
          shape1 = a_slider,
          shape2 = b_slider)
  })
  
  output$beta_random_output <- renderPrint(round(beta_random_reactive(), 
                                                 digits = 2))
  
  #GAMMA
  
  #PDF plot
  
  gamma_reactive <- reactive({
    a_slider <- input$slider_gamma_a
    lambda_slider <- input$slider_gamma_lambda
    
    data.frame(result = seq(from = 0, to = 9, by = 0.01)) %>%
      mutate(p = dgamma(x = result,
                        shape = a_slider,
                        scale = lambda_slider)) %>%
      mutate(d = pgamma(q = result,
                        shape = a_slider,
                        scale = lambda_slider))
  })
  
  output$plotly_gamma_PDF <- renderPlotly(
    plot_ly(gamma_reactive(),
            y = ~p,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Gamma PDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #CDF plot
  
  output$plotly_gamma_CDF <- renderPlotly(
    plot_ly(gamma_reactive(),
            y = ~d,
            x = ~result,
            type = "scatter",
            mode = "lines",
            color = I("lightsteelblue")) %>%
      layout(
        title = "Gamma CDF",
        yaxis = list(range = c(0, 1.1),
                     title = ""),
        xaxis = list(title = "")
      )
  )
  
  #random values
  
  gamma_random_reactive <- reactive({
    input$gamma_random_button
    
    a_slider <- input$slider_gamma_a
    lambda_slider <- input$slider_gamma_lambda
    
    rgamma(n = 5,
           shape = a_slider,
           scale = lambda_slider)
  })
  
  output$gamma_random_output <- renderPrint(round(gamma_random_reactive(), 
                                                   digits = 2))
}


shinyApp(ui = ui, server = server)