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
                          icon = icon("smile-beam"))

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
                         icon = icon("bus"))
 
#create sidebar
sidebar <- dashboardSidebar(sidebarMenu(menu_welcome,
                                        menu_bernoulli,
                                        menu_binomial,
                                        menu_hypergeom,
                                        menu_geometric,
                                        menu_negativebinom,
                                        menu_poisson))

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

#create body
body <- dashboardBody(tabItems(tab_item_welcome,
                               tab_item_bernoulli,
                               tab_item_binomial,
                               tab_item_hypergeom,
                               tab_item_geometric,
                               tab_item_negativebinom))

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
        yaxis = list(range = c(0, 1),
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
        yaxis = list(range = c(0, 1),
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
        yaxis = list(range = c(0, 2),
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
        yaxis = list(range = c(0, 1),
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
        yaxis = list(range = c(0, 2),
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
        yaxis = list(range = c(0, 1),
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
        yaxis = list(range = c(0, 2),
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
        yaxis = list(range = c(0, 1),
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
        yaxis = list(range = c(0, 2),
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
}


shinyApp(ui = ui, server = server)