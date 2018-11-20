library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(img(src = 'logo.png'),
    sidebarLayout(
        sidebarPanel(
            sliderInput("priceInput", "Price", 0, 400, c(25, 40), pre = "$"),
            radioButtons(
                "typeInput",
                "Product type",
                choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                selected = "WINE"
            ),
            uiOutput("countryOutput"),
            checkboxInput("sortInput", "Sort by Price", value = FALSE)
        ),
        mainPanel(plotOutput("coolplot"),
                  br(), br(),
                  tableOutput("results"))
    ))

server <- function(input, output) {
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country",
                    c("ALL", sort(unique(
                        bcl$Country
                    ))),
                    selected = "CANADA")
    })
    
    filtered <- reactive({
        if (is.null(input$countryInput)) {
            return(NULL)
        # if filtering by country
        } else if (input$countryInput %in% unique(bcl$Country))  {
            if (input$sortInput) {
                bcl %>%
                    filter(
                        Price >= input$priceInput[1],
                        Price <= input$priceInput[2],
                        Type == input$typeInput,
                        Country == input$countryInput
                    ) %>%
                    arrange(Price)
            } else {
                bcl %>%
                    filter(
                        Price >= input$priceInput[1],
                        Price <= input$priceInput[2],
                        Type == input$typeInput,
                        Country == input$countryInput
                    )
            }
        # if not filtering by country
        } else {
            if (input$sortInput) {
                bcl %>% filter(
                    Price >= input$priceInput[1],
                    Price <= input$priceInput[2],
                    Type == input$typeInput
                ) %>%
                    arrange(Price)
            } else {
                bcl %>%
                    filter(
                        Price >= input$priceInput[1],
                        Price <= input$priceInput[2],
                        Type == input$typeInput
                    )
            }
        }
    })
    
    # output plot
    output$coolplot <- renderPlot({
        if (is.null(filtered())) {
            return()
        }
        ggplot(filtered(), aes(Price)) +
            geom_histogram(bins = (input$priceInput[2] - input$priceInput[1]))
        
    })
    
    output$results <- renderTable({
        filtered()
    })
}

shinyApp(ui = ui, server = server)
