# Pakete laden -----------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinyWidgets)

# Arbeitsbereich laden ---------------------------------------------------------

load("data/de.RData")
load("data/en.RData")

# UI für Anwendung definieren --------------------------------------------------

ui <- fluidPage(
  
  HTML("
       <style>a:hover {text-decoration: none;}</style>
       "), 
  
  tags$head(
    
    # CSS einbinden ------------------------------------------------------------
    
    includeCSS("css/style.css"),
    includeCSS("css/colors/sky.css")
  ),
  
  # Header ---------------------------------------------------------------------
  
  tags$header(
    
    tags$br(),
    tags$img(src = "logo.png", 
             alt = "FoodExplorer by NXTLVLETE", 
             height = 56),
    tags$br(),
    tags$br()
  ),
  
  # Registerkarten-Layout ------------------------------------------------------
  
  tabsetPanel(
    
    # Erste Registerkarte ------------------------------------------------------
    
    tabPanel(
      title = "English",
      setBackgroundImage(src = "bg.webp", 
                         shinydashboard = FALSE),
             
             # Titel der Anwendung ---------------------------------------------
             
             titlePanel("Find your perfect foods"),
             
             # Layout mit Seitenleiste -----------------------------------------
             
             sidebarLayout(
               sidebarPanel(
                 p("Select the category and two features of the food you'd like 
                   to see in detail and press 'Show'."),
                 br(),
                 selectInput(inputId = "selMainCat", 
                             label = "Select main category", 
                             choices = levels(food_02$MainCategory), 
                             selected = "Meat, fish and eggs"),
                 uiOutput(outputId = "uiCat"),
                 selectInput(inputId = "selX", 
                             label = "Feature 1", 
                             choices = varFeat, 
                             selected = "Protein (g)"),
                 selectInput(inputId = "selY", 
                             label = "Feature 2", 
                             choices = varFeat, 
                             selected = "Energy (kcal)"),
                 br(),
                 actionButton(class = "btn btn-sky rounded", 
                              inputId = "btnGo", 
                              label = "Show")
               ),
               
               # Diagramme anzeigen --------------------------------------------
               
               mainPanel(
                 htmlOutput(outputId = "htmlNote"),
                 br(),
                 plotOutput(outputId = "plotScatterChart", 
                            hover = "brushScatterChart"
                 # brush = brushOpts(id = "brushScatterChart", fill = "#87ceeb", stroke = "#a6979c")
                            ),
                 br(),
                 tableOutput(outputId = "tblScatterChart"),
                 br(),
                 plotOutput(outputId = "plotBarChart")
               )
             )
    ),
    
    # Zweite Registerkarte -----------------------------------------------------
    
    tabPanel(title = "Deutsch",
             
             # Titel der Anwendung ---------------------------------------------
             
             titlePanel("Finde deine perfekten Lebensmittel"),
             
             # Layout mit Seitenleiste -----------------------------------------
             
             sidebarLayout(
               sidebarPanel(
                 p("Wähle die Kategorie und zwei Merkmale der Lebensmittel aus, 
                   die du näher betrachten möchtest, und klicke auf 'Anzeigen'."),
                 br(),
                 selectInput(inputId = "selMainCat_de", 
                             label = "Hauptkategorie auswählen", 
                             choices = levels(food_02_de$Hauptkategorie), 
                             selected = "Fleisch, Fisch und Eier"),
                 uiOutput(outputId = "uiCat_de"),
                 selectInput(inputId = "selX_de", 
                             label = "Merkmal 1", 
                             choices = varMerkmal, 
                             selected = "Protein (g)"),
                 selectInput(inputId = "selY_de", 
                             label = "Merkmal 2", 
                             choices = varMerkmal, 
                             selected = "Energie (kcal)"),
                 br(),
                 actionButton(class = "btn btn-sky rounded", 
                              inputId = "btnGo_de", 
                              label = "Anzeigen")
               ),
               
               # Diagramme anzeigen --------------------------------------------
               
               mainPanel(
                 htmlOutput(outputId = "htmlNote_de"),
                 br(),
                 plotOutput(outputId = "plotScatterChart_de", 
                            hover = "brushScatterChart_de"
                 # brush = brushOpts(id = "brushScatterChart_de", fill = "#87ceeb", stroke = "#a6979c")
                            ),
                 br(),
                 tableOutput(outputId = "tblScatterChart_de"),
                 br(),
                 plotOutput(outputId = "plotBarChart_de")
               )
             )
    ),
    
    # Dritte Registerkarte -----------------------------------------------------
    
    tabPanel(title = "Sources",
             
             fluidRow(
               
               column(width = 5,
                      withTags({
                        div(
                          h2("Sources"),
                          p("Data source:", 
                            a(href = "https://naehrwertdaten.ch/en/", 
                              class = "hover link-primary", "naehrwertdaten.ch/en")),
                          p("Getting started with Shiny:", 
                            a(href = "https://shiny.posit.co", 
                              class = "hover link-primary", "shiny.posit.co")),
                          br(),
                          h3("Interpretation of food composition data"),
    p("All data refer to 100 g edible portion for solid foods or to 100 ml for 
      liquid foods. Inedible fractions such as stones, kernels, bones, etc. are 
      not taken into account. Unless otherwise stated, all data refer to fresh, 
      uncooked, unprocessed foods."),
                          br(),
                          h3("Changes to nutrients on cooking"),
    p("Cooking changes the nutritional content of a foodstuff. The degree of change 
      depends on factors such as the nature of the foodstuff, cooking technique, 
      duration of cooking and cooking temperature. Different weight changes and 
      modifications to the nutrient can occur, depending on the cooking technique."),
                          br(),
                          br()
                        )
                      })
               ),
               
               column(width = 1),
               
               column(width = 5,
                      withTags({
                        div(
                          h2("Quellenangaben"),
                          p("Datenquelle:",
                            a(href = "https://naehrwertdaten.ch/de/", 
                              class = "hover link-primary", "naehrwertdaten.ch/de")),
                          p("Erste Schritte mit Shiny (in Englisch):", 
                            a(href = "https://shiny.posit.co", 
                              class = "hover link-primary", "shiny.posit.co")),
                          br(),
                          h3("Interpretation von Nährwertangaben"),
    p("Sämtliche Angaben beziehen sich jeweils auf 100 g essbaren Anteil bei 
      festen resp. auf 100 ml bei flüssigen Lebensmitteln. Nicht essbare Anteile 
      wie Steine, Kerne, Knochen usw. werden nicht eingerechnet. Sofern nicht 
      anders vermerkt, beziehen sich alle Angaben auf frische, ungekochte, 
      unverarbeitete Lebensmittel."),
                          br(),
                          h3("Nährwertveränderungen durch Kochen"),
    p("Kochen verändert den Nährstoffgehalt eines Lebensmittels. Das Ausmass der 
      Veränderung hängt dabei unter anderem von der Art des Lebensmittels, der 
      Kochmethode, der Dauer des Kochvorgangs und der Temperatur beim Kochen ab. 
      Je nach Kochmethode kann es dabei zu unterschiedlichen Gewichts- und 
      Nährstoffveränderungen kommen.")
                        )
                      })
               ),
               
               column(width = 1)
               
             )
    )
  ),
  
  # Footer ---------------------------------------------------------------------
  
  fluidRow(
    
    column(
      width = 12, 
      withTags({
        div(
          footer(
            div(class = "pt-10 pb-7", 
                div(class = "d-md-flex align-items-center 
                    justify-content-between", 
                    p(class = "mb-2 mb-lg-0", 
                      "Shiny app developed by ", 
                      a(class = "a-sky", 
                        href="https://www.datavisual.ch/index.html", 
                        class = "hover link-primary", "DataVisual."), 
                      "All rights reserved."))))
        )
      })
    )
  )
)

# Serverlogik definieren -------------------------------------------------------

server <- function(input, output) {
  
  # Vektor Kategorieauswahl in Abhängigkeit Hauptkategorie erstellen -----------
  
  vCat <- reactive({
    if (input$selMainCat == "Beverages") {
      levels(varBeverage$Category)
    } else if (input$selMainCat == "Cakes, sweets and snacks") {
      levels(varCake$Category)
    } else if (input$selMainCat == "Dishes and fast food") {
      levels(varDish$Category)
    } else if (input$selMainCat == "Fruit and vegetables") {
      levels(varFV$Category)
    } else if (input$selMainCat == "Grains and potatoes") {
      levels(varGrain$Category)
    } else if (input$selMainCat == "Meat, fish and eggs") {
      levels(varMeat$Category)
    } else {
      levels(varMilk$Category)
    }
  })
  
  vCat_de <- reactive({
    if (input$selMainCat_de == "Getränke") {
      levels(varGetraenk$Kategorie)
    } else if (input$selMainCat_de == "Kuchen, Süssigkeiten und Snacks") {
      levels(varKuchen$Kategorie)
    } else if (input$selMainCat_de == "Gerichte und Fast Food") {
      levels(varGericht$Kategorie)
    } else if (input$selMainCat_de == "Obst und Gemüse") {
      levels(varOuG$Kategorie)
    } else if (input$selMainCat_de == "Getreideprodukte und Kartoffeln") {
      levels(varGetreide$Kategorie)
    } else if (input$selMainCat_de == "Fleisch, Fisch und Eier") {
      levels(varFleisch$Kategorie)
    } else {
      levels(varMilch$Kategorie)
    }
  })
  
  # Dynamisches Kategoriemenü erstellen ----------------------------------------
  
  output$uiCat <- renderUI({
    selectInput(inputId = "selCat", label = "Select category", choices = vCat())
  })
  
  output$uiCat_de <- renderUI({
    selectInput(inputId = "selCat_de", label = "Kategorie auswählen", 
                choices = vCat_de())
  })
  
  # Text mit Hinweis erstellen -------------------------------------------------
  
  varNote <- eventReactive(input$btnGo, {
    p("Hover or tap on the points you are interested in. This will give you 
      detailed information about the food.", br(), br(), 
      em("Note: Depending on the selected features, several foods can be displayed 
         via one diagram point."))
  })
  
  varNote_de <- eventReactive(input$btnGo_de, {
    p("Klicke oder tippe auf die Punkte, die dich interessieren. So erhältst 
      du detaillierte Informationen über die Lebensmittel.", br(), br(), 
      em("Hinweis: Je nach ausgewählten Merkmalen können mehrere Lebensmittel über 
      einen Diagrammpunkt dargestellt werden."))
  })
  
  # Text mit Hinweis ausgeben --------------------------------------------------
  
  output$htmlNote <- renderPrint({
    varNote()
  })
  
  output$htmlNote_de <- renderPrint({
    varNote_de()
  })
  
  # Teilmenge für Streudiagramm bilden -----------------------------------------
  
  varFoodSubset <- eventReactive(input$btnGo, {
    food_02 %>% 
      filter(Category == input$selCat)
  })
  
  varFoodSubset_de <- eventReactive(input$btnGo_de, {
    food_02_de %>% 
      filter(Kategorie == input$selCat_de)
  })
  
  # Streudiagramm erstellen ----------------------------------------------------
  
  varScatterChart <- eventReactive(input$btnGo, {
    ggplot(varFoodSubset(), mapping = aes(x = .data[[input$selX]], 
                                          y = .data[[input$selY]])) +
      geom_point(color = "#87ceeb", size = 3) +
      labs(title = paste0("Plot of ", sapply(strsplit(x = input$selX, " [\\(]"), '[', 1), 
                          " and ", sapply(strsplit(x = input$selY, " [\\(]"), '[', 1)), 
           caption = "Created by DataVisual.ch") +
      theme_classic() +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", 
                                      size = 24),
            plot.caption = element_text(family = "Helvetica", size = 11),
            axis.title = element_text(family = "Helvetica", size = 14),
            axis.text = element_text(family = "Helvetica", 
                                     color = "black", size = 14))
  })
  
  varScatterChart_de <- eventReactive(input$btnGo_de, {
    ggplot(varFoodSubset_de(), mapping = aes(x = .data[[input$selX_de]], 
                                             y = .data[[input$selY_de]])) +
      geom_point(color = "#87ceeb", size = 3) +
      labs(title = paste0(sapply(strsplit(x = input$selX_de, " [\\(]"), '[', 1), 
                          "/", sapply(strsplit(x = input$selY_de, " [\\(]"), '[', 1), 
                          "-Diagramm"), 
           caption = "Erstellt von DataVisual.ch") +
      theme_classic() +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", 
                                      size = 24),
            plot.caption = element_text(family = "Helvetica", size = 11),
            axis.title = element_text(family = "Helvetica", size = 14),
            axis.text = element_text(family = "Helvetica", 
                                     color = "black", size = 14))
  })
  
  # Streudiagramm ausgeben -----------------------------------------------------
  
  output$plotScatterChart <- renderPlot({
    varScatterChart()
  })
  
  output$plotScatterChart_de <- renderPlot({
    varScatterChart_de()
  })
  
  # Tabelle erstellen ----------------------------------------------------------
  
  varCatExcl <- reactive({
    c("Fatty acids (g)", "Cholesterol (mg)", "Dietary fibres (g)", "Vitamin C (mg)", 
      "Calcium (mg)", "Magnesium (mg)", "Iron (mg)")
  })
  
  tblPoints <- eventReactive(input$brushScatterChart, {
    
    if(input$selX %in% varCatExcl() | 
       input$selY %in% varCatExcl()) {
      validate(
  "\nWe're sorry. There is no table available for this combination of features in the free version.\n\n")
    }
    
    # brushedPoints(df = varFoodSubset()[, c(2, 4:6, 9:10, 12)], 
                  # brush = input$brushScatterChart, 
                  # xvar = input$selX, yvar = input$selY)
    
    nearPoints(df = varFoodSubset()[, c(2, 4:6, 9:10, 12)], 
               coordinfo = input$brushScatterChart, 
               xvar = input$selX, yvar = input$selY)
  })
  
  varCatExcl_de <- reactive({
    c("Fettsäuren (g)", "Cholesterin (mg)", "Nahrungsfasern (g)", 
      "Vitamin C (mg)", "Calcium (mg)", "Magnesium (mg)", 
      "Eisen (mg)")
  })
  
  tblPoints_de <- eventReactive(input$brushScatterChart_de, {
    
    if(input$selX_de %in% varCatExcl_de() | 
       input$selY_de %in% varCatExcl_de()) {
      validate(
  "\nEntschuldige bitte, für diese Merkmalskombination gibt es in der kostenlosen Version keine Tabelle.\n\n")
    }
    
    # brushedPoints(df = varFoodSubset_de()[, c(2, 4:6, 9:10, 12)], 
                  # brush = input$brushScatterChart_de, 
                  # xvar = input$selX_de, yvar = input$selY_de)
    
    nearPoints(df = varFoodSubset_de()[, c(2, 4:6, 9:10, 12)], 
               coordinfo = input$brushScatterChart_de, 
               xvar = input$selX_de, yvar = input$selY_de)
  })
  
  # Tabelle ausgeben -----------------------------------------------------------
  
  output$tblScatterChart <- renderTable({
    tblPoints()
  })
  
  output$tblScatterChart_de <- renderTable({
    tblPoints_de()
  })
  
  # Balkendiagramm erstellen ---------------------------------------------------
  
  varBarChart <- eventReactive(input$btnGo, {
    food_02 %>% 
      filter(!is.na(MainCategory)) %>% 
      group_by(MainCategory) %>% 
      summarise(varMeanFeat = mean(.data[[input$selX]], na.rm = TRUE)) %>% 
      ggplot(mapping = aes(x = fct_reorder(MainCategory, .x = varMeanFeat, 
                                           .fun = mean, .na_rm = TRUE, 
                                           .desc = FALSE), 
                           y = varMeanFeat)) +
      geom_col(fill = "#87ceeb") +
      coord_flip() +
      labs(title = paste0("Chart of ", sapply(strsplit(x = input$selX, " [\\(]"), '[', 1)), 
           subtitle = "Average value of the feature", 
           caption = "Created by DataVisual.ch",
           x = "Food category", 
           y = "Feature 1") +
      theme_classic() +
      geom_text(mapping = aes(label = round(varMeanFeat, digits = 2)), hjust = 0.45, 
                family = "Helvetica", color = "black", size = 5) +
      geom_hline(yintercept = 0, linetype = "solid", color = "#a6979c") +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", 
                                      size = 24),
            plot.subtitle = element_text(family = "Helvetica", size = 14),
            plot.caption = element_text(family = "Helvetica", size = 11),
            axis.title = element_text(family = "Helvetica", size = 14),
            axis.text = element_text(family = "Helvetica", 
                                     color = "black", size = 11))
  })
  
  varBarChart_de <- eventReactive(input$btnGo_de, {
    food_02_de %>% 
      filter(!is.na(Hauptkategorie)) %>% 
      group_by(Hauptkategorie) %>% 
      summarise(varMeanFeat_de = mean(.data[[input$selX_de]], na.rm = TRUE)) %>% 
      ggplot(mapping = aes(x = fct_reorder(Hauptkategorie, .x = varMeanFeat_de, 
                                           .fun = mean, .na_rm = TRUE, 
                                           .desc = FALSE), 
                           y = varMeanFeat_de)) +
      geom_col(fill = "#87ceeb") +
      coord_flip() +
      labs(title = paste0(sapply(strsplit(x = input$selX_de, " [\\(]"), '[', 1), 
                          "-Diagramm"), 
           subtitle = "Durchschnittswert des Merkmals", 
           caption = "Erstellt von DataVisual.ch",
           x = "Lebensmittelgruppe", 
           y = "Merkmal 1") +
      theme_classic() +
      geom_text(mapping = aes(label = round(varMeanFeat_de, digits = 2)), hjust = 0.45, 
                family = "Helvetica", color = "black", size = 5) +
      geom_hline(yintercept = 0, linetype = "solid", color = "#a6979c") +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", 
                                      size = 24),
            plot.subtitle = element_text(family = "Helvetica", size = 14),
            plot.caption = element_text(family = "Helvetica", size = 11),
            axis.title = element_text(family = "Helvetica", size = 14),
            axis.text = element_text(family = "Helvetica", 
                                     color = "black", size = 11))
  })
    
  # Balkendiagramm ausgeben ----------------------------------------------------
  
  output$plotBarChart <- renderPlot({
    varBarChart()
  })
  
  output$plotBarChart_de <- renderPlot({
    varBarChart_de()
  })
}

# Anwendung ausführen ----------------------------------------------------------

shinyApp(ui = ui, server = server)
