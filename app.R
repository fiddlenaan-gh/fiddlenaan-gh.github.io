library(shiny)
library(dplyr)

## Reference data (CSVs must sit next to app.R)
PlasmidDF <- read.csv("Plasmid.csv")
ModuleDF  <- read.csv("Module.csv")

ui <- fluidPage(
  titlePanel("Mutaplasmid Calc"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ModuleIn", "Module", choices = sort(unique(ModuleDF$Module))),
      selectInput("ClassIn",  "Class",  choices = sort(unique(PlasmidDF$Class))),
      selectInput("SizeIn",   "Size",   choices = sort(unique(ModuleDF$Size))),
      selectInput("TierIn",   "Tier",   choices = sort(unique(ModuleDF$Tier))),
      numericInput("OpenCapIn",  "Open CPU",       value = 0),
      numericInput("OpenPGIn",   "Open Powergrid", value = 0),
      numericInput("QuantityIn", "Quantity",       value = 1, min = 1),
      numericInput("Comparative", "Comparative value (optional)", value = NA)
    ),
    mainPanel(
      h4("Final odds"),
      textOutput("finalOdds"),
      h4("Roll breakdown"),
      tableOutput("resultsTable")
    )
  )
)

server <- function(input, output, session) {
  
  calc <- reactive({
    req(input$ModuleIn, input$ClassIn, input$SizeIn, input$TierIn,
        input$OpenCapIn, input$OpenPGIn, input$QuantityIn)
    
    ## Filter lines that match input criteria
    ## (as.character() on both sides so numeric CSV columns still match selectInput text)
    RefPlasmid <- PlasmidDF %>%
      filter(Module == input$ModuleIn,
             as.character(Class) == input$ClassIn,
             Category != 0)
    RefModule <- ModuleDF %>%
      filter(Module == input$ModuleIn,
             as.character(Size) == input$SizeIn,
             as.character(Tier) == input$TierIn,
             Category != 0)
    
    ## Error prevention for filter failure (shown in the app instead of erroring)
    validate(need(nrow(RefPlasmid) > 0 && nrow(RefModule) > 0,
                  "No matching rows found for the selected parameters."))
    
    CombineDF <- inner_join(RefPlasmid, RefModule, by = "RollName",
                            suffix = c(".Plasmid", ".Module"))
    
    ## Columns shared by both tables get suffixed by the join. Recreate plain
    ## names from whichever version exists (Plasmid copy preferred, matching
    ## the old Category.x behavior).
    unsuffix <- function(df, nm) {
      for (cand in c(nm, paste0(nm, ".Plasmid"), paste0(nm, ".Module"))) {
        if (cand %in% names(df)) return(df[[cand]])
      }
      stop("Column '", nm, "' not found after join. Columns are: ",
           paste(names(df), collapse = ", "))
    }
    CombineDF$BaseValue <- unsuffix(CombineDF, "BaseValue")
    CombineDF$Category  <- unsuffix(CombineDF, "Category")
    CombineDF$MinRoll   <- unsuffix(CombineDF, "MinRoll")
    CombineDF$MaxRoll   <- unsuffix(CombineDF, "MaxRoll")
    
    ## Translate module rolls to numerical values
    CombineDF$MinRollValue <- CombineDF$BaseValue * (1 + CombineDF$MinRoll)
    CombineDF$MaxRollValue <- CombineDF$BaseValue * (1 + CombineDF$MaxRoll)
    CombineDF$MinDiff   <- CombineDF$MinRollValue - CombineDF$BaseValue
    CombineDF$MaxDiff   <- CombineDF$BaseValue - CombineDF$MaxRollValue
    CombineDF$RollRange <- CombineDF$MaxRollValue - CombineDF$MinRollValue
    
    ## Calculate expected values
    CombineDF <- CombineDF %>%
      mutate(ExpectedValue = (MinRollValue + MaxRollValue) / 2)
    
    ## Identify gating criteria
    GateCap <- input$OpenCapIn / input$QuantityIn
    GatePG  <- input$OpenPGIn  / input$QuantityIn
    
    ## Update minimum values if gating is more restrictive than possible rolls
    CombineDF <- CombineDF %>%
      mutate(
        MinRollValue = case_when(
          RollName == "CPU Usage"       & GateCap < MinDiff ~ BaseValue + GateCap,
          RollName == "Powergrid Usage" & GatePG  < MinDiff ~ BaseValue + GatePG,
          TRUE ~ MinRollValue
        )
      )
    
    ## Replace minimum roll for priority mods
    ## (empty numericInput returns NA, not NULL)
    if (!is.na(input$Comparative)) {
      comparative <- input$Comparative
      CombineDF <- CombineDF %>%
        mutate(MinRollValue = case_when(
          Category == 2 ~ comparative,
          TRUE ~ MinRollValue
        ))
    } else {
      CombineDF <- CombineDF %>%
        mutate(MinRollValue = case_when(
          Category == 2 ~ BaseValue,
          TRUE ~ MinRollValue
        ))
    }
    
    ## Calculate odds for each module
    CombineDF <- CombineDF %>%
      mutate(RollChance = (MaxRollValue - MinRollValue) / RollRange)
    
    ## Results data frame and final odds
    ResultsDF <- CombineDF %>%
      select(RollName, RollChance, ExpectedValue)
    
    list(
      ResultsTable = ResultsDF,
      FinalOdds    = prod(ResultsDF$RollChance)
    )
  })
  
  output$finalOdds <- renderText({
    sprintf("%.4f%%", calc()$FinalOdds * 100)
  })
  
  output$resultsTable <- renderTable({
    calc()$ResultsTable
  })
}

shinyApp(ui, server)