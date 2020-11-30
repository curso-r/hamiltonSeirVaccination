#' original_version UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_original_version_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        shinyjs::useShinyjs(),
        br(),
        fluidRow(
          column(
            width = 3,
            sliderInput(ns("vacc_Y"), "Daily number of vaccinations for under 65s", 0, 50000, 2000, step = 100),
            
            sliderInput(ns("vacc_O"), "Daily number of vaccinations for over 65s", 0, 50000, 18000, step = 100),
            
            sliderInput(ns("vacc_ef"), "Vaccine effectiveness (%)", 50, 100, 90, step = 1),
            
            sliderInput(ns("R0"), "Average number of infections from each infected person (R number) for under 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(ns("R0_1"), "Average number of infections from each infected person (R number) for over 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(inputId = ns("R0_O_Y"),
                        label = "Average number of infections passed between under and over 65s per infected person (Cross R number)",
                        0, 10, 0.3, step=0.1),
            
            actionButton(inputId = ns("button"), label = "show extra options"),
  
            
            numericInput(inputId = ns("exp"),
                         label = "Number of asymptomatic spreaders under 65 at start date",
                         value = 2000),
            
            numericInput(inputId = ns("inf"),
                         label = "Number of symptomatic spreaders under 65 at start date",
                         value = 2000),
            
            numericInput(inputId = ns("exp2"),
                         label = "Number of asymptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("inf2"),
                         label = "Number of symptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("rec"),
                         label = "Number of recovered (i.e. immune) people under 65 at start date",
                         value = 200000),
            
            numericInput(inputId = ns("rec2"),
                         label = "Number of recovered (i.e. immune) people over 65 at start date",
                         value = 100000),
            
            numericInput(inputId = ns("pop_under_65"),
                         label = "Population of Ireland under 65",
                         value = 4000000),
            
            numericInput(inputId = ns("pop_over_65"),
                         label = "Population of Ireland over 65",
                         value = 900000),
            
            numericInput(inputId = ns("num_sim"),
                         label = "Number of simulations to run (higher = slower but more accurate)",
                         value = 200)
            
          ),
         bs4Dash::bs4TabCard(
           width = 9,
           title = "COVID-19 Vaccination Planning",
           id = 'tabcard',
           closable = FALSE,
           collapsible = FALSE,
           bs4Dash::bs4TabPanel(
             tabName = "Spread",
            plotly::plotlyOutput(ns("plot"), height = 500) %>% hamiltonThemes::distill_load_spinner(),
             checkboxInput(ns("log_scale"), "Log scale?", value = FALSE)
           ),
           bs4Dash::bs4TabPanel(
             tabName = "Assumptions",
             get_assumptions_text()
           )
         ) 
        )
      ),
      footer = hamiltonThemes:::bs4dash_distill_footer()
    )
 
  )
}
    
#' original_version Server Function
#'
#' @noRd 
mod_original_version_server <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$button, {
    shinyjs::toggle("exp")
    shinyjs::toggle("exp2")
    shinyjs::toggle("inf")
    shinyjs::toggle("inf2")
    shinyjs::toggle("rec")
    shinyjs::toggle("rec2")
    shinyjs::toggle("pop_under_65")
    shinyjs::toggle("pop_over_65")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  
  #realisation <- reactive({
  output$plot <- plotly::renderPlotly({
    ##### General setup
    # Inputs are YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O
    # Number of simulations
    num_sim = input$num_sim
    store = vector('list', num_sim)
    for (i in 1:num_sim) {
      store[[i]] = twoagesv(YS = input$pop_under_65 - 
                              input$exp - 
                              input$inf - 
                              input$rec, # Under 65s susceptible
                            YE = input$exp,
                            YI = input$inf,
                            YR = input$rec,
                            OS = input$pop_over_65 - 
                              input$exp2 - 
                              input$inf2 - 
                              input$rec2,
                            OE = input$exp2,
                            OI = input$inf2,
                            OR = input$rec2,
                            YR0Y = input$R0,
                            YR0O = input$R0_O_Y,
                            OR0Y = input$R0_O_Y,
                            OR0O = input$R0_1,
                            Yvac = 
                              (input$vacc_ef/100)*rep(input$vacc_Y, 1000), 
                            Ovac = 
                              (input$vacc_ef/100)*rep(input$vacc_O, 1000)) %>% 
        as.data.frame %>% 
        dplyr::rename("Time" = 1, "YS" = 2,"YE" = 3,
               "YI" = 4, "YR" = 5, "OS" = 6,
               "OE" = 7, "OI" = 8, "OR" = 9)
      len = length(store[[i]]$Time)
      store[[i]]$YV = pmin(store[[i]]$YS, 
                           (input$vacc_ef/100)*input$vacc_Y) # Young vaccinated
      store[[i]]$OV = pmin(store[[i]]$OS, 
                           (input$vacc_ef/100)*input$vacc_O) # Old vaccinated
      store[[i]]$YR_NV = store[[i]]$YR - 
        dplyr::lag(cumsum(store[[i]]$YV), default = 0) - 
        input$rec # Young recovered not vaccinated
      store[[i]]$OR_NV = store[[i]]$OR - 
        dplyr::lag(cumsum(store[[i]]$OV), default = 0) - 
        input$rec2 # Old recovered not vaccinated
    }
    # Quick plot
    # plot(store[[1]]$Time, store[[1]]$OR_NV, type = 'l')
    # lines(store[[1]]$Time, store[[1]]$OI, col = 'red')
    # Should be plotting difference in removed category
    # plot(store[[1]]$Time, diff2(store[[1]]$YR), type = 'l')
    # lines(store[[1]]$Time, diff2(store[[1]]$OR), col = 'red')
    
    # Extract out the infections and quantiles for each group
    YR_all = lapply(store, "[", "YR_NV")
    
    # Add 0s to each vector to make them the same length
    YR_padded = pad_fun(YR_all)
    
    # Now calculate medians and 90% CI
    YR_median = (apply(YR_padded, 1, 'quantile', 0.5))
    YR_high = (apply(YR_padded, 1, 'quantile', 0.95))
    YR_low = (apply(YR_padded, 1, 'quantile', 0.05))

    # Final data frame for YR
    dead_shift = input$dead_shift # Gap between cases and deaths
    nrows = lapply(YR_all, 'nrow') %>% unlist
    time_max = store[[which.max(nrows)]]$Time
    dates = as.Date("2021-01-01") + time_max #- dead_shift # Start from 3 weeks ago
    YR_final = tibble::tibble(Date = dates, 
                      `Under 65sXXXInfected - Value` = YR_median,
                      `Under 65sXXXInfected - low est` = YR_low,
                      `Under 65sXXXInfected - high est` = YR_high)
    
    # Now do the same thing for old infected
    OR_all = lapply(store, "[", "OR_NV")
    
    # Add 0s to each vector to make them the same length
    OR_padded = pad_fun(OR_all)
    
    # Now calculate medians and 90% CI
    OR_median = (apply(OR_padded, 1, 'quantile', 0.5))
    OR_high = (apply(OR_padded, 1, 'quantile', 0.95))
    OR_low = (apply(OR_padded, 1, 'quantile', 0.05))
    
    # Final data frame for OR
    OR_final = tibble::tibble(Date = dates, 
                      `Over 65sXXXInfected - Value` = OR_median,
                      `Over 65sXXXInfected - low est` = OR_low,
                      `Over 65sXXXInfected - high est` = OR_high)
    
    # Tidy up into one data frame
    final = dplyr::left_join(YR_final, OR_final, by = "Date") %>% 
      tidyr::pivot_longer(names_to = 'Type', values_to = 'Count', -Date) %>% 
      dplyr::mutate(Count = round(Count))
    final_twocols = as.matrix(stringr::str_split(final$Type, 'XXX', simplify = TRUE))    
    final$`Age group` = final_twocols[,1]
    final$Type = final_twocols[,2]
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    plt1 = ggplot2::ggplot(final %>% dplyr::filter(Type == 'Infected - Value'), 
                           ggplot2::aes(x = Date, colour = `Age group`)) +
      ggplot2::geom_line(ggplot2::aes(y = `Count`)) +
      ggplot2::labs(x = "Date", title = "Infected per day", y = NULL) +
      ggplot2::scale_x_date(date_labels = "%d-%b-%y") + 
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
      ggplot2::theme_bw()
    # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + ggplot2::scale_y_log10(expand = c(0, 0), labels = scales::comma)
    
    plotly::ggplotly(plt1)
    
  })
 
}
    
## To be copied in the UI
# mod_original_version_ui("original_version_ui_1")
    
## To be copied in the server
# callModule(mod_original_version_server, "original_version_ui_1")
 
