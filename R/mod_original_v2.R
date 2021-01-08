#' original_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_original_v2_ui <- function(id){
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
            dateInput(ns("start_vac"), "Starting date of vaccination", "2021-01-01" ),
            sliderInput(ns("vacc_Y"), "Daily number of vaccinations for under 65s", 0, 50000, 2000, step = 100),
            
            sliderInput(ns("vacc_O"), "Daily number of vaccinations for over 65s", 0, 50000, 18000, step = 100),
            
            sliderInput(ns("vacc_ef"), "Vaccine effectiveness (%)", 50, 100, 90, step = 1),
            sliderInput(ns("vacc_re"), "Vaccine refusal rate (%)", 0, 100, 30, step = 1),
            # https://www.irishpost.com/news/almost-a-third-of-irish-people-would-refuse-covid-19-vaccine-survey-says-194257
            
            sliderInput(ns("R0_Y"), "Average number of infections from each infected person (R number) for under 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(ns("R0_O"), "Average number of infections from each infected person (R number) for over 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(inputId = ns("R0_O_Y"),
                        label = "Average number of infections passed between under and over 65s per infected person (Cross R number)",
                        0, 10, 0.3, step=0.1),
            
            actionButton(inputId = ns("button"), label = "Show/hide extra options"),
            
            numericInput(inputId = ns("num_days"),
                         label = "Total number of days that vaccine can be administered",
                         min = 0,
                         value = 1000),
            
            numericInput(inputId = ns("exp_Y"),
                         label = "Number of asymptomatic spreaders under 65 at start date",
                         min = 0,
                         value = 2000),
            
            numericInput(inputId = ns("inf_Y"),
                         label = "Number of symptomatic spreaders under 65 at start date",
                         value = 2000),
            
            numericInput(inputId = ns("exp_O"),
                         label = "Number of asymptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("inf_O"),
                         label = "Number of symptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("rec_Y"),
                         label = "Number of recovered (i.e. immune) people under 65 at start date",
                         value = 200000),
            
            numericInput(inputId = ns("rec_O"),
                         label = "Number of recovered (i.e. immune) people over 65 at start date",
                         value = 100000),
            
            numericInput(inputId = ns("pop_Y"),
                         label = "Population of Ireland under 65",
                         value = 4000000),
            
            numericInput(inputId = ns("pop_O"),
                         label = "Population of Ireland over 65",
                         value = 900000),
            
            numericInput(inputId = ns("num_sim"),
                         label = "Number of simulations to run (higher = slower but more accurate)",
                         min = 0,
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
              get_assumptions_text2()
            )
          )
        )
      ),
      footer = hamiltonThemes:::bs4dash_distill_footer()
    )
  )
}
    
#' original_v2 Server Function
#'
#' @noRd 
mod_original_v2_server <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$button, {
    shinyjs::toggle("num_days")
    shinyjs::toggle("exp_Y")
    shinyjs::toggle("exp_O")
    shinyjs::toggle("inf_Y")
    shinyjs::toggle("inf_O")
    shinyjs::toggle("rec_Y")
    shinyjs::toggle("rec_O")
    shinyjs::toggle("pop_Y")
    shinyjs::toggle("pop_O")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  
  output$plot <- plotly::renderPlotly({
    ##### General setup
    
    # Inputs are YSU, YSNV, YE, YI, YR, OSU, OSNV, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, Yvac, Ovac, Veff
    # where YSU = Young Susceptible Not Yet Vaccinated
    # YSNV = Young Susceptible Refused Vaccine
    # etc
    
    # Outputs are Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
    # These mean
    # YSV = Young Susceptible And Vaccinated - waiting for it to become effective
    # YSVNE = Young Susceptible but Vaccine Not Effective
    # YSNV = Young Susceptible But Refused Vaccine
    # YR = Young Recovered due to having the disease
    # YVR = Young Recovered due to having vaccination
    
    # Number of simulations
    num_sim = input$num_sim
    store = vector('list', num_sim)
    for (i in 1:num_sim) {
      start_S_Y = input$pop_Y - 
        input$exp_Y - 
        input$inf_Y - 
        input$rec_Y # Total people in S = pop - E - I - R
      start_S_O = input$pop_O - 
        input$exp_O - 
        input$inf_O - 
        input$rec_O
      store[[i]] = twoagesv2(
        YSU = start_S_Y * (1-input$vacc_re/100),# Young Susceptible Not Yet Vaccinated - calculated as the proportion of people who are willing to be vaccinated
        YSNV = start_S_Y*input$vacc_re/100, # Young Susceptible Refused Vaccine - this is all the people who refused to be vaccinated, 
        YE = input$exp_Y, 
        YI = input$inf_Y, 
        YR = input$rec_O, 
        OSU = start_S_O * (1-input$vacc_re/100), 
        OSNV = start_S_O*input$vacc_re/100, 
        OE = input$exp_O, 
        OI = input$inf_O, 
        OR = input$rec_O, 
        YR0Y = input$R0_Y, 
        YR0O = input$R0_O_Y, 
        OR0Y = input$R0_O_Y, 
        OR0O = input$R0_O, 
        Yvac = rep(input$vacc_Y, input$num_days),  
        Ovac = rep(input$vacc_O, input$num_days), 
        Veff = input$vacc_ef/100) %>% 
        as.data.frame %>% 
        dplyr::rename("Time" = 1, "YSU" = 2, "YSV" = 3, 
               "YSVNE" = 4, "YSNV" = 5, "YE" = 6,
               "YI" = 7, "YR" = 8, "YRV" = 9, 
               "OSU" = 10, "OSV" = 11, 
               "OSVNE" = 12, "OSNV" = 13, "OE" = 14,
               "OI" = 15, "OR" = 16, "ORV" = 17)
      #Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
      
      len = length(store[[i]]$Time)
    }
    
    # Quick plot
    # plot(store[[1]]$Time, store[[1]]$ORV, type = 'l') # Vaccinated and recovered
    # lines(store[[1]]$Time, store[[1]]$OR, col = 'red') # Vaccinated after disease
    # plot(store[[1]]$Time, store[[1]]$YRV, type = 'l') # Vaccinated and recovered
    # lines(store[[1]]$Time, store[[1]]$YR, col = 'red') # Vaccinated after disease
    
    # Extract out the infections and quantiles for each group
    YR_final = grab_all(store, "YR", "Under 65s recovered from disease", input$start_vac)
    OR_final = grab_all(store, "OR", "Over 65s recovered from disease", input$start_vac)
    OR_final = grab_all(store, "OR", "Over 65s recovered from disease", input$start_vac)
    YRV_final = grab_all(store, "YRV", "Under 65s successfully vaccinated", input$start_vac)
    ORV_final = grab_all(store, "ORV", "Over 65s successfully vaccinated", input$start_vac)
    
    # Tidy up into one data frame
    final = dplyr::left_join(YR_final, OR_final, by = "Date") %>% 
      dplyr::left_join(YRV_final, by = 'Date') %>% 
      dplyr::left_join(ORV_final, by = 'Date') %>% 
      tidyr::pivot_longer(names_to = 'Type', values_to = 'Count', -Date) %>% 
      dplyr::mutate(Count = round(Count))
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    plt1 = ggplot2::ggplot(final,
                           ggplot2::aes(x = Date, colour = Type)) +
      ggplot2::geom_line(ggplot2::aes(y = `Count`)) +
      ggplot2::labs(x = "Date", title = "Virus progression with vaccination effectiveness and refusal", y = NULL) +
      ggplot2::scale_x_date(date_labels = "%d-%b-%y") + 
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
      ggplot2::theme_bw()
    # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + ggplot2::scale_y_log10(expand = c(0, 0), labels = scales::comma)
    
    plotly::ggplotly(plt1)
    
  })
}
    
## To be copied in the UI
# mod_original_v2_ui("original_v2_ui_1")
    
## To be copied in the server
# callModule(mod_original_v2_server, "original_v2_ui_1")
 
