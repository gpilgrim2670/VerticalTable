# load packages
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(reactable)
library(gt)

#### import data set ####
vertical_championships <- readRDS("vertical_championships.rds")

#### reformat data ####
top_line_summary <- vertical_championships %>%
  pivot_longer(cols = contains("Attempt"), names_to = "Attempt", names_prefix = "Attempt", values_to = "Outcome") %>%
  filter(Outcome == "O" | Outcome == "X") %>%
  select(Name, Meet, Event, Gender, Height, Bar_Order, Event_Date, Attempt, Outcome, Meet_ID, Event_ID) %>%
  group_by(Name, Event, Event_Date, Outcome) %>%
  mutate(Meet_Mark = case_when(Outcome == "O" ~ max(Height),
                               TRUE ~ NA_real_)) %>%
  ungroup() %>%
  group_by(Name, Event, Event_Date) %>%
  mutate(Meet_Jump_Attempts = length(Outcome),
         Meet_Ht_Attempts = length(unique(Height)),
         Meet_Ht_Clear = sum(Outcome == "O"),
         Meet_Jump_Clear = Meet_Ht_Clear) %>%
  ungroup() %>%
  filter(Meet_Mark != "NA") %>%
  group_by(Event, Gender, Event_Date) %>%
  mutate(Place = match(Meet_Mark, sort(unique(Meet_Mark), decreasing = TRUE))) %>%
  ungroup() %>%
  distinct(Name, Event, Meet, Gender, Meet_Mark, Place, Event_Date, Meet_Ht_Attempts, Meet_Ht_Clear, Meet_Jump_Attempts, Meet_Jump_Clear, Meet_ID, Event_ID)

#### select random athlete to populate Name_rt ####
starter <- sample(unique(top_line_summary$Name), 1)

#### UI ####
ui <- fluidPage(
         tags$head(includeCSS("www/vertical_tables.css")),
         useShinyjs(),
                fluidRow(
                  column(12,
                  # sidebarLayout(
                  #   sidebarPanel(
                      selectizeInput("Name_rt", choices = NULL, label = "Find an athlete") %>% 
                        tagAppendAttributes(class = "athlete_input"),
                      hr()),
                  column(12,
                      textOutput("athlete") %>%
                        tagAppendAttributes(class = "athlete_name_big"),
                      textOutput("event") %>%
                        tagAppendAttributes(class = "athlete_event_big")
                      ),
                   column(12,
                    # mainPanel(
                      reactableOutput("name_rt"),
                    # mainPanel(uiOutput("athlete_meets")),
                    hr(),
                    gt_output("gt_meet"))
                  #  mainPanel(textOutput("row_print"))

                    )
             )

#### server ####
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "Name_rt",
                       choices = unique(top_line_summary$Name),
                       selected = starter,
                       server = TRUE)

  #### filter data for chosen athlete ####
  athlete_selection <- reactive({
    req(input$Name_rt)
    top_line_summary %>%
      filter(Name %in% input$Name_rt)
  })

  #### determine event(s) in which athlete competes ####
   athlete_event <- reactive({
     athlete_selection() %>%
       select(Event) %>%
       distinct() %>%
       mutate(Event = ifelse(nrow(.) > 1, "Multi", Event)) %>%
       distinct() %>%
       pull()
       
   })

  #### display athlete name ####
  output$athlete <- renderText({
    input$Name_rt
  })

  #### display athlete event ####
  output$event <- renderText({
    athlete_event()
  })

  #### reactable allowing selection of meets in which chosen athlete has competed with summary results ####
  output$name_rt <- renderReactable({

    #### columns for bar heights and number of jumps ####
  agg_height_cols <- c("Meet_Ht_Attempts", "Meet_Ht_Clear")
  agg_jump_cols <- c("Meet_Jump_Attempts", "Meet_Jump_Clear")
  
  #### produce reactable for chosen athlete with meets competed in and summary results ####
   athlete_selection() %>%
      select(-Gender, -Name) %>%
     {if(length(unique(athlete_selection() %>% pull(Event))) <= 1)
       select(., -Event) else .
     } %>% 
      reactable(.,
                defaultColDef = colDef(align = "center", headerClass = "rt_header_class"),
                columnGroups = list(
                  colGroup(name = "Heights", columns = agg_height_cols),
                  colGroup(name = "Jumps", columns = agg_jump_cols)
                ),
                columns = list(
                  Meet = colDef(minWidth = 85, align = "left"),
                  Event_ID = colDef(show = FALSE),
                  Meet_ID = colDef(show = FALSE),
                  Meet_Mark = colDef(name = "Mark"),
                  Event_Date = colDef(name = "Date", maxWidth = 75),
                  Meet_Ht_Attempts = colDef(name = "Attempted", maxWidth = 100),
                  Meet_Ht_Clear = colDef(name = "Cleared", maxWidth = 85),
                  Meet_Jump_Attempts = colDef(name = "Attempted", maxWidth = 100),
                  Meet_Jump_Clear = colDef(name = "Cleared", maxWidth = 85),
                  Place = colDef(maxWidth = 60)
                ),
                selection = "single",
                defaultSelected = 1,
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#cbdeff", boxShadow = "inset 5px 0 0 0 #ff180e")
                )
              )
   }
)

#### get meet (row) selected in reactable to display detailed results for ####
  selected_row <- reactive(
    getReactableState("name_rt", name = "selected")
    )

  #### meet to display complete, detailed results for ####
  selected_meet <- reactive({
    athlete_selection() %>%
      slice(selected_row()) %>%
      select(Event_ID) %>%
      pull()
  })


  #### collect detailed results for all athletes in a given meet ####
  full_meet_table <- reactive({
    full_meet <- vertical_championships %>%
      filter(Event_ID == selected_meet()) %>%
      select(Name, Attempt1, Attempt2, Attempt3, Height) %>%
      pivot_longer(
        cols = contains("Attempt"),
        names_to = "Attempt",
        names_prefix = "Attempt",
        values_to = "Outcome"
      )

    #### order athletes by finish place ####
    standings_order <- full_meet %>%
      filter(Outcome == "O" | Outcome == "X") %>%
      group_by(Name, Outcome) %>%
      mutate(Mark = case_when(Outcome == "O" ~ max(Height),
                              TRUE ~ NA_real_)) %>%
      ungroup() %>%
      group_by(Name) %>%
      mutate(Attempts = n()) %>%
      distinct(Name, Mark, Attempts) %>%
      arrange(desc(Mark), desc(Attempts)) %>%
      filter(!is.na(Mark)) %>%
      select(Name) %>%
      as.vector()

    #### vector to arrange table - order of finish ####
    table_order <- as.vector(standings_order$Name)
    
    #### background colors  for cells based on results of athlete's attempt ####
    color_scheme = c(X = "#ff180e",
                     O = "#44aa22",
                     P = "#cbdeff",
                     "-" = "#cbdeff",
                     " " = "#ff180e44",
                     "  " = "#44aa22")
    
    #### collect basic information about the meet/event to display ####
    meet_info <- vertical_championships %>%
      filter(Event_ID == selected_meet()) %>%
      select(Meet, Event, Gender, Event_Date) %>%
      distinct()
    
    #### create gt table to display entire event results in vertical format ####
    full_meet %>%
      arrange(factor(Name, levels = table_order)) %>%
      mutate(Bar_Attempt = paste0(Height, "_", Attempt),
             
             Outcome = case_when(is.na(Outcome) ~ " ", 
                                 Outcome == "" ~ "  ",
                                 TRUE ~ Outcome))  %>%
      select(Name, Outcome, Bar_Attempt) %>%
      pivot_wider(names_from = "Bar_Attempt", values_from = "Outcome") %>%
      mutate(across(cols = -Name, .fns = ~ as.factor(.)))  %>%
      gt() %>%
      tab_header(title = meet_info$Meet,
                 subtitle = paste0(meet_info$Gender, "'s ", meet_info$Event, " | ", meet_info$Event_Date)) %>%
      opt_align_table_header(align = "left") %>% 
      tab_spanner_delim(columns = -Name, delim = "_") %>%
      data_color(
        columns = -Name,
        colors = scales::col_factor(
          palette = color_scheme,
          domain = names(color_scheme),
          ordered = TRUE
        ),
        apply_to = c("fill", "text")
      )
    

 })

  #### output gt results table ####
  # needs to wait for computation of full_meet_table to finish
  delay(1000,
  output$gt_meet <- render_gt(
    expr = full_meet_table()
  )
  )

}


# Run the application
shinyApp(ui = ui, server = server)













