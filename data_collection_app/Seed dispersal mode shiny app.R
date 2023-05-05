library("shiny")
library("lubridate")
library("tidyverse")
library("httr")

# Make some folders if they don't already exist --------------------------------
if(!"output_dfs" %in% list.files("./data_collection_app/")){
  dir.create("./data_collection_app/output_dfs/")
}
if(!"tidy_data" %in% list.files("./data_collection_app/")){
  dir.create("./data_collection_app/tidy_data/")
}

# Load data --------------------------------------------------------------------
# Download these if they aren't already downloaded
# https://www.science.org/doi/10.1126/sciadv.add8553
if("sp_df.csv" %in% list.files("./data_collection_app/tidy_data")){
  sp_df <- read.csv("./data_collection_app/tidy_data/sp_df.csv", row.names = 1)
} else{
  sp_df <- read.csv("https://raw.githubusercontent.com/Wubing-Xu/Global_tree_beta-diversity/main/data/tree_pam/Status_Species_Ranges.csv")
  write.csv(sp_df, "./data_collection_app/tidy_data/sp_df.csv")
}

sp_df$sp <- sp_df$sp %>% gsub("_", " ", .)
sp_df <- sp_df %>%
  mutate(genus = word(sp, 1))


# Traits
if("traits_df.csv" %in% list.files("./data_collection_app/tidy_data")){
  traits_df <- read.csv("./data_collection_app/tidy_data/traits_df.csv", row.names = 1)
} else{
  traits_df <- read.csv("https://raw.githubusercontent.com/Wubing-Xu/Global_tree_beta-diversity/main/data/traits_phylogeny/TC_species_8Traits_mean_final.csv",
                        sep = ";")
  write.csv(traits_df, "./data_collection_app/tidy_data/traits_df.csv")
}

traits_df <- traits_df %>% 
  rename("sp" = "species")
traits_df$sp <- traits_df$sp %>% gsub("_", " ", .)


# Taxonomy
if("taxonomy_df.csv" %in% list.files("./data_collection_app/tidy_data")){
  taxonomy_df <- read.csv("./data_collection_app/tidy_data/taxonomy_df.csv", row.names = 1)
} else{
  taxonomy_df <- read.csv("https://raw.githubusercontent.com/Wubing-Xu/Global_tree_beta-diversity/main/data/traits_phylogeny/TC_species.family.54020sp.csv",
                          sep = ";")
  write.csv(taxonomy_df, "./data_collection_app/tidy_data/taxonomy_df.csv")
}

taxonomy_df <- taxonomy_df %>% 
  rename("sp" = "species")



# Associate each genus with its family -----------------------------------------
sp_df <- sp_df %>% 
  left_join(unique(dplyr::select(taxonomy_df, genus, family)))


# Determine which groups have existing data ------------------------------------
# Want to target our observations on genera and familys for which there's not
# existing data

mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1)
mode_average <- mode_dat %>% 
  group_by(sp) %>% 
  summarise(biotic = mean(biotic, na.rm = T))
all_dat <- sp_df %>% 
  left_join(mode_average) %>% 
  left_join(unique(dplyr::select(taxonomy_df, sp, family))) %>% 
  arrange(family)

gen_summary <- all_dat %>%
  group_by(genus) %>% 
  summarise(genus_coverage = mean(!is.na(biotic)) * 100,
            genus_biotic = mean(biotic, na.rm = T),
            family = first(family))
fam_summary <- all_dat %>%
  group_by(family) %>%
  summarise(family_coverage = mean(!is.na(biotic)) * 100,
            family_biotic = mean(biotic, na.rm = T),)
all_summary <- gen_summary %>% 
  left_join(fam_summary) %>% 
  arrange(family)

focal_genera <- all_summary %>% filter(family_coverage < 10,
                       genus_coverage < 10) %>% 
  pull(genus)

# Will just filter to these genera for data collection
sp_df <- sp_df %>% 
  filter(genus %in% focal_genera)


# Sample N per genus -----------------------------------------------------------
# Will just do one per genus for now. To make it potentially easier to
# find resources about these, we will use the species for which most 
# occurrence records were available
sp_df <- sp_df %>%
  group_by(genus) %>%
  arrange(desc(N)) %>% 
  slice_head(n = 1) %>%
  ungroup()

# Let's order by family
sp_df <- sp_df %>% 
  arrange(family)


# Pull in existing data --------------------------------------------------------
completed_sp <- list.files("./data_collection_app/output_dfs/", 
                          pattern = "output_df_",
                          full.names = T) %>% 
  lapply(read.csv) %>% 
  do.call(rbind, .)

# Filter these out of sp_df
sp_df <- sp_df %>% 
  filter(!sp %in% completed_sp$sp)


# Build app --------------------------------------------------------------------

# Make some choices about how this is displayed
col_width <- 2

Sys.setenv("R_BROWSER" = "/usr/bin/open -a '/Applications/Google Chrome.app'")
options(browser = as.vector(Sys.getenv("R_BROWSER")))

# Define UI
ui <- fluidPage(
  
  fluidRow(
    column(2, textOutput("family")),
    column(3, textOutput('timeleft')),
    column(7, textOutput("reminder"))
  ),

  # Display the sp column of the current element in sp_df
    
  fluidRow(
    column(12, h4(textOutput("sp")))
  ),
  
  # Button for opening Google Images search
  actionButton("images_fruit", "Fruit Img sp", style = "color: white; background-color: blue; border-color: blue"),
  
  # Button for opening Google Images search
  actionButton("images_seed", "Seed Img sp", style = "color: white; background-color: blue; border-color: blue"),
  
  # Button for opening Google Images search
  actionButton("images_fruit_gen", "Fruit Img gen", style = "color: white; background-color: purple; border-color: purple"),
  
  # Button for opening Google Images search
  actionButton("images_seed_gen", "Seed Img gen", style = "color: white; background-color: purple; border-color: purple"),
  
  br(),
  br(),
  
  # Button for opening scholar search
  actionButton("scholar", "Scholar sp", style = "color: white; background-color: darkorange; border-color: darkorange"),
  
  # Button for opening scholar search
  actionButton("scholar_gen", "Scholar gen", style = "color: white; background-color: orange; border-color: orange"),
  
  # Button for opening scholar search
  actionButton("google", "Google sp", style = "color: white; background-color: maroon; border-color: maroon"),
  
  # Button for opening scholar search
  actionButton("google_gen", "Google gen", style = "color: white; background-color: brown; border-color: brown"),
  
  br(),
  br(),
  
  # Button for resetting the checkboxes
  fluidRow(
    column(col_width, actionButton("reset", "Reset Checkboxes", 
                                   style = "color: white; background-color: grey; border-color: grey")),
    column(4, textInput("DataCollector", label = NULL, 
                      placeholder = "Enter your name")),
  ),

  # Labels for the rows of checkboxes
  fluidRow(
    column(1, h4("Structures"))
  ),

  # Checkboxes
  fluidRow(
    column(col_width, checkboxInput("PulpReward", "PulpReward")),
    column(col_width, checkboxInput("Elaiosome", "Elaiosome")),
    column(col_width, checkboxInput("Mimetic", "Mimetic"))
  ),
  fluidRow(
    column(col_width, checkboxInput("BarbsHooks", "BarbsHooks")),
    column(col_width, checkboxInput("StickySubstance", "StickySubstance"))
  ),
  fluidRow(
    column(col_width, checkboxInput("Wings", "Wings")),
    column(col_width, checkboxInput("PappusSilk", "PappusSilk")),
    column(col_width, checkboxInput("FloatStructures", "FloatStructures"))
  ),

  # Labels for the rows of checkboxes
  fluidRow(
    column(1, h4("Attributes"))
  ),

  # Checkboxes
  fluidRow(
    column(col_width, checkboxInput("Dry", "Dry")),
    column(col_width, checkboxInput("Fleshy", "Fleshy"))
  ),
  fluidRow(
    column(col_width, checkboxInput("Dehiscent", "Dehiscent")),
    column(col_width, checkboxInput("Indehiscent", "Indehiscent"))
  ),

  # Labels for the rows of checkboxes
  fluidRow(
    column(1, h4("Mode"))
  ),

  # Checkboxes
  fluidRow(
    column(col_width, checkboxInput("Wind", "Wind")),
    column(col_width, checkboxInput("Water", "Water")),
    column(col_width, checkboxInput("Ballistic", "Ballistic")),
    column(col_width, checkboxInput("Unassisted", "Unassisted"))
  ),
  fluidRow(
    column(col_width, checkboxInput("Endozoochory", "Endozoochory")),
    column(col_width, checkboxInput("Cache", "Cache")),
    column(col_width, checkboxInput("Attachment", "Attachment")),
    column(col_width, checkboxInput("Ant", "Ant"))
  ),
  
  br(),
  
  # Labels for the rows of checkboxes
  fluidRow(
    column(2, h4("Determination")),
    column(4, textInput("DetermRefs", label = NULL, 
                        placeholder = "Paste reference URLs separated by |"))
  ),

  # Checkboxes
  fluidRow(
    column(col_width, checkboxInput("None", "Insufficient")),
    column(col_width, checkboxInput("Literature", "Literature")),
    column(col_width, checkboxInput("VisualAssessment", "VisualAssessment"))
  ),
  
  fluidRow(
    column(col_width, checkboxInput("GenusLevel", "GenusLevel")),
    column(col_width, checkboxInput("FamilyLevel", "FamilyLevel")),
    column(col_width, checkboxInput("GenusVaried", "GenusVaried"))
    
  ),
  
  br(),
  
  # Labels for the rows of checkboxes
  fluidRow(
    column(2, h4("Other zoochory")),
    column(4, textInput("ZooEvidRefs", label = NULL,
                        placeholder = "Paste reference URLs separated by |"))
  ),
  
  # Checkboxes
  fluidRow(
    column(col_width, checkboxInput("ZoochoryEvidence", "ZoochoryEvidence")),
    column(col_width, checkboxInput("ZooEvidSpecies", "ZooEvidSpecies")),
    column(col_width, checkboxInput("ZooEvidGenus", "ZooEvidGenus"))
  ),
  
  br(),


  # Button for entering the current selection
  actionButton("enter", "Enter", style = "color: white; background-color: green; border-color: green"),
  
  # Button for going to previous
  actionButton("previous", "Previous", style = "color: grey; background-color: white; border-color: grey"),
  
  
  br(),
  br(),
  # Button for finishing up with the present session. This is necessary in order
  # to automatically have the results written to file
  actionButton("stop_button", "Finish session", style = "color: white; background-color: red; border-color: red")

)

# Define server
server <- function(input, output, session) {
  # Initialize a counter for tracking the current row of sp_df
  counter <- reactiveValues(row = 1)

  output$reminder <- renderText({
    "Remember to click 'Finish Session', or data will not be entered!"
  })

  # Display the sp column of the current element in sp_df
  output$sp <- renderText({
    sp_df$sp[counter$row]
  })
  
  # Display the fam column of the current element in sp_df
  output$family <- renderText({
    sp_df$family[counter$row]
  })


  # Event for clicking the Google Images button
  observeEvent(input$images_fruit, {
    browseURL(paste0("https://www.google.com/search?tbm=isch&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     word(sp_df$sp[counter$row],2),
                     "+",
                     "fruit"))
  })

  # Event for clicking the Google Images button
  observeEvent(input$images_seed, {
    browseURL(paste0("https://www.google.com/search?tbm=isch&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     word(sp_df$sp[counter$row],2),
                     "+",
                     "seed"))
  })

  # Event for clicking the Google Images button
  observeEvent(input$images_fruit_gen, {
    browseURL(paste0("https://www.google.com/search?tbm=isch&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     "fruit"))
  })

  # Event for clicking the Google Images button
  observeEvent(input$images_seed_gen, {
    browseURL(paste0("https://www.google.com/search?tbm=isch&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     "seed"))
  })



  # Event for clicking the scholar button
  observeEvent(input$scholar, {
    browseURL(paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     word(sp_df$sp[counter$row],2),
                     "+",
                     "\"seed+dispersal\"&btnG=") %>% URLencode(repeated = T))
  })

  # Event for clicking the scholar button
  observeEvent(input$scholar_gen, {
    browseURL(paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     "\"seed+dispersal\"&btnG=") %>% URLencode(repeated = T))
  })
  
  
  
  # Event for clicking the google button
  observeEvent(input$google, {
    browseURL(paste0("https://www.google.com/search?q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     word(sp_df$sp[counter$row],2),
                     "+",
                     "\"seed+dispersal\"") %>% URLencode(repeated = T))
  })
  
  # Event for clicking the google button
  observeEvent(input$google_gen, {
    browseURL(paste0("https://www.google.com/search?q=",
                     word(sp_df$sp[counter$row],1),
                     "+",
                     "\"seed+dispersal\"") %>% URLencode(repeated = T))
  })
  



  # Reset if need be
  observeEvent(input$reset, {
    # Reset all checkbox inputs to FALSE
    updateCheckboxInput(session, "PulpReward", value = FALSE)
    updateCheckboxInput(session, "Elaiosome", value = FALSE)
    updateCheckboxInput(session, "Mimetic", value = FALSE)
    updateCheckboxInput(session, "BarbsHooks", value = FALSE)
    updateCheckboxInput(session, "StickySubstance", value = FALSE)
    updateCheckboxInput(session, "Wings", value = FALSE)
    updateCheckboxInput(session, "PappusSilk", value = FALSE)
    updateCheckboxInput(session, "FloatStructures", value = FALSE)

    updateCheckboxInput(session, "Dry", value = FALSE)
    updateCheckboxInput(session, "Fleshy", value = FALSE)
    updateCheckboxInput(session, "Dehiscent", value = FALSE)
    updateCheckboxInput(session, "Indehiscent", value = FALSE)

    updateCheckboxInput(session, "Wind", value = FALSE)
    updateCheckboxInput(session, "Water", value = FALSE)
    updateCheckboxInput(session, "Ballistic", value = FALSE)
    updateCheckboxInput(session, "Unassisted", value = FALSE)
    updateCheckboxInput(session, "Endozoochory", value = FALSE)
    updateCheckboxInput(session, "Cache", value = FALSE)
    updateCheckboxInput(session, "Attachment", value = FALSE)
    updateCheckboxInput(session, "Ant", value = FALSE)


    updateCheckboxInput(session, "None", value = FALSE)
    updateCheckboxInput(session, "Literature", value = FALSE)
    updateCheckboxInput(session, "VisualAssessment", value = FALSE)
    updateCheckboxInput(session, "GenusLevel", value = FALSE)
    updateCheckboxInput(session, "FamilyLevel", value = FALSE)
    updateCheckboxInput(session, "GenusVaried", value = FALSE)
    
    updateCheckboxInput(session, "ZoochoryEvidence", value = FALSE)
    updateCheckboxInput(session, "DetermEvidence", value = FALSE)
    updateCheckboxInput(session, "ZooEvidSpecies", value = FALSE)
    updateCheckboxInput(session, "ZooEvidGenus", value = FALSE)
    
    updateTextInput(session, "ZooEvidRefs", value = NULL,
                    placeholder = "Paste reference URLs separated by |")
    updateTextInput(session, "DetermRefs", value = NULL,
                    placeholder = "Paste reference URLs separated by |")
  })

  # Event for clicking the enter button
  observeEvent(input$enter, {
    # Create a new row in the output data frame for the current selection
    output_row <- data.frame(
      sp = sp_df$sp[counter$row],
      PulpReward = input$PulpReward,
      Elaiosome = input$Elaiosome,
      Mimetic = input$Mimetic,
      BarbsHooks = input$BarbsHooks,
      StickySubstance = input$StickySubstance,
      Wings = input$Wings,
      PappusSilk = input$PappusSilk,
      FloatStructures = input$FloatStructures,

      Dry = input$Dry,
      Fleshy = input$Fleshy,
      Dehiscent = input$Dehiscent,
      Indehiscent = input$Indehiscent,

      Wind = input$Wind,
      Water = input$Water,
      Ballistic = input$Ballistic,
      Unassisted = input$Unassisted,
      Endozoochory = input$Endozoochory,
      Cache = input$Cache,
      Attachment = input$Attachment,
      Ant = input$Ant,

      None = input$None,
      Literature = input$Literature,
      VisualAssessment = input$VisualAssessment,
      GenusLevel = input$GenusLevel,
      FamilyLevel = input$FamilyLevel,
      GenusVaried = input$GenusVaried,
      
      ZoochoryEvidence = input$ZoochoryEvidence,
      ZooEvidRefs = input$ZooEvidRefs,
      DetermRefs = input$DetermRefs,
      ZooEvidSpecies = input$ZooEvidSpecies,
      ZooEvidGenus = input$ZooEvidGenus,
      
      DataCollector = input$DataCollector
      
    )
    # Append the new row to the output data frame
    output_df <<- rbind(output_df, output_row)


    # Increment the counter to move to the next element in sp_df
    counter$row <- counter$row + 1
  })

  # Event for clicking the enter button
  observeEvent(input$previous, {

    # Move to the previous element in sp_df
    counter$row <- counter$row - 1
  })

  observeEvent(input$stop_button, {
    stopApp()
  })
  
  
  
  
  
  
  
  
  # This is the timer
  timer <- reactiveVal(0)
  active <- reactiveVal(TRUE)
  update_interval = 1
  
  output$timeleft <- renderText({
    paste("Time passed: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(round(timer()+update_interval, 1))
      }
    })
  })
  
  observeEvent(input$enter, {timer(0)})

}

# Run the app ------------------------------------------------------------------
output_df <- data.frame(sp = character(), A = logical(), B = logical(), C = logical())
shinyApp(ui, server)

# Need to only take the last row for each species in the dataframe, as there is
# a row created at each "enter", even if you went back to the previous

output_df <- output_df %>%
  group_by(sp) %>%
  slice_tail(n = 1)


# Output the data from this session to a csv file
output_filename <- paste0("./data_collection_app/output_dfs/output_df_", Sys.time(), ".csv")
write.csv(output_df, output_filename)





