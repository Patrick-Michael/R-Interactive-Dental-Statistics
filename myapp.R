#load necessary packages
pacman::p_load(
  tidyverse,   # Collection of R packages for data science (ggplot2, dplyr, tidyr, etc.)
  data.table,  # Fast data manipulation
  lubridate,   # Date and time handling
  stringr,     # String manipulation
  readr,       # Reading data
  magrittr,    # Pipes (%>%)
  devtools,    # Tools for R package development
  knitr,       # Dynamic report generation
  rmarkdown,   # R Markdown for reproducible reports
  shiny,       # Interactive web applications
  plotly,      # Interactive plots
  tidymodels,  # Tidy machine learning
  readxl,      # Excel sheets
  here,        # Versatile pathways
  janitor,     # Cleaning functions
  openxlsx,        #Exports into excel
  scico,       # More color palette options
  rsconnect   #To move the shiny app to a host
)


dataset_raw <- read_excel(here::here("stats_doc_202412.xlsm"))

dataset <- dataset_raw 
colnames(dataset) <- dataset[4,]   #Clean redundant columns and empty cells
dataset <- dataset[-c(1,2,3,4),]

dataset <- dataset %>%
  janitor::clean_names() %>%       #Clean and transliterate column names
  
  slice(-c(58:144))  %>%            #Remove empty rows
  
  mutate(across(3:31, as.numeric))    #Changes the value of all relevant columns 
# to numeric instead of character


#replace clean names with custom intelligeble names

colnames(dataset) <- recode(colnames(dataset)                               ,
                            "na"             = "names_dr"     ,
                            "na_2"           = "type_mn"      , "khlʿ_lbny"        = "ext_dc"       ,
                            "khlʿ_bdly"      = "ext_pm"       , "ghyar"            = "drsng"        ,
                            "khlʿ_jrahy"     = "ext_srg"      , "ʿmlyat_jrahyt"    = "surgery"      ,
                            "kht_jyr"        = "scaling"      , "ʿmlyat_ltht"      = "perio_srg"    , 
                            "hshw_mwqt_lbny" = "fl_dc_temp"   , "hshw_mmlghm_lbny" = "fl_dc_amlg"   ,
                            "kmbwzyt_lbny"   = "fl_dc_cmp"    , "zjajy_lbny"       = "fl_dc_gi"     ,
                            "ghyar_ʿsb_lbny" = "endo_dc_acc"  , "hshw_ʿsb_lbny"    = "endo_dc"      ,
                            "hshw_mwqt_bdly" = "endo_pm_temp" , "hshw_mmlghm_bdly" = "fl_pm_amlg"   ,
                            "kmbwzyt_bdly"   = "fl_pm_cmp"    , "zjajy_bdly"       = "fl_pm_gi"     ,
                            "hshw_ʿsb_bdly"  = "endo_pm"      , "ghyar_ʿsb_bdly"   = "endo_pm_acc"  ,
                            "na_3"           = "meds"         , "na_4"             = "xray"         ,
                            "thwyl_kharjy"   = "rfr_ext"      , "thwyl_dakhly"     = "rfr_int"      ,
                            "na_5"           = "general"      , "na_6"             = "other"        ,
                            "na_7"           = "total"        , "na_8"             = "fl_total"     ,
                            "na_9"           = "fl_total_mn"  , "na_10"            = "procedures"
)



for (current_col in colnames(dataset)) {
  assign(current_col, dataset[[current_col]]) #Changes column names into objects
}                                           #for easier manipulation                     



dataset$names_dr <- ifelse(is.na(dataset$names_dr), lag(dataset$names_dr),
                           dataset$names_dr)     
#Repeat the first name into the 
#two next NA rows


dataset$names_dr <- ifelse(is.na(dataset$names_dr), lag(dataset$names_dr,2)
                           ,dataset$names_dr) 



#Translation and transliteration for names into Latin Characters

dataset$type_mn <-  recode(dataset$type_mn, "اقتصادي مسائي" = "paid_night" ,
                           "اقتصادي صباحي" = "paid_day"  ,
                           "مجاني"         = "free"      ) 

dataset$names_dr <- recode(dataset$names_dr                                , 
                           "د.رانيا محفوظ بخيت"   = "Rania M. Bekheit"     ,
                           "د.مارتن ماجد لويس"    = "Martin M. Louis"      ,
                           "د.شيماء زغلول أحمد"   = "Shaima Z. Ahmed"      ,
                           "د.كريستين ماجد الفونس"= "Christine M. Alfons"  ,
                           "د.ساندى عماد منير"    = "Sandy E. Mounir"      ,
                           "د.ياسر عبد الرحمن على"= "Yassir A. Ali"        ,
                           "د.ريم  حسن فارس"      = "Reem H. Fares"        ,
                           "د.ساندرا اشرف موريس"  = "Sandra A. Maurice"    ,
                           "د.دينا عونى محمد"     = "Dina A. Mohamed"      ,
                           "د.محمد اشرف الحسيني"  = "Mohamed A. Alhusseiny",
                           "د.احمد احمد عباس"     = "Ahmed A. Abbas"       ,
                           "د.مارينا سامى صبرى"   = "Marina S. Sabry"      , 
                           "د.شاهنده مجدى محمد"   = "Shahenda M. Mohamed"  ,
                           "د.فاطمه طارق محمد"    = "Fatima T. Mohamed"    ,
                           "د.عمرو عصام الدين"    = "Amr Essameldein"      ,
                           "د.حمزه محمد عاشور"    = "Hamza M. Ashour"      ,
                           "د.تقي ياسر محمد"      = "Toqa Y. Mohamed"      ,
                           "د.سعد مرشد حنا"       = "Saad M. Hanna"        ,
                           "هالة رشدي داود"       = "Hala R. Dawood"       ,
                           "الاجمالي"              = "Total"                )




#Assign a list of colnames that belong to each money group for later manipulation
#We used list function to make the list, setdiff to exclude, and we used quoted names
#because we dont want to access the numbers under these themselves

value_groups <-  list( group50 = c( "drsng" , "ext_dc" , "ext_pm", "scaling",
                                    "xray"  )                               ,
                       group100 = setdiff( grep("fl" , colnames(dataset) , value = TRUE )  ,
                                           c("fl_total", "fl_total_mn"))                           ,              
                       group200 =c( grep("endo", colnames(dataset) , value = TRUE)  , 
                                    "ext_srg" , "surgery" , "perio_srg")                                       )



#Assigns a value of 50 to group 50 etc. We used the rep function to repeat the value
#the necessary times to match the number of vectors inside each group

value_table <- c( setNames( rep(50, length(value_groups$group50)            )
                            ,value_groups$group50)                      ,
                  setNames( rep(100 , length(value_groups$group100)         )
                            ,value_groups$group100)                     , 
                  setNames(rep(200,length(value_groups$group200)            )
                           ,value_groups$group200)                     )


#Create new columns showing the monetary value of previously prepared data
#excluding free by assigning it a 0 value

dataset <-  dataset   %>%  
  mutate(across(all_of(names(value_table)), 
                ~ifelse(type_mn == "free", 0 , .x * value_table[cur_column()]), 
                .names = "revenue_{.col}"))

#Create total revenue and use it to calculate how much each doctor is owed
#for their work. We start by grouping all the "revenue" columns into one group

revenue_cols <-  grep("revenue_", colnames(dataset), value = TRUE )

dataset <-  dataset %>% 
  mutate(revenue_total = rowSums(across(all_of( revenue_cols)), na.rm = TRUE) , 
         dr_owed = revenue_total * 0.10                                        )

#Create new columns to display total of free cases and total of referred and med cases
#This will be important later during the doctor evaluation

#We first have to assign the columns corresponding to services and not metadata

case_columns <- setdiff(colnames(dataset), c("names_dr" , "meds"   , revenue_cols , "total"      ,
                                             "rfr_ext"  , "rfr_int", "fl_total"   , "fl_total_mn",
                                             "procedures" , "type_mn"))
ref_columns <- c("rfr_ext", "rfr_int" , "meds")
dataset <-  dataset %>% 
  mutate(total_free = ifelse(type_mn == "free", rowSums(across(all_of(case_columns)),
                                                        na.rm = TRUE),0
  ) ,
  total_ref = rowSums(across(all_of(ref_columns)), na.rm = TRUE ))





#Creation of a clean and main summary for later visualization

# Step 1: Calculate totals for day and night separately
day_night_summary <- dataset %>%
  group_by(names_dr, type_mn) %>%
  summarize(
    total_cases = sum(across(all_of(names(value_table))), na.rm = TRUE),  # Total cases (paid only)
    total_revenue = sum(revenue_total, na.rm = TRUE),  # Total revenue
    total_free = sum(total_free, na.rm = TRUE),  # Free cases
    total_ref = sum(total_ref, na.rm = TRUE),  # Referral and medication cases
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = type_mn,
    values_from = c(total_cases, total_revenue, total_free, total_ref),
    names_glue = "{.value}_{type_mn}"                      # Append type_mn (day/night) to column names
  )

# Step 2: Merge day/night data with the overall summary
doctor_summary <- dataset %>%
  group_by(names_dr) %>%
  summarize(
    total_revenue = sum(revenue_total, na.rm = TRUE),  # Overall revenue
    total_paid = sum(across(all_of(names(value_table))), na.rm = TRUE),  # Total paid cases
    total_free = sum(total_free, na.rm = TRUE),  # Total free cases
    total_ref = sum(total_ref, na.rm = TRUE),  # Total referral and medication cases
    total_cases = total_paid + total_free + total_ref,  # Comprehensive total
    total_owed = sum(dr_owed, na.rm = TRUE),  # Amount owed
    .groups = "drop"
  ) %>%
  left_join(day_night_summary, by = "names_dr")  # Add day/night data



#Now the percentages

# Add percentages to the doctor_summary table and round them to 2 decimal places
doctor_summary <- doctor_summary %>%
  mutate(
    pct_free = round((total_free / total_cases) * 100, 2),  # Free-to-total percentage
    pct_ref = round((total_ref / total_cases) * 100, 2),   # Referral-to-total percentage
    pct_paid = round((total_paid / total_cases) * 100, 2), # Paid-to-total percentage
    
    pct_day_cases = round((total_cases_paid_day / total_cases) * 100, 2),   # Day case percentage
    pct_night_cases = round((total_cases_paid_night / total_cases) * 100, 2), # Night case percentage
    
    pct_day_revenue = round((total_revenue_paid_day / total_revenue) * 100, 2), # Day revenue percentage
    pct_night_revenue = round((total_revenue_paid_night / total_revenue) * 100, 2) # Night revenue percentage
  ) %>%
  mutate(across(starts_with("pct_"), ~ replace_na(.x, 0)))  # Replace NaN/NA with 0




# Reorder rows in doctor_summary to ensure the non-contributers appear first
#and total appears last


doctor_summary <- doctor_summary %>%
  arrange(
    case_when(
      names_dr == "Yassir A. Ali" ~ 1,  # Assign priority 1 to "Yassir A. Ali"
      names_dr == "Total" ~ 3,          # Assign priority 2 to "Total"
      TRUE ~ 2                          # Assign priority 3 to all other names
    )
  )

#map the procedure columns to a broader category for use in a heat map
column_categories <- tribble(
  ~col_name,         ~specialty,
  "ext_dc",          "Oral Surgery",
  "ext_pm",          "Oral Surgery",
  "ext_srg",         "Oral Surgery",
  "surgery",         "Oral Surgery",
  "scaling",         "Periodontics",
  "perio_srg",       "Periodontics",
  "endo_dc",         "Endodontics",
  "endo_pm",         "Endodontics",
  "endo_dc_acc",     "Endodontics",
  "endo_pm_acc",     "Endodontics",
  "drsng",           "Endodontics",
  "fl_dc_temp",      "Restorative",
  "fl_dc_amlg",      "Restorative",
  "fl_dc_cmp",       "Restorative",
  "fl_dc_gi",        "Restorative",
  "fl_pm_amlg",      "Restorative",
  "fl_pm_cmp",       "Restorative",
  "fl_pm_gi",        "Restorative")


#Make a proper list including values
df_long <- dataset %>%
  select(
    names_dr,
    all_of(column_categories$col_name)  # only the procedure columns
  ) %>%
  pivot_longer(
    cols      = -names_dr,             # pivot everything except names_dr
    names_to  = "procedure",
    values_to = "count"
  ) %>%
  left_join(
    column_categories,
    by = c("procedure" = "col_name")
  )



#Summarize for later use

df_summary <- df_long %>%
  group_by(names_dr, specialty) %>%
  summarize(
    total_cases = sum(count, na.rm = TRUE),  # total procedures in that specialty
    .groups = "drop"
  )







# Shiny App
ui <- fluidPage(
  # Add padding to the title and center it
  div(
    titlePanel("Interactive Pie Charts"),
    style = "text-align: center; padding-top: 20px;"
  ),
  sidebarLayout(
    sidebarPanel(
      # Checkbox group for selecting doctors
      checkboxGroupInput(
        inputId = "selected_doctors",       # Input ID for dynamic filtering
        label = "Select Doctors to Include:",  # Label for the input
        choices = unique(doctor_summary$names_dr),  # Populate with all unique doctor names
        
        # Remove "Total" from the default selection so it's unchecked
        selected = setdiff(unique(doctor_summary$names_dr), "Total")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Revenue Pie Chart", 
                 div(
                   plotlyOutput("revenuePie"), 
                   textOutput("revenueTotal"),  # Add text output for total revenue
                   style = "text-align: center;"
                 )),
        tabPanel("Cases Pie Chart", 
                 div(
                   plotlyOutput("casesPie"), 
                   textOutput("casesTotal"),  # Add text output for total cases
                   style = "text-align: center;"
              )),
        tabPanel(
          "Case Type Breakdown",
          div(plotlyOutput("stackedBarChart"), style = "text-align: center;"
              )),
        tabPanel("Specialty Heatmap", plotlyOutput("heatmapPlot"
              )),
        tabPanel(
          "Treemap (Doctor Income)",
          div(
            plotlyOutput("treemapPlot"),
            style = "text-align: center;"
            ))
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Render Revenue Pie Chart
  output$revenuePie <- renderPlotly({
    # Filter data based on user selection
    filtered_data <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total") &
          (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors) &
          total_revenue > 0  # Exclude rows with zero revenue
      )
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(plotly_empty())  # Return an empty Plotly chart
    }
    
    # Create a Plotly native pie chart
    plot_ly(
      filtered_data,
      labels = ~names_dr,
      values = ~total_revenue,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = scico::scico(nrow(filtered_data), palette = "romaO"))  # Use scico palette romaO
    ) %>%
      layout(title = "Doctor Contributions to Total Revenue")
  })
  
  # Display Total Revenue
  output$revenueTotal <- renderText({
    total_revenue <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total") &
          (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors)
      ) %>%
      summarize(total = sum(total_revenue, na.rm = TRUE)) %>%
      pull(total)
    
    paste("Total Revenue: $", format(total_revenue, big.mark = ","))
  })
  
  # Render Cases Pie Chart
  output$casesPie <- renderPlotly({
    # Filter data based on user selection
    filtered_data <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total") &
          (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors) &
          total_cases > 0  # Exclude rows with zero cases
      )
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(plotly_empty())  # Return an empty Plotly chart
    }
    
    # Create a Plotly native pie chart
    plot_ly(
      filtered_data,
      labels = ~names_dr,
      values = ~total_cases,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = scico::scico(nrow(filtered_data), palette = "romaO"))  # Use scico palette romaO
    ) %>%
      layout(title = "Doctor Contributions to Total Cases")
  })
  
  # Display Total Cases
  output$casesTotal <- renderText({
    total_cases <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total") &
          (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors)
      ) %>%
      summarize(total = sum(total_cases, na.rm = TRUE)) %>%
      pull(total)
    
    paste("Total Cases: ", format(total_cases, big.mark = ","))
  })
  
  # Render Stacked Bar Chart
  output$stackedBarChart <- renderPlotly({
    # Prepare data for plotting
    filtered_data <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total") &
          (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors)
      ) %>%
      select(names_dr, total_paid, total_free, total_ref) %>%
      pivot_longer(
        cols = c(total_paid, total_free, total_ref),
        names_to = "case_type",
        values_to = "count"
      ) %>%
      mutate(
        case_type = recode(case_type,
                           "total_paid" = "Paid Cases",
                           "total_free" = "Free Cases",
                           "total_ref" = "Referral Cases")
      )
    
    # Check if filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(plotly_empty())  # Return an empty Plotly chart
    }
    
    # Create the stacked bar chart
    stacked_bar <- ggplot(filtered_data, aes(
      x = names_dr,
      y = count,
      fill = case_type,
      text = paste(
        "Doctor:", names_dr,
        "<br>Case Type:", case_type,
        "<br>Count:", count
      )
    )) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(
        title = "Case Type Breakdown by Doctor",
        x = "Doctors",
        y = "Number of Cases",
        fill = "Case Type"
      ) +
      scale_fill_scico_d(palette = "roma") +  # Use scico palette for colors
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert to interactive plotly chart
    ggplotly(stacked_bar, tooltip = "text")
  })
  
  output$heatmapPlot <- renderPlotly({
    # 1) Possibly filter `df_summary` by selected doctors
    filtered_df <- df_summary %>%
      filter(names_dr %in% input$selected_doctors)
    
    # 2) Create plotly heatmap
    plot_ly(
      data = filtered_df,
      x = ~names_dr,
      y = ~specialty,
      z = ~total_cases,
      type = "heatmap",
      colors = "Reds"
    ) %>%
      layout(
        title = "Doctor Specialty Heatmap",
        xaxis = list(title = "Doctor"),
        yaxis = list(title = "Specialty")
      )
  })
  
  output$treemapPlot <- renderPlotly({
    
    # 1) Filter data based on user selection
    filtered_data <- doctor_summary %>%
      filter(
        !names_dr %in% c("Total"),  # Exclude any 'Total' row
        (is.null(input$selected_doctors) | names_dr %in% input$selected_doctors),
        total_owed > 0             # Optional: exclude doctors owed 0
      )
    
    # 2) Check if there's any data left
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data to display. Please select different doctor(s)."))
    }
    
    # 3) Create a Plotly treemap
    plot_ly(
      filtered_data,
      labels = ~names_dr,
      parents = NA,               # Since there's no hierarchy, just set to NA
      values = ~total_owed,
      type = "treemap",
      textinfo = "label+value+percent entry",  # Show the doctor’s name, owed amount, and % of total
      hovertemplate = paste(
        "<b>Doctor:</b> %{label}",
        "<br><b>Owed:</b> $%{value:,.0f}",
        "<extra></extra>"    # Hide secondary box
      ),
      marker = list(
        colors = scico::scico(nrow(filtered_data), palette = "romaO")  
        
      )
    ) %>%
      layout(
        title = "Treemap of Doctor Income",
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  
  
}




#rsconnect::deployApp(
 # appDir       = "D:/University/Digital Health/Patrick-Michael-University",
  #appPrimaryDoc= "project_day_1.R"
#)


shiny::runApp()

