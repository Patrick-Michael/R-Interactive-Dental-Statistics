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
  googleLanguageR,  # Translating functions 
  openxlsx        #Exports into excel
)


dataset_raw <- read_excel("stats_doc_202412.xlsm") #starts the file and renames it for easier manipulation
View(stats_doc_202412)

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
doctor_summary <- doctor_summary %>%
  arrange(
    case_when(
      names_dr == "Yassir A. Ali" ~ 1,  # Assign priority 1 to "Yassir A. Ali"
      names_dr == "Total" ~ 3,          # Assign priority 2 to "Total"
      TRUE ~ 2                          # Assign priority 3 to all other names
    )
  )






