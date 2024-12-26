
install.packages("dplyr")
install.packages("readxls")
install.packages("ggplot")
install.packages("utils")
install.packages("tidyverse")
library(dplyr)
library(tidyr)
install.packages("openxlsx")
library(openxlsx)
write.xlsx(?, file = "//pdc-vm/Docs/Roudy.Sassine/Desktop/nabe")

install.packages("gt")
library(gt)
library(webshot2)

install.packages("ineq")
library(ineq)
library(purrr)


# select relevant data-frame 
cps23 <- cps2023 %>%
  select (PRECORD, # Person record.
          PH_SEQ, # Household seq number
          HHDREL, # Household summary (Householder, spouse, child, relative)
          PEMLR, # Employment status
          A_UNTYPE, # Reason for unemployment
          WEMIND, #Industry of longest job by major industry groups (15 industries)
          A_WKSTAT, #Full/part-time status
          PRWKSTAT, #
          PRPTREA, #Detailed reason for part-time
          A_CLSWKR, #Class of worker
          LJCW, #longest job class of worker
          
          #social:
          A_SEX, #sex
          PRDTRACE, #race
          A_AGE,  #age
          AGE1, #age
          A_HGA,# education attainment
          A_MARITL,#Marital status
          A_CLSWKR,# Class of worker
          A_MJIND,# Major industry code (14 industries, link to wemind)
          A_MJOCC, # Major occupation 
          
          #Geo
          MIG_REG,# region (Northeast, Midwest, South, West)
          MIG_ST,# all 50 states 
          MIG_ST, #Census division of previous year residence
          MIG_REG, #regional 
          MIG_DIV, #Census division of previous year residence
          
          #Major Income types
          WSAL_VAL, # from wage and salaries 
          PTOTVAL, # Total Income
          FICA, #Social security retirement payroll deduction 
          
          #Wealth Income types
          INT_VAL,# income from interest
          RNT_VAL,# Income from rent
          DIV_VAL,# Income from div
          
          A_ERNLWT, #	
          A_FNLWGT,
          MARSUPWT )

# number of rows
RowsNb <- nrow(cps23)

# Delete last row
cps23 <- cps23[-nrow(cps23), ]

# Recoding
cps23 <- cps23 %>%
  mutate(PEMLR = case_when(
    PEMLR == 0 ~ "NIU",
    PEMLR == 1 ~ "Employed - at work",
    PEMLR == 2 ~ "Employed - absent",
    PEMLR == 3 ~ "Unemployed - on layoff",
    PEMLR == 4 ~ "Unemployed - looking",
    PEMLR == 5 ~ "Not in labor force - retired",
    PEMLR == 6 ~ "Not in labor force - disabled",
    PEMLR == 7 ~ "Not in labor force - other"))

cps23 <- cps23 %>%
  mutate(A_MJIND = case_when(
    A_MJIND == 0  ~ "NIU",
    A_MJIND == 1  ~ "Agriculture",
    A_MJIND == 2  ~ "Mining & Gas",
    A_MJIND == 3  ~ "Construction",
    A_MJIND == 4  ~ "Manufacturing",
    A_MJIND == 5  ~ "Whole & Retail",
    A_MJIND == 6  ~ "Transport & Utils",
    A_MJIND == 7  ~ "Information",
    A_MJIND == 8  ~ "Finance Insure & RE",
    A_MJIND == 9  ~ "Prof Scient & Manage",
    A_MJIND == 10 ~ "Educ & Health",
    A_MJIND == 11 ~ "Arts & Entertain" ,
    A_MJIND == 12 ~ "Other",
    A_MJIND == 13 ~ "Public Admin",
    A_MJIND == 14 ~ "Military",
    TRUE          ~ as.character(A_MJIND) 
  ))

cps23 <- cps23 %>%
  mutate(EducLevel = case_when(
    A_HGA == 0  ~ "Children",
    A_HGA %in% 31:38  ~ "Pre-High School",
    A_HGA == 39  ~ "High School Graduate",
    A_HGA == 40 ~ "Some college",
    A_HGA == 41 ~ "Associate degree/Voc",
    A_HGA == 42 ~ "Associate degree/Acad",
    A_HGA == 43  ~ "Bachelor's Degree",
    A_HGA == 44  ~ "Master's Degree",
    A_HGA == 45  ~ "Professional Degree",
    A_HGA == 46  ~ "Doctorate",
    TRUE          ~ as.character(A_HGA)  
  ))


cps23 <- cps23 %>%
  mutate(MIG_DIV = case_when(
    MIG_DIV == 0  ~ "NIU",
    MIG_DIV == 1  ~ "new england",
    MIG_DIV == 2  ~ "middle atlantic",
    MIG_DIV == 3  ~ "east north central",
    MIG_DIV == 4  ~ "west north central",
    MIG_DIV == 5  ~ "south atlantic",
    MIG_DIV == 6  ~ "east south central",
    MIG_DIV == 7  ~ "west south central",
    MIG_DIV == 8  ~ "mountain",
    MIG_DIV == 9  ~ "pacific",
    MIG_DIV == 10  ~ "abroad" ))


cps23 <- cps23 %>%
  mutate(MIG_REG = case_when(
    MIG_REG == 0  ~ "NIU",
    MIG_REG == 1  ~ "northeast",
    MIG_REG == 2  ~ "midwest",
    MIG_REG == 3  ~ "south",
    MIG_REG == 4  ~ "west"
    TRUE          ~ as.character(MIG_REG)  
  ))




# count by employment (145,676)

PopulEmp <- cps23 %>%
  group_by(PEMLR) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(100 * Count / sum(Count), 2)) %>% 
  bind_rows(summarize(., PEMLR = "Total", Count = sum(Count), Prop = 100))%>%
  mutate(Prop = paste0(Prop, "%"))

# relevant table 
table_PopulEmp <- PopulEmp %>%
  gt() %>%
  tab_header(title = md("**Survey Count**")) %>%
  fmt_number(columns = vars(Count), decimals = 0) %>%
  fmt_number(columns = vars(Prop), decimals = 1) %>%
  cols_label(PEMLR = "Status", Count = "Number", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13) %>%
  tab_source_note(source_note = "Table 1")



# re-categorize  
cps23 <- cps23 %>%
  mutate(EmpStatus = case_when(
    PEMLR == "NIU" ~ "Not in Labor",
    PEMLR == "Employed - at work" ~ "Employed",
    PEMLR == "Employed - absent" ~ "Employed",
    PEMLR == "Unemployed - on layoff" ~ "Unemployed",
    PEMLR == "Unemployed - looking" ~ "Unemployed",
    PEMLR == "Not in labor force - retired" ~ "Not in Labor",
    PEMLR == "Not in labor force - disabled" ~ "Not in Labor",
    PEMLR == "Not in labor force - other" ~ "Not in Labor"
  ))

# count by employment status
LFEmp <- cps23 %>%
  group_by(EmpStatus) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(100* Count/sum(Count),2)) %>% 
  bind_rows(summarize(., EmpStatus = "Total", Count = sum(Count), Prop = 100)) %>% 
  mutate(Prop = paste0(Prop, "%"))

# exclude not in labor
cps23 <- cps23 %>%
  filter(EmpStatus != "Not in Labor")

# count by employment status
LFEmp <- cps23 %>%
  group_by(EmpStatus) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(100* Count/sum(Count),2)) %>% 
  bind_rows(summarize(., EmpStatus = "Total", Count = sum(Count), Prop = 100)) %>% 
  mutate(Prop = paste0(Prop, "%"))

# relevant table
table_LFEmp <- LFEmp %>%
  gt() %>%
  tab_header(title = md("**Labor Force**"),
    subtitle = "*Part-time for economic reasons counted as employed") %>%
  fmt_number(columns = vars(Count), decimals = 0) %>%
  fmt_percent(columns = vars(Prop), decimals = 0) %>%
  cols_label(EmpStatus = "Status", Count = "Number", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13) %>%
  tab_source_note(source_note = "Table 2 (tentative)")


# count part-timers for economic reasons as unemployed
cps23 <- cps23 %>%
  mutate(EmpStatusAdjusted = case_when(A_WKSTAT %in% c(3, 5) ~ "Unemployed",  
    TRUE ~ EmpStatus ))

# count by employment status (readjusted: part time for economic reasons as unemployed)
LFEmpAdjusted <- cps23 %>%
  group_by(EmpStatusAdjusted) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(100*Count/sum(Count),2)) %>% 
  bind_rows(summarize(., EmpStatusAdjusted = "Total", Count = sum(Count), Prop = 100)) %>% 
  mutate(Prop = paste0(Prop, "%"))

# relevant table
table_LFEmpAdjusted <- LFEmpAdjusted %>%
  gt() %>%
  tab_header(title = md("**Labor Force**"),
    subtitle = "*Part-time for economic reasons counted as unemployed") %>%
  fmt_number(columns = vars(Count), decimals = 0) %>%
  fmt_percent(columns = vars(Prop), decimals = 0) %>%
  cols_label(EmpStatusAdjusted = "Status", Count = "Number", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13) %>%
  tab_source_note(source_note = "Table 3 (tentative)")

# count by sector 
Sectors <- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(count = n()) %>%
  mutate(Prop = round(100*count/sum(count),2)) %>% 
  bind_rows(summarize(., A_MJIND = "Total", count = sum(count), Prop = 100)) %>%
  mutate(Prop = paste0(Prop, "%"))

# exclude Public Admin, Military and NIU 
cps23 <- cps23 %>%
  filter(!(A_MJIND == "Public Admin" | A_MJIND == "Military" | A_MJIND == "NIU"))

# Count again
Sectors <- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(count = n()) %>%
  mutate(Prop = round(100*count/sum(count),2)) %>% 
  bind_rows(summarize(., A_MJIND = "Total", count = sum(count), Prop = 100)) %>%
  mutate(Prop = paste0(Prop, "%"))


# desired order 
desired_order_sectors <- c("Information", "Finance Insure & RE", "Prof Scient & Manage", "Educ & Health",
                   "Mining & Gas", "Manufacturing", "Construction",  "Transport & Utils", "Agriculture",
                   "Whole & Retail", "Arts & Entertain" ,"Other")

# Convert A_MJIND to a factor with desired order
cps23$A_MJIND <- factor(cps23$A_MJIND, levels = desired_order_sectors)

# count again with desired order
Sectors <- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(count = n()) %>%
  mutate(Prop = round(100*count/sum(count),2)) %>% 
  bind_rows(summarize(., A_MJIND = "Total", count = sum(count), Prop = 100)) %>%
  mutate(Prop = paste0(Prop, "%"))


# relevant table
table_Sectors <- Sectors %>%
  gt() %>%
  tab_header(title = md("**Count by Sector**")) %>%
  fmt_number(columns = vars(count), decimals = 0) %>%
  fmt_percent(columns = vars(Prop), decimals = 0) %>%
  cols_label(A_MJIND = "Sector", count = "No of Workers", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_source_note(source_note = "Table 4" ) %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13)


# Count by sector after the exclusion (Keeping part time workers as employed)
OurPopulation <- cps23 %>%
  group_by(EmpStatus) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(Count / sum(Count) * 100, 2)) %>% 
  bind_rows(summarize(., EmpStatus = "Total", Count = sum(Count), Prop = 100)) %>%
  mutate(Prop = paste0(Prop, "%"))

## re-adjust table 2  as final
table_OurPopulation <- OurPopulation %>%
  gt() %>%
  tab_header(title = md("**Labor Force**"),
             subtitle = "*Part-time for economic reasons counted as employed")%>%
  fmt_number(columns = vars(Count), decimals = 0) %>%
  cols_label(EmpStatus = "Emp Status", Count = "No of People", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_source_note(source_note = "Source: Table 2") %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13)


OurPopulationAdjusted <- cps23 %>%
  group_by(EmpStatusAdjusted) %>%
  summarize(Count = n()) %>% 
  mutate(Prop = round(Count / sum(Count) * 100, 2)) %>% 
  bind_rows(summarize(., EmpStatusAdjusted = "Total", Count = sum(Count), Prop = 100)) %>%
  mutate(Prop = paste0(Prop, "%"))

# re-adjust table 3  as final
table_OurPopulationAdjusted <- OurPopulationAdjusted %>%
  gt() %>%
  tab_header(title = md("**Labor Force**"),
             subtitle = "*Part-time for economic reasons counted as unemployed")%>%
  fmt_number(columns = vars(Count), decimals = 0) %>%
  cols_label(EmpStatusAdjusted = "Emp Status", Count = "No of People", Prop = "Proportion") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_source_note(source_note = "Table 3") %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13)

# formulate table for unemployment rate 
# 1- Count by Sector and employment status
EmpSectorAdjusted <- cps23 %>%
  group_by(A_MJIND, EmpStatusAdjusted) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(A_MJIND) %>%
  mutate(Prop = round(Count / sum(Count), 2)) %>%
  ungroup() %>%
  bind_rows(cps23 %>%
      summarise(A_MJIND = "Total", EmpStatusAdjusted = "Total", Count = n()) %>%
      mutate(Prop = 1))

## 2- remove row Prop
EmpSectorAdjusted <- EmpSectorAdjusted %>% 
  select (! Prop)

## 3- remove last row
EmpSectorAdjusted <- EmpSectorAdjusted %>% slice(1:(n() - 1))

## 4-Pivot the data to wide format
EmpSectorAdjusted_Pivoted <- EmpSectorAdjusted %>%
  pivot_wider(names_from = EmpStatusAdjusted, values_from = Count) %>%
  mutate(`Grand Total` = Employed + Unemployed,
         `Unemployment Rate` = round((Unemployed / `Grand Total`), 2))

## 4- Calculate grand totals
grand_totals <- EmpSectorAdjusted_Pivoted %>%
  summarise(A_MJIND = "Grand Total",
            Employed = sum(Employed, na.rm = TRUE),
            Unemployed = sum(Unemployed, na.rm = TRUE),
            `Grand Total` = sum(`Grand Total`, na.rm = TRUE),
            `Unemployment Rate` = round((sum(Unemployed, na.rm = TRUE) / sum(`Grand Total`, na.rm = TRUE)), 2))
# 5- table
EmpSectorAdjusted_Pivoted <- bind_rows(EmpSectorAdjusted_Pivoted, grand_totals)

# Formulate table for  Unemployment Ratio

# 1-  filter to only include unemployed, count and unemployment rate
unemployment_stats <- cps23 %>%
  filter(EmpStatusAdjusted == "Unemployed") %>%
  group_by(A_MJIND) %>%
  summarise(Unemployed_Count = n()) %>%
  left_join(Sectors, by = "A_MJIND") %>%
  mutate(Unemployed_Percent = (Unemployed_Count / count)) 

unemployment_stats <- unemployment_stats[, -4]

## 2- Unemployment Ratio by sector 
unemployment_stats <- unemployment_stats %>% 
  mutate(Unemployed_Ratio = round(0.03/Unemployed_Percent,2))

### Relevant table
table_unemployment_stats <- unemployment_stats %>%
  gt() %>%
  tab_header(title = md("**Unemployment Rate and Ratio by Sector**"),
            subtitle = "Ratios normalized against the baseline 3%") %>%
  fmt_number(columns = vars(Unemployed_Count, count), decimals = 0) %>%
  fmt_number(columns = vars(Unemployed_Percent, Unemployed_Ratio), decimals = 2) %>%
  cols_label(A_MJIND = "Sector", Unemployed_Count = "#Unemp", count = "Total Labor", 
             Unemployed_Percent = "Unemp Rate", Unemployed_Ratio = "Unemp Ratio") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_source_note(source_note = "Table 5") %>%
  tab_options(table.background.color = "#F5F5F5",  
    table.font.size = 13)


# correlation between wages and unemployment (Result is -0.6)

# 1- median wages grouped by sector
tablex <- cps23 %>% 
  group_by(A_MJIND) %>% 
    summarise(medWages = median(WSAL_VAL))

# 2- join median wages column to previous table
 Employment_statsComb <- unemployment_stats  %>%
   left_join(tablex, by = "A_MJIND")

# 3- take out irrelevant columns
 Employment_statsComb <- Employment_statsComb %>%
   select(Unemployed_Percent, medWages)
 
# 4- Calculate overall correlation
correlation_UnempWages <- cor(Employment_statsComb$Unemployed_Percent, Employment_statsComb$medWages, use = "complete.obs")
 

# Wages/Labor Cost & Labor Cost Ratio by Sector

# 1- Employers pay wages and payroll tax (social security and medicaid)
cps23 <- cps23 %>%
  mutate(SocialSecPay = ifelse(WSAL_VAL <= 160200, 
                               WSAL_VAL * 0.062, 
                               160200 * 0.062))
cps23 <- cps23 %>%
  mutate(MedicaidPay = WSAL_VAL * 0.0145)

cps23 <- cps23 %>%
  mutate(NonWagePay = SocialSecPay + MedicaidPay)

cps23 <- cps23 %>%
  mutate(LaborPay = NonWagePay + WSAL_VAL)


# 2- Wages and laborPay by sector
SectorPay<- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(AvNonWage = mean(NonWagePay),
            MedianNonWage = median(NonWagePay),
            AvWage = mean(WSAL_VAL),
            MedianWage = median(WSAL_VAL),
            AvLaborPay = mean(LaborPay),
            MedianLaborPay = median(LaborPay))
        

#3- Pick Average or median? Gini coefficient by sector
gini_by_sector <- cps23 %>%
  group_by(A_MJIND) %>%
  summarize(Gini = ineq(WSAL_VAL, type = "Gini"))

library("ineq")

## relevant table
table_gini_by_sector <- gini_by_sector %>%
  gt() %>%
  tab_header(title = md("**Gini Coefficient by Sector**")) %>%
  fmt_number(columns = c(A_MJIND, Gini), decimals = 2) %>%
  cols_label(A_MJIND = "Sector", Gini = "Coefficient") %>%
  cols_align(align = "left", columns = c(A_MJIND)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_source_note(source_note = "Table 6") %>%
  tab_options(table.background.color = "#F5F5F5",  
              table.font.size = 13)

# 4 - median wage is 45000 
MedianWage <- median(cps23$WSAL_VAL, na.rm = TRUE)

# 5- median LaborPay is 48442
MedianLaborPay <- median(cps23$LaborPay, na.rm = TRUE)


# 6- Final table
SectorPay <- SectorPay %>%
  mutate(WagePayRatio =  round(MedianWage/45000,2)) #note wage ratio is equal to laborPay Ratio

SectorPay <- SectorPay %>%
  mutate(LaborPayRatio =  round(MedianLaborPay/48442,2)) 

SectorPay <- SectorPay %>% 
  select(-WagePayRatio)

## Relevant table
table_SectorPay <- SectorPay %>%
  gt() %>%
  tab_header(title = md("**Labor Cost & Labor Cost Ratio by Sector**"),
    subtitle = "Ratios Normalized against the baseline of 48,442") %>%
  fmt_number(columns = vars(AvNonWage, MedianNonWage, AvWage, MedianWage, AvLaborPay, MedianLaborPay), decimals = 0) %>%
  fmt_number(columns = vars(LaborPayRatio), decimals = 2) %>%
  cols_label(A_MJIND = "Sector", AvNonWage = "Av Non-Wage Cost", MedianNonWage = "Med Non-Wage Cost",
             AvWage = "Av Wage Cost", MedianWage = "Med Wage Cost", AvLaborPay = "Av Labor Cost", MedianLaborPay = "Med Labor Cost", 
             LaborPayRatio = "Wage/Labor Cost Ratio" ) %>%
  cols_align(align = "left", columns = c(A_MJIND)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  tab_source_note(source_note = "Table 7") %>%
  tab_options(table.background.color = "#F5F5F5", 
    table.font.size = 12.5 )


# education 

#correlation between education and wage

# 1- re-create with numeric values 
cps23 <- cps23 %>%
  mutate(NumericValue = case_when(
    EducLevel == "Pre-High School" ~ 1,
    EducLevel == "High School Graduate" ~ 2,
    EducLevel == "Some college" ~ 3,
    EducLevel == "Associate degree/Acad" ~ 4,
    EducLevel == "Associate degree/Voc" ~ 5,
    EducLevel == "Bachelor's Degree" ~ 6,
    EducLevel == "Master's Degree" ~ 7,
    EducLevel == "Doctorate" ~ 8,
    EducLevel == "Professional Degree" ~ 9,
    TRUE ~ NA_real_  ))

# 2- average wages for each education level
AvWages <- cps23 %>%
  group_by(EducLevel, NumericValue) %>%
  summarise(AverageWage = mean(WSAL_VAL, na.rm = TRUE), .groups = 'drop') %>%
  arrange(NumericValue) %>%
  bind_rows(tibble(EducLevel = "Total", NumericValue = NA, AverageWage = mean(cps23$WSAL_VAL, na.rm = TRUE)))


# 3- overall Correlation (0.9, strong)
Overallcorrelation <- cor(AvWages$NumericValue, AvWages$AverageWage, use = "complete.obs")

# 4- Break down by sector 

sectors <- c("Information", "Finance Insure & RE", "Prof Scient & Manage", "Educ & Health",
                           "Mining & Gas", "Manufacturing", "Construction", "Transport & Utils",
             "Agriculture", "Whole & Retail", "Arts & Entertain" ,"Other")

# 4.1 - function to calculate the correlation 

calculate_correlation <- function(sector) {
  cps23 %>%
    filter(A_MJIND == sector) %>%
    group_by(EducLevel) %>%
    summarise(MedianWage = median(WSAL_VAL, na.rm = TRUE), NumericValue = first(NumericValue), .groups = 'drop') %>%
    summarise(Correlation = cor(NumericValue, MedianWage, use = "complete.obs")) %>%
    mutate(Sector = sector)
}

# 4.1.2 Apply function to each sector to get results
sector_correlations <- map_dfr(sectors, calculate_correlation)

library(purrr)

# 4.1.3 Re-order columns
sector_correlations <- sector_correlations %>%
  select(Sector, Correlation)

##

table_sector_correlations <- sector_correlations %>%
  gt() %>%
  tab_header(title = md("**Correlation between Educ & Wage**")) %>%
  fmt_number(columns = vars(Correlation), decimals = 2) %>%
  cols_label(Sector = "Sector", Correlation = "Correlation Coeff") %>%
  cols_align(align = "left", columns = c(Sector)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_source_note(source_note = "Table 8") %>%
  tab_options(table.background.color = "#F5F5F5",  
              table.font.size = 13)

# Median Wage by Sector & Educational Level

# 1- median wages by Sectior and education
median_wages <- cps23 %>%
  group_by(A_MJIND, EducLevel) %>%
  summarise(Median_Wage = median(WSAL_VAL, na.rm = TRUE)) %>%
  pivot_wider(names_from = EducLevel, values_from = Median_Wage) %>%
  ungroup()

# 2- median by sector (considering all values)
median_by_A_MJIND <- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(`Sector Median` = median(WSAL_VAL, na.rm = TRUE))

# 3- median by education
median_by_EducLevel <- cps23 %>%
  group_by(EducLevel) %>%
  summarise(Median_by_Education = median(WSAL_VAL, na.rm = TRUE)) %>%
  pivot_wider(names_from = EducLevel, values_from = Median_by_Education)

# 4- combine results
final_table <- median_wages %>%
  left_join(median_by_A_MJIND, by = "A_MJIND") %>%
  bind_rows(median_by_EducLevel %>% mutate(A_MJIND = "Educ Median")) %>%
  mutate(`Sector Median` = ifelse(A_MJIND == "Educ Median", NA, `Sector Median`))

# 5- overall median
overall_median <- median(cps23$WSAL_VAL, na.rm = TRUE)

# 6- add overall median to last cell
final_table <- final_table %>%
  mutate(`Sector Median` = ifelse(A_MJIND == "Educ Median", overall_median, `Sector Median`))

# 7- order 
desired_order <- c(
  "A_MJIND", "Pre-High School", "High School Graduate", "Some college",
  "Associate degree/Acad", "Associate degree/Voc", "Bachelor's Degree",
  "Master's Degree", "Doctorate", "Professional Degree", "Sector Median")

# 8- final table
final_table <- final_table %>%
  select(all_of(desired_order))

# relevant neat table
table_final_table <- final_table %>%
  gt() %>%
  tab_header(title = md("**Median Wage by Sector & Educational Level**")) %>%
  fmt_number(columns = 2:ncol(final_table), decimals = 0) %>%
  cols_label(.list = setNames(names(final_table), names(final_table))) %>%
  cols_label(A_MJIND = "Sector/Educ Level  ") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.width = pct(50),  
    table.background.color = "#F5F5F5",  
    table.font.size = 12.5,
    data_row.padding = px(2.5)) %>%
  tab_source_note(source_note = "Table 9")


# Education Wage Ratios (WER)
final_table_ratios <- final_table %>%
  mutate(across(-A_MJIND, ~ round(. / 45000, 2)))


# relevant table
table_final_table_ratios <- final_table_ratios %>%
  gt() %>%
  tab_header(title = md("**Educational Wage Ratios**"),
    subtitle = "Wages normalized against the baseline of $45,000") %>%
  fmt_number(columns = 2:ncol(final_table_ratios),decimals = 2) %>%
  cols_label(.list = setNames(names(final_table_ratios), names(final_table_ratios))) %>%
  cols_label(A_MJIND = "Sector/Educ Level") %>%
  tab_style(style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  tab_options(table.width = pct(50),  
    table.background.color = "#F5F5F5",  
    table.font.size = 12.5,
    data_row.padding = px(2),  
    table_body.hlines.style = "solid", 
    table_body.vlines.style = "none",  
    heading.border.bottom.style = "solid", 
    column_labels.border.bottom.style = "solid") %>% 
  tab_source_note(source_note = "Table 10") %>%
  data_color(columns = c("Pre-High School", "High School Graduate", "Some college",
                "Associate degree/Acad", "Associate degree/Voc", "Bachelor's Degree",
                "Master's Degree", "Doctorate", "Professional Degree", "Sector Median"),
    colors = scales::col_bin(bins = c(0, 0.8, 1, Inf),
    palette = c("#FFDAB9", "#FFFFCC", "#CCFFCC")))  
    

# Median Wage by Sector & Region

# 1- median wages by sector  and region
median_wages_regions <- cps23 %>%
  group_by(A_MJIND, MIG_DIV) %>%
  summarise(Median_Wage = median(WSAL_VAL, na.rm = TRUE)) %>%
  pivot_wider(names_from = MIG_DIV, values_from = Median_Wage) %>%
  ungroup()

# 2- median by  industry  (considering all values)
median_by_A_MJIND <- cps23 %>%
  group_by(A_MJIND) %>%
  summarise(Sector_Median = median(WSAL_VAL, na.rm = TRUE))

# 3- median by region
median_by_MIG_DIV <- cps23 %>%
  group_by(MIG_DIV) %>%
  summarise(Region_Median = median(WSAL_VAL, na.rm = TRUE)) %>%
  pivot_wider(names_from = MIG_DIV, values_from = Region_Median)

# 4- combine results
final_table_regions <- median_wages_regions %>%
  left_join(median_by_A_MJIND, by = "A_MJIND") %>%
  bind_rows(median_by_MIG_DIV %>% mutate(A_MJIND = "Region Median")) %>%
  mutate(Sector_Median = ifelse(A_MJIND == "Region Median", NA, Sector_Median))

#5- overall median
overall_median <- median(cps23$WSAL_VAL, na.rm = TRUE)

# 6- Add overall median to last cell
final_table_regions <- final_table_regions %>%
  mutate(Sector_Median = ifelse(A_MJIND == "Region Median", overall_median, Sector_Median))

# 7- final table
final_table_regions <- final_table_regions %>%
  rename("Sector Median" = Sector_Median)

# relevant table 
table_final_table_regions <- final_table_regions %>%
  gt() %>%
  tab_header(title = md("**Median Wage by Sector & Region**")) %>%
  fmt_number(columns = 2:ncol(final_table_regions), decimals = 0) %>%
  cols_label(.list = setNames(names(final_table_regions), names(final_table_regions))) %>%
  cols_label(A_MJIND = "Sector/Region") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.width = pct(50),  
    table.background.color = "#F5F5F5",  
    table.font.size = 12.5,
    data_row.padding = px(2.5)) %>%
  tab_source_note(source_note = "Table 11") 


# Regional Wage Ratio
final_table_regions_ratios <- final_table_regions %>%
  mutate(across(-A_MJIND, ~ round(. / 45000, 2)))

# relevant neat table
table_final_table_regions_ratios <- final_table_regions_ratios %>%
  gt() %>%
  tab_header(title = md("**Regional Wage Ratios**"),
    subtitle = "Wages normalized against the baseline of $45,000") %>%
  fmt_number(columns = 2:ncol(final_table_regions_ratios),
    decimals = 2) %>%
  cols_label(.list = setNames(names(final_table_regions_ratios), names(final_table_regions_ratios))) %>%
  cols_label(A_MJIND = "Sector/Region") %>%
  tab_style(style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  tab_options(table.width = pct(50),
    table.background.color = "#F5F5F5",  
    table.font.size = 12.5,
    data_row.padding = px(4),
    table_body.hlines.style = "solid",
    table_body.vlines.style = "none",
    heading.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid" ) %>%
  tab_source_note(source_note = "Table 12") %>%
  data_color(columns = c("abroad", "east north central", "east south central", 
                "middle atlantic", "mountain", "new england", "pacific", 
                "south atlantic", "west north central", "west south central", "Sector Median"),
    colors = scales::col_bin(bins = c(0, 0.8, 1, Inf),
      palette = c("#FFDAB9", "#FFFFCC", "#CCFFCC"))) 


# Composite Index

# lINK two tables by A_MJIND
composite_table <- unemployment_stats %>%
  left_join(SectorPay, by = "A_MJIND")

# calculate composite Index

composite_table  <- composite_table  %>%
  mutate(CompositeIndex = (Unemployed_Ratio + LaborPayRatio) / 2) %>% 
  select (A_MJIND, Unemployed_Ratio, LaborPayRatio, CompositeIndex)

# relevant table
table_composite <- composite_table %>%
  gt() %>%
  tab_header(
    title = md("**Composite Ratio Index**")) %>%
  fmt_number(columns = 2:ncol(composite_table), decimals = 2) %>%
  cols_label(
    A_MJIND = "Sector",
    Unemployed_Ratio = "Unemployment Ratio",
    LaborPayRatio = "Wage/Labor Ratio",
    CompositeIndex = "Composite Index") %>%
  cols_align(align = "center",
    columns = 2:ncol(composite_table)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  tab_style(style = cell_text(align = "left"),
    locations = cells_body(columns = vars(A_MJIND))) %>%
  tab_options(
    table.width = pct(50),
    table.background.color = "#F5F5F5",
    table.font.size = 13,
    data_row.padding = px(4),
    table_body.hlines.style = "solid",
    table_body.vlines.style = "none",
    heading.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid" ) %>%
  tab_source_note(
    source_note = md("Table 13")) %>%
  data_color(columns = vars(CompositeIndex),
           colors = scales::col_bin(bins = c(0, 0.8, 1, Inf),
          palette = c("#FFDAB9", "#FFFFCC", "#CCFFCC")))

##end 

