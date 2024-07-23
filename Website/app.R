library(shiny)
library(dplyr)
library(viridis)
library(tigris)
library(leaflet)
library(ggplot2)
library(tidyr)
library(htmltools)
library(showtext)
library(rsconnect)

# Load Datasets
virginia_2018_2022 <- read.csv("data/virginia_2018_2022.csv")
appalachia_2018_2022 <- read.csv("data/appalachia_2018_2022.csv")
chr_sites_2023 <- read.csv("data/chr_sites.csv")
VA_DOD_SEX_1822 <- read.csv("data/VA.DOD.SEX.1822.txt", sep = "\t", header = TRUE)
VA_DOD_AGE_1822 <- read.csv("data/VA.DOD.AGE.1822.txt", sep = "\t", header = TRUE)
VA_DOD_RACE_1822 <- read.csv("data/VA.DOD.RACE.1822.txt", sep = "\t", header = TRUE)
VA_DOD_1822 <- read.csv("data/VA.DOD.1822.txt", sep = "\t", header = TRUE)
US_state_deaths <- read.csv("data/merged_state_death.csv")

#mutate us states
US_state_deaths_avg <- US_state_deaths %>%
  group_by(State) %>%
  summarize(
    mean_DOD_deaths = mean(DOD_deaths, na.rm = TRUE),
    mean_drugs_deaths = mean(drugs_deaths, na.rm = TRUE),
    mean_alcohol_deaths = mean(alcohol_deaths, na.rm = TRUE),
    mean_suicide_deaths = mean(suicide_deaths, na.rm = TRUE),
    mean_Population = mean(Population, na.rm = TRUE)
  ) %>%
  mutate(
    DOD_deaths_per = (mean_DOD_deaths / mean_Population) * 100000,
    drugs_deaths_per = (mean_drugs_deaths / mean_Population) * 100000,
    alcohol_deaths_per = (mean_alcohol_deaths / mean_Population) * 100000,
    suicide_deaths_per = (mean_suicide_deaths / mean_Population) * 100000
  ) %>%
  mutate(max_cause = case_when(
    mean_drugs_deaths > mean_suicide_deaths & mean_drugs_deaths > mean_alcohol_deaths ~ "Drug Overdoses",
    mean_suicide_deaths > mean_drugs_deaths & mean_suicide_deaths > mean_alcohol_deaths ~ "Suicide",
    mean_alcohol_deaths > mean_drugs_deaths & mean_alcohol_deaths > mean_suicide_deaths ~ "Alcohol"
  ))

# Load state boundaries using tigris
states <- states(cb = TRUE)

# Merge summarized deaths data with state boundaries
states_merged <- left_join(states, US_state_deaths_avg, by = c("NAME" = "State"))

#clean Virginia DOD files
VA_DOD_SEX_1822 <- VA_DOD_SEX_1822 %>%
  select(Year,Gender,Crude.Rate) %>%
  na.omit()

VA_DOD_AGE_1822 <- VA_DOD_AGE_1822 %>%
  select(Year,Ten.Year.Age.Groups,Crude.Rate) %>%
  na.omit()

VA_DOD_RACE_1822 <- VA_DOD_RACE_1822 %>%
  select(Year,Single.Race.6,Crude.Rate) %>%
  filter(!Single.Race.6 %in% c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander")) %>%
  na.omit()

# Tigris library to get county shapefiles
options(tigris_use_cache = TRUE)
appalachian_states <- c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN", "VA", "WV")

all_counties <- list()

for (state in appalachian_states) {
  state_counties <- counties(state = state, cb = TRUE)
  all_counties[[state]] <- state_counties
}

us_counties <- do.call(rbind, all_counties)
us_counties$GEOID <- as.integer(us_counties$GEOID)

# Merging shapefiles with Virginia
map_virginia_2018_2022 <- us_counties %>%
  filter(STATEFP == "51") %>%
  right_join(virginia_2018_2022, by = c("GEOID" = "FIPS"), multiple = "all") %>%
  mutate(below_poverty = below_poverty_ratio * 100) %>%
  mutate(DOD_death_per = DOD_death_rate)

# merging shapefiles with appalachia
map_appalachia_2018_2022 <- us_counties %>%
  right_join(appalachia_2018_2022, by = c("GEOID" = "FIPS"), multiple = "all") %>%
  mutate(below_poverty = below_poverty_ratio * 100)

# Define UI
ui <- navbarPage("Deaths of Despair",
                 tags$head(
                   tags$style(HTML("
                   .leaflet-control.legend {
                     max-height: 300px; /* Adjust maximum height as needed */
                     max-width: 150px; /* Adjust maximum width as needed */ 
                   }
                   .navbar {
                     background-color: #85200c; /* Maroon background color for the navbar */
                     border-color: white; /* White border color for the navbar */
                   }
                   .navbar-default .navbar-nav > .active > a {
                     background-color: #a14735; /* Custom color for active tab */
                     color: white; /* White text color for active tab */
                   }
                   .navbar-default .navbar-nav > li > a {
                     color: white; /* White text color for navbar items */
                     border-color: white; /* White border color for navbar items */
                   }
                   .navbar-default .navbar-nav > li > a:hover {
                     background-color: #660000; /* Darker maroon for hover effect */
                     color: white; /* White text color on hover */
                   }
                   .navbar-default .navbar-nav > li > a:focus,
                   .navbar-default .navbar-nav > li > a:active {
                     background-color: #85200c; /* Ensure the background color remains maroon */
                     color: white; /* White text color when tab is clicked */
                   }
                   .navbar-default .navbar-brand {
                     color: white; /* White text color for the brand name */
                     font-family: 'Times New Roman', Times, serif; /* Change font to Times New Roman */
                   }
                   .navbar-default .navbar-brand:hover,
                   .navbar-default .navbar-brand:focus,
                   .navbar-default .navbar-brand:active {
                     color: white; /* Keep text color white on hover, focus, and active states */
                   }
                   .h1, .h2, .h3, .h4, .h5, .h6 {
                     color: white; /* White text color for headers */
                   }
                   "))
                 ),
                 # Overview Dropdown Menu
                 navbarMenu("Overview",
                            tabPanel("Overview", 
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              align = "justify",
                                              column(width = 12,
                                                     fluidRow(
                                                       style = "font-family: 'Times New Roman', Times, serif;",
                                                       align = "center",
                                                       h1(strong("Analyzing Socioeconomic Factors Driving Deaths of Despair"), style = "font-size: 40px;"),
                                                       h4("Data Science for the Public Good Program", style = "font-size: 20px;"),
                                                       h4("Virginia Tech", style = "font-size: 20px;"),
                                                       img(src = "vce_long.jpg", style = "display: inline; margin-right: 5px;", width = "500px;", align = "center"),
                                                     )
                                              ),
                                              column(width = 5,
                                                     fluidRow(
                                                       style = "font-family: 'Times New Roman', Times, serif; margin-left: 50px;",
                                                       div(style = "margin-top: 30px;"),
                                                       h1(style = "text-align: center; font-size: 35px;", "Research Objectives"),
                                                       p(style = "font-size: 16px; text-align: left;", 
                                                         "This research project aims to meticulously investigate the socioeconomic determinants of 
                                                  deaths of despair in Appalachia and Virginia. By thoroughly examining a range of variables, 
                                                  including employment rates, percentage of the population with a bachelor’s degree or higher, 
                                                  demographic factors such as race and age group, median household income, and harm reduction programs,
                                                  the project seeks to uncover the underlying causes and identify potential solutions to this 
                                                  escalating crisis. A critical component of this study involves developing a comprehensive
                                                  understanding of the impacts and prevalence of deaths of despair by considering 
                                                  demographics, particularly in relation to race, age, and education level."
                                                       ),
                                                       div(style = "margin-top: 20px;"),
                                                       p(style = "font-size: 16px;", 
                                                         "Given the rise in deaths of despair, it is essential to address the multifaceted factors 
                                                  driving this crisis. This project aims to provide a detailed, data-driven analysis to inform 
                                                  the development of targeted strategies and interventions that can effectively address the needs 
                                                  of different demographics within the Appalachian region. By doing so, the research seeks to
                                                  contribute to a more holistic and nuanced understanding of the crisis, ultimately guiding public 
                                                  health and policy efforts to mitigate the factors leading to deaths of despair."
                                                       ),
                                                       div(style = "margin-top: 30px;"),
                                                     )
                                              ),
                                              column(width = 7,
                                                     fluidRow(style = "margin-left: 12px;  margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                                              div(style = "margin-top: 30px;"),
                                                              h1(style = "text-align: center; font-size: 35px;", "Deaths of Despair Background"),
                                                              img(src = "dod_factors_graphic.jpg", style = "display: block; float: right; padding: 20px; float: top; padding: 10px;", width = "325px"),
                                                              p(style = "font-size: 16px;", 
                                                                "The phrase 'Deaths of despair' was introduced by Anne Case and Angus Deaton in 2015 to 
                                                                describe fatalities resulting from suicide, drug overdoses, and alcohol-related diseases. 
                                                                This term has gained significant attention as public health researchers and policymakers 
                                                                have observed an alarming increase in such deaths, particularly within specific demographics."
                                                              ),
                                                              div(style = "margin-top: 20px;"),
                                                              p(style = "font-size: 16px;", 
                                                                "Appalachia, a region encompassing parts of 13 states from Southern New York to Northern 
                                                                Mississippi, is profoundly impacted by this crisis. Historically, Appalachia's economy has relied heavily on coal mining, other 
                                                                extractive industries, and manufacturing. The decline of these industries has left the region 
                                                                economically disadvantaged, with substantial job losses and economic instability. The scarcity 
                                                                of alternative employment opportunities has exacerbated the difficulties faced by residents. 
                                                                This economic hardship is often accompanied by social isolation and limited access to healthcare 
                                                                services, further intensifying the risk factors associated with deaths of despair."
                                                              ),
                                                              div(style = "margin-top: 20px;"),
                                                              p(style = "font-size: 16px;", 
                                                                "Public health initiatives in the region have struggled to keep pace with the growing crisis. 
                                                                Efforts to address substance abuse, mental health, and other related issues are often 
                                                                underfunded and lack the necessary resources to be effective. Comprehensive public health 
                                                                strategies are needed to address the multifaceted nature of deaths of despair in Appalachia."
                                                              ),
                                                              div(style = "margin-top: 30px;")
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Overview Dashboard", 
                                     fluidRow(
                                       style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                       align = "justify",
                                       column(width = 12,
                                              h1("Deaths of Despair in the United States", align = "center"),
                                              p(style = "font-size: 16px;",
                                                "The phenomenon of deaths of despair (DOD) has become a critical public health concern in the 
                                         United States, particularly in the Appalachian region. We aim to explore the prevalence, causes, 
                                         and geographical distribution of these deaths, with a specific focus on the Appalachian states. 
                                         The analysis draws on data from 2018 to 2022 to uncover trends and correlations that can inform 
                                         targeted interventions and policies.")
                                       ),
                                     ),
                                     fluidRow(
                                       style = "font-family: 'Times New Roman', Times, serif;",
                                       align = "center",
                                       column(width = 7,
                                              selectInput("death_type", "Select Death Type:",
                                                          choices = list("Overall DOD" = "DOD_deaths_per",
                                                                         "Overdose Deaths" = "drugs_deaths_per",
                                                                         "Alcohol Deaths" = "alcohol_deaths_per",
                                                                         "Suicide Deaths" = "suicide_deaths_per")),
                                              leafletOutput("usmap1", height = 400)
                                       ),
                                       column(width = 5,
                                              align = "justify",
                                              div(style = "margin-top: 30px;"),
                                              h3("Deaths of Despair Rates by State"),
                                              p(style = "font-size: 16px; margin-right: 50px;",
                                                "The map illustrates the average rates of deaths of despair per 
                                                100,000 people by state from 2018 to 2022. This visualization highlights 
                                                the geographic concentration of higher DOD rates, particularly in the 
                                                north-central and central Appalachian states. Notably, West Virginia and 
                                                New Mexico emerge as the states with the highest DOD rates, each driven 
                                                by different primary causes."),
                                              div(style = "margin-top: 20px;"),
                                              p(style = "font-size: 16px; margin-right: 50px;",
                                                "Here you can navigate between different visualizations to explore 
                                                DOD rates by state as well as rates for each component of DOD, 
                                                including suicide, alcohol-related diseases, and drug overdoses. 
                                                This interactive feature allows for a more detailed understanding 
                                                of the specific factors contributing to deaths of despair in various regions."),
                                              div(style = "margin-top: 20px;"),
                                              p(style = "font-size: 16px;",
                                                tags$strong("Key findings: ")),
                                              div(style = "margin-top: 0px;"),
                                              p(style = "font-size: 16px;",
                                                "West Virginia: Has the highest drug overdose rate in the country."),
                                              div(style = "margin-top: 0px;"),
                                              p(style = "font-size: 16px;",
                                                "New Mexico: Has the highest alcohol death rate."),
                                              div(style = "margin-top: 0px;"),
                                              p(style = "font-size: 16px;",
                                                "Wyoming: Has the highest suicide death rate.")
                                       )
                                     ),
                                     fluidRow(
                                       style = "font-family: 'Times New Roman', Times, serif;",
                                       align = "justify",
                                       column(width = 7,
                                              div(style = "margin-top: 20px;"),
                                              leafletOutput("usmap2", height = 400)
                                       ),
                                       column(width = 5,
                                              align = "justify",
                                              h3("Most Prevalent Component of DOD by State"),
                                              div(style = "margin-top: 20px;"),
                                              p(style = "font-size: 16px; margin-right: 50px;",
                                                "A comprehensive analysis of the most prevalent cause of deaths of despair 
                                                across the United States from 2018 to 2022 reveals stark regional differences.
                                                The map indicates that drug overdoses are the leading cause of DOD in all 
                                                Appalachian states. This finding underscores the significant impact of the 
                                                opioid crisis and other substance abuse issues in this region. States outside 
                                                of Appalachia show a varied pattern, with some states like New Mexico displaying 
                                                a higher prevalence of alcohol-related deaths, highlighting the regional 
                                                specificity of substance abuse issues.")
                                       )
                                     ),
                                     div(style = "margin-top: 30px;")
                            )
                 ),
                 
                 # Virginia Dropdown Menu
                 navbarMenu("Virginia",
                            tabPanel("Factors Dashboard", 
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              column(width = 12,
                                                     offset = 0,
                                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; text-align: center;",
                                                              align = "center",
                                                              h1(style = "text-align: center;", "Deaths of Despair Factors in Virginia"),
                                                              p(style = "font-size: 16px; text-align: left;", "The 'Factors' tab examines critical socioeconomic indicators, which we refer to as factors,
                                                                such as educational attainment (Bachelor's or higher), unemployment rates, mean and median household
                                                                income, and the percentage of people living in poverty. These factors play a significant role
                                                                in shaping the living conditions and opportunities within a community. For instance, higher
                                                                education levels can lead to better job prospects and higher income, while high unemployment
                                                                and poverty rates can increase stress and reduce access to essential resources.
                                                                These findings underscore the critical influence of socioeconomic factors on the risk of deaths 
                                                                of despair, highlighting the need for targeted interventions to support vulnerable populations.")
                                                     )
                                              )
                                     ),
                                     fluidRow(style = "margin: 12px; font-family: 'Times New Roman', Times, serif;",
                                              # Maps side by side
                                              column(width = 5,
                                                     fluidRow(
                                                       align = "center",
                                                       h4(style = "font-weight: normal; text-align: center;", "Virginia Socioeconomic Factors (2018-2022)"),
                                                       div(style = "margin-top: 10px; margin-left: 10px;"),
                                                       # Leaflet map output
                                                       leafletOutput("map_virginia", width = "98%", height = "400px") 
                                                     )
                                              ),
                                              column(width = 5,
                                                     fluidRow(
                                                       align = "center",
                                                       h4(style = "font-weight: normal; text-align: center;", "Virginia Deaths of Despair Rate (2018-2022)"),
                                                       div(style = "margin-top: 10px;"),
                                                       # Leaflet map output
                                                       leafletOutput("map_another", width = "98%", height = "400px")
                                                     )
                                              ),
                                              column(width = 2,
                                                     fluidRow(
                                                       align = "center",
                                                       div(style = "margin-top: 50px; margin-right: 10px;"),
                                                       # Dropdown for year selection
                                                       selectInput("year_virginia", label = "Select Year",
                                                                   choices = unique(map_virginia_2018_2022$Year),
                                                                   selected = 2018),
                                                       div(style = "margin-top: 20px; margin-left: 0px; margin-right: 40px;"),
                                                       selectInput("variable_virginia", label = "Select Factor",
                                                                   choices = c("Percentage with Bachelor's or Higher", "Percentage Unemployed", "Mean Household Income", "Median Household Income", "Percentage in Poverty"),
                                                                   selected = "BA or Higher")
                                                     )
                                              )
                                     ),
                                     fluidRow(style = "margin: 12px; font-family: 'Times New Roman', Times, serif;",
                                              # Correlation plot section
                                              column(width = 8,
                                                     offset = 0,
                                                     fluidRow(
                                                       align = "center",
                                                       # Dropdown for correlation variable selection
                                                       selectInput("variable_correlation", label = "Select Variable for Correlation",
                                                                   choices = c("Mean Household Income", "Median Household Income", "Percentage Unemployed", "BA or Higher Ratio", "Percentage in Poverty"),
                                                                   selected = "BA or Higher Ratio"),
                                                       h4(style = "font-weight: normal; text-align: center;", "Virginia DOD Factor vs Deaths of Despair Rate per 100,000 (2018-2022)"),
                                                       div(style = "margin-top: 10px;"),
                                                       plotOutput("corr_plot", width = "96%", height = "500px")
                                                     )
                                              ),
                                              
                                              column(width = 4,
                                                     offset = 0,
                                                     fluidRow(
                                                       align = "justify",
                                                       div(style = "margin-top: 110px;"),
                                                       p(style = "font-size: 16px; margin-right: 50px;", 
                                                         "Research indicates significant correlations between specific socioeconomic factors and deaths of 
                                                despair (DOD). Populations with a bachelor's degree (BA) or higher and higher household incomes are 
                                                less susceptible to deaths of despair compared to those with lower levels of education and lower economic status."
                                                       ),
                                                       div(style = "margin-top: 20px;"),
                                                       p(style = "font-size: 16px; margin-right: 50px;", 
                                                         "This trend is evident in highly educated and more economically advantaged areas such as Arlington, 
                                                Fairfax, and Loudoun County in Virginia, where the deaths of despair rates are relatively low. In 
                                                contrast, counties such as Lee, Buchanan, Dickenson, Roanoke, and Petersburg, among others exhibit 
                                                higher rates of deaths of despair and lower levels of education and income. "
                                                       ),
                                                       div(style = "margin-top: 20px;"),
                                                       p(style = "font-size: 16px; margin-right: 50px;", 
                                                         "Conversely, higher rates of deaths from despair are positively correlated with unemployment and 
                                                poverty. Individuals unemployed or living below the poverty line face greater challenges that 
                                                contribute to increased rates of substance abuse, depression, and suicide. "
                                                       )
                                                     ),
                                                     div(style = "margin-top: 30px;")
                                              )
                                     )
                            ),
                            tabPanel("Demographics Dashboard", 
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                              column(width = 12,
                                                     fluidRow(style = "text-align: center;",
                                                              h1("Virginia Demographics Dashboard"),
                                                              p(style = "font-size: 16px; text-align: left;", 
                                                                "The 'Demographics' tab provides a comprehensive overview of how deaths of despair 
                                                                rates vary by age, race, and sex. Additionally, it explores educational attainment 
                                                                (BA or higher) and poverty percentages across these demographic groups. This analysis 
                                                                helps identify which populations are most affected and reveals patterns that can inform 
                                                                targeted interventions and policies.")
                                                     )
                                              ),
                                              column(width = 8,
                                                     offset = 0,
                                                     fluidRow(
                                                       h4(style = "font-weight: normal; text-align: center;", "Deaths of Despair by Demographic (2018-2022)"),
                                                       selectInput("demographic_selector", "Demographic:",
                                                                   choices = c("Age" = "Ten.Year.Age.Groups",
                                                                               "Race" = "Single.Race.6",
                                                                               "Sex" = "Gender",
                                                                               "DOD total" = "DOD total"),
                                                                   selected = "Ten.Year.Age.Groups"
                                                       )
                                                     ),
                                                     plotOutput("plot_output", height = "400px")
                                              ),
                                              column(width = 4,
                                                     offset = 0,
                                                     fluidRow(
                                                       div(style = "margin-top: 100px;"),
                                                       p(style = "font-size: 16px; text-align: justify;",
                                                         "Research indicates a correlation between deaths of despair (DOD) and various demographic factors, 
                                                         including age, race, and sex. As visualized, certain factors can be identified as particularly 
                                                         vulnerable to these outcomes. Some notable observations are: ",
                                                         tags$ul(
                                                           tags$li(style = "font-size: 16px; text-align: justify;",
                                                                   strong("Age: "), "DOD rate highest for the older population ages 54-64. 
                                                                   The sharpest increase among 35-44 age group, raises concerns for younger adults."),
                                                           tags$li(style = "font-size: 16px; text-align: justify;",
                                                                   strong("Race: "), "DOD rates for African American peaked in 2022, surpassing 
                                                                   White individuals in 2021. All 3 populations spiked in 2020, likely due to COVID-19 pandemic."),
                                                           tags$li(style = "font-size: 16px; text-align: justify;",
                                                                   strong("Sex: "), "Males have significantly higher rates than females consistently. 
                                                                   DOD rate for males was 90 in 2018 and 123 in 2022. Rate for females was 33 in 2018 and 45 in 2022.")
                                                         )
                                                       ),
                                                       p(style = "font-size: 16px; text-align: justify;",
                                                         "These correlations emphasize the complex interplay between demographic characteristics in 
                                                       determining the risk of deaths of despair.")
                                                     )
                                              )
                                     ),
                                     fluidRow(style = "margin: 12px; font-family: 'Times New Roman', Times, serif;",
                                              column(width = 12, style = "text-align: center;",
                                                     selectInput("demographic_selector2", "Demographic:",
                                                                 choices = c("Age" = "Age",
                                                                             "Race" = "Race",
                                                                             "Sex" = "Gender"),
                                                                 selected = "Age")
                                              )
                                     ),
                                     fluidRow(style = "margin: 12px; font-family: 'Times New Roman', Times, serif;",
                                              column(width = 6,
                                                     fluidRow(style = "text-align: center;",
                                                              h4(style = "font-weight: normal; text-align: center;", "Percentage with Bachelor's or Higher by Demographic (2018-2022)")),
                                                     plotOutput("plot_ba_higher_ratio", height = "400px"),
                                                     div(style = "margin-top: 60x;"),
                                                     p(style = "font-size: 16px; text-align: left",
                                                       "There is a significant correlation between educational attainment and deaths of despair. Individuals with a bachelor’s degree 
                                                       (BA) or higher are generally less likely to die from causes categorized as deaths of despair compared to those with lower educational levels."),
                                                     tags$ul(
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "Since 2018, all age groups have seen an increase in obtaining a degree with 25-34 being the highest."),
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "The Asian/Pacific population has maintained the highest rate of those who have a BA or higher."),
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "More females have a BA in comparison to men.")
                                                     )
                                              ),
                                              column(width = 6,
                                                     fluidRow(style = "text-align: center;",
                                                              h4(style = "font-weight: normal; text-align: center;", "Percentage in Poverty by Demographic (2018-2022)")),
                                                     plotOutput("plot_poverty_ratio", height = "400px"),
                                                     div(style = "margin-top: 60x;"),
                                                     p(style = "font-size: 16px; text-align: left; margin-right: 50px;",
                                                       "Poverty is another critical factor influencing deaths of despair. Higher percentages of individuals living in poverty are 
                                                       associated with increased rates of these deaths.",
                                                       tags$ul(
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "As one ages, the likelihood of poverty consistently reduces."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "Black people have the highest rates of poverty followed by Native Americans."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "Females have higher poverty rates compared to men.")
                                                       )
                                                     )
                                              )
                                     ),
                                     div(style = "margin-top: 30px;")
                            ),
                            tabPanel("Harm Reduction", 
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              align = "justify",
                                              column(12,
                                                     h1(style = "text-align: center", "Harm Reduction"),
                                                     div(style = "margin-top: 10px;")
                                              )
                                     ),
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              align = "justify",
                                              column(width = 12,
                                                     p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", 
                                                       "Harm reduction refers to an approach that encompasses a variety of intentional practices, 
                                                        public health policies, and interventions aimed at minimizing the negative health, social, 
                                                        and legal impacts associated with drug use and other behaviors, whether legal or illegal. 
                                                        It seeks to reduce harm through practical measures, such as providing life-saving tools and 
                                                        information, ensuring access to safer-use supplies, and support services. A core principle of 
                                                        harm reduction is acknowledging every individual's right to safety and dignity, and rejecting 
                                                        the notion that drug use is a personal shortcoming. Instead, it focuses on engaging people who 
                                                        use drugs, offering them the resources and respect needed to create positive change and improve 
                                                        their overall well-being.")
                                              )
                                     ),
                                     tabsetPanel(
                                       tabPanel("Programs",       
                                                fluidRow(style = "margin-left: 100px; margin-right: 100px; font-family: 'Times New Roman', Times, serif;",
                                                         align = "justify",
                                                         column(12,
                                                                div(style = "margin-top: 10px;"), 
                                                                h3(style = "text-align: center; font-family: 'Times New Roman', Times, serif;", "Programs")
                                                         )
                                                ),
                                                fluidRow(style = "font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                         column(7,
                                                                h3(style = "text-align: left; margin-left: 18px; font-family: 'Times New Roman', Times, serif;", "Health Professional Shortage Area (HPSA)"),
                                                                p(style = "text-align: left; font-size: 16px; margin-left: 18px; margin-right: 10px; font-family: 'Times New Roman', Times, serif;", 
                                                                  "A Health Professional Shortage Area is a designated region or group with a shortage of primary, dental, or mental health care providers. 
                                                                  Counties are classified with a code of 1, 2, and 3 indicating whether none, part, or the whole county faces a shortage."),
                                                                div(style = "margin-top: 20px;"),
                                                                h4(style = "text-align: center; font-family: 'Times New Roman', Times, serif;", "Location of Harm Reduction Programs with Health Professional Shortage Area Codes (HPSA) in 2022"),
                                                                leafletOutput("chr_sites_map", width = "100%", height = "500px"),
                                                                div(style = "margin-top: 5px;"),
                                                                p(style = "text-align: center;", em("Harm reduction intervention programs are limited in counties with an existing shortage of health care providers.")),
                                                                div(style = "margin-top: 20px;")
                                                         ),
                                                         column(5,
                                                                div(style = "margin-top: 20px; "),
                                                                h3(style = "text-align: left; font-family: 'Times New Roman', Times, serif;", "Virginia Harm Reduction Coalition"),
                                                                p(style = "text-align: left; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                                  "The Virginia Harm Reduction Coalition improves public health by providing safer-use supplies, 
                                                           social services to marginalized populations, advocating for progressive health policies, 
                                                           and collaborating with other agencies to deliver essential public health services."),
                                                                div(style = "margin-top: 20px; "),
                                                                h3(style = "text-align: left; font-family: 'Times New Roman', Times, serif;", "Comprehensive Harm Reduction (CHR) Program"),
                                                                div(style = "margin-top: 20px;"),
                                                                p(style = "text-align: left;margin-right: 50px; text-align: left; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                                  "Comprehensive Harm Reduction (CHR) programs, or often known as needle exchange sites, offer a range of services to support 
                                                           individuals and communities in reducing harm associated with substance use. These services include:",
                                                                  tags$ul(
                                                                    style = "text-align: left; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                                    tags$li(strong("Sterile Needle and Syringe Distribution: "), " Providing sterile hypodermic needles and syringes to prevent the spread of infectious diseases."),
                                                                    tags$li(strong("Safe Disposal: "), "Ensuring the safe disposal of used needles and syringes to protect community health."),
                                                                    tags$li(strong("Naloxone Distribution: "), "Offering Naloxone to reverse opioid overdoses."),
                                                                    tags$li(strong("Education: "), "Delivering education on safer use practices, overdose prevention, and harm reduction strategies."),
                                                                    tags$li(strong("Peer Support: "), "Facilitating peer support networks to provide a sense of a community."),
                                                                    tags$li(strong("HIV and Hepatitis Testing: "), "Conducting HIV and hepatitis testing to identify and manage infections early."),
                                                                    tags$li(strong("Referrals: "), "Providing referrals to drug treatment programs and medical care to support recovery and overall well-being.")
                                                                  )
                                                                ),
                                                         )
                                                ),
                                                div(style = "margin-top: 30px;"),
                                       ),
                                       tabPanel("Legislation",
                                                fluidRow(style = "margin-left: 100px; margin-right: 100px; font-family: 'Times New Roman', Times, serif;",
                                                         align = "justify",
                                                         column(12,
                                                                h2(style = "text-align: center", "Legislation"),
                                                                div(style = "margin-top: 10px;")
                                                         )
                                                ),
                                                # Add content for Legislation tab here
                                                fluidRow(
                                                  column(12,
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", 
                                                           "In recent years, both federal and state governments have implemented various legislative measures to address the 
                                                            escalating opioid crisis. These legislative measures at both levels reflect a comprehensive approach to combatting 
                                                            the opioid crisis. They emphasize increasing access to naloxone, supporting harm reduction strategies, and 
                                                            addressing opioid abuse through community-based initiatives and legal protections to reduce the overall death 
                                                            rate from overdose.")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(6,
                                                         h3(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;", "Federal"),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           strong("FDA Approval of Over-the-Counter Narcan Nasal Spray (March 2023):")),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           "The FDA approved the first over-the-counter Narcan nasal spray, making it more accessible for individuals 
                                                            to obtain this life-saving medication without a prescription."),
                                                         p(style = "margin-left: 50px; margin-right: 50px;font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           strong("S.2796 - Rural Opioid Abuse Prevention Act (December 20, 2022):")),
                                                         p(style = "margin-left: 50px; margin-right: 50px;font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           "This act provides grants for pilot programs in rural areas focused on reducing opioid overdose deaths. 
                                                            It encourages alternatives to incarceration and aims to enhance community support systems."),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           strong("Biden-Harris Administration Initiatives (2020-2024):")),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           "This administration expanded access to not only naloxone, but also other harm reduction interventions. 
                                                            It permitted federal funding for state and local health departments to purchase naloxone, facilitated 
                                                            state development of naloxone saturation plans, and streamlined processes for harm reduction programs 
                                                            to obtain and distribute naloxone. These efforts have contributed to a decline or stabilization in 
                                                            overdose reports according to CDC data.")
                                                  ),
                                                  column(6,
                                                         h3(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;", "Virginia"),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           strong("Naloxone Prescription Requirement (June 2023):")),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           "Virginia requires providers to prescribe naloxone alongside opioid prescriptions “when risk factors 
                                                            of prior overdose, substance misuse, doses in excess of 120 MME/day, or concomitant benzodiazepine 
                                                            are present” (18VAC85-21-40). This measure aims to ensure that patients at risk of overdose or their 
                                                            caregivers have immediate access to naloxone."),
                                                         p(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           strong("Good Samaritan Law:")),
                                                         p(style = " margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                                           "Virginia's Good Samaritan Law provides civil immunity to individuals who render emergency assistance in good faith 
                                                            to any person who is in need of medical attention. Administering Naloxone is included in this law. This protects 
                                                            bystanders who help those in need without the fear of legal repercussions, potentially saving lives.")
                                                  )
                                                ),
                                                div(style = "margin-top: 30px;")
                                       )
                                     )
                            )
                 ),
                 
                 # Appalachia Dropdown Menu
                 navbarMenu("Appalachia",
                            tabPanel("Factors Dashboard", 
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              # Left side content (if any)
                                              column(width = 12,
                                                     offset = 0,
                                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; text-align: center;",
                                                              align = "center",
                                                              h1(style = "text-align: center;", "Deaths of Despair Factors in Appalachia"),
                                                              p(style = "font-size: 16px; text-align: left;", "The 'Factors' tab examines critical socioeconomic indicators, which we refer 
                                                                to as factors, such as educational attainment (Bachelor's or higher), unemployment rates, 
                                                                mean household income, and the percentage of people living in poverty. These 
                                                                factors play a significant role in shaping the living conditions and opportunities 
                                                                within a community. For instance, higher education levels can lead to better job 
                                                                prospects and higher income, while high unemployment and poverty rates can increase 
                                                                stress and reduce access to essential resources. By analyzing these factors, we aim 
                                                                to understand how they influence the rates of deaths of despair in the Appalachian 
                                                                region, shedding light on the broader social and economic context of this public health issue."),
                                                              p(style = "font-size: 16px; text-align: left;", em("Appalachia encompasses counties within the following states:
                                                                Alabama, Georgia, Kentucky, Maryland, Mississippi, New York, North Carolina, Ohio, Pennsylvania,
                                                                South Carolina, Tennessee, Virginia, and West Virginia."))
                                                     )
                                              )
                                     ),
                                     fluidRow(style = "margin-left: 20px; margin-right: 20px; font-family: 'Times New Roman', Times, serif;",
                                              # Maps side by side
                                              column(width = 5,
                                                     fluidRow(
                                                       align = "center",
                                                       h4(style = "font-weight: normal; text-align: center;", "Appalachia Socioeconomic Factors (2018-2022)"),
                                                       div(style = "margin-top: 10px; margin-left: 10px;"),
                                                       # Leaflet map output
                                                       leafletOutput("map_app_factors", width = "98%", height = "400px")
                                                     )
                                              ),
                                              column(width = 5,
                                                     fluidRow(
                                                       align = "center",
                                                       h4(style = "font-weight: normal; text-align: center;", "Appalachia Deaths of Despair Rate (2018-2022)"),
                                                       div(style = "margin-top: 10px;"),
                                                       # Leaflet map output
                                                       leafletOutput("map_app_dod", width = "98%", height = "400px")
                                                     )
                                              ),
                                              column(width = 2,
                                                     fluidRow(
                                                       align = "center",
                                                       div(style = "margin-top: 50px;"),
                                                       # Dropdown for year selection
                                                       selectInput("year_appalachia", label = "Select Year",
                                                                   choices = unique(map_appalachia_2018_2022$Year),
                                                                   selected = 2018),
                                                       div(style = "margin-top: 20px; margin-left: 0px; margin-right: 40px;"),
                                                       selectInput("variable_appalachia", label = "Select Factor",
                                                                   choices = c("Percentage with Bachelor's or Higher", "Percentage Unemployed", "Mean Household Income", "Percentage in Poverty"),
                                                                   selected = "BA or Higher")
                                                     )
                                              )
                                     ),
                                     #correlation plot here
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px;margin-top:20px; font-family: 'Times New Roman', Times, serif;",
                                              # Correlation plot section
                                              column(width = 8,
                                                     offset = 0,
                                                     fluidRow(
                                                       align = "center",
                                                       # Dropdown for correlation variable selection
                                                       selectInput("app_variable_correlation", label = "Select Variable for Correlation",
                                                                   choices = c("Mean Household Income", "Percentage Unemployed", "BA or Higher Ratio", "Percentage in Poverty"),
                                                                   selected = "BA or Higher Ratio"),
                                                       h4(style = "font-weight: normal; text-align: center;", "Appalachia DOD Factor vs Deaths of Despair Rate per 100,000 (2018-2022)"),
                                                       div(style = "margin-top: 20px;"),
                                                       plotOutput("app_corr_plot", width = "96%", height = "500px")
                                                     )
                                              ),
                                              
                                              column(width = 4,
                                                     offset = 0,
                                                     fluidRow(
                                                       align = "justify",
                                                       div(style = "margin-top: 100px;"),
                                                       p(style = "font-size: 16px;", 
                                                         "There are significant correlations between specific socioeconomic factors and deaths 
                                                         of despair (DOD) in Appalachia comparative to the rest of the United States. There is 
                                                         a clear correlation showing that as the percentage of individuals with a bachelor's 
                                                         degree or higher increases, DOD rates decrease. DOD rates also show a consistent decline 
                                                         with higher median household income levels. These trends highlight the protective effect 
                                                         that higher education and higher income has against deaths of despair."
                                                       ),
                                                       div(style = "margin-top: 20px;"),
                                                       p(style = "font-size: 16px;", 
                                                         "Conversely, areas with higher poverty rates tend to experience elevated DOD rates. 
                                                         Furthermore, there is a concentrated relationship observed between unemployment rates 
                                                         and DOD. Counties experiencing unemployment rates ranging from 2% to 4.5% tend to 
                                                         report higher rates of deaths from despair. Overall, these findings underscore the 
                                                         critical influence of socioeconomic factors on the risk of deaths of despair in Appalachia, 
                                                         emphasizing the need for targeted interventions to support vulnerable populations."
                                                       )
                                                     )
                                              )
                                     ),
                                     div(style = "margin-top: 30px;")
                            ),
                            tabPanel("Demographics Dashboard", 
                                     fluidRow(style = "text-align: center; font-family: 'Times New Roman', Times, serif;",
                                              align = "center",
                                              h1("Appalachia Demographics Dashboard"),
                                              p(style = "font-size: 16px; text-align: left; margin-left: 50px; margin-right: 50px;",
                                                "Recent data from 2018 to 2022 reveals important trends in socioeconomic factors affecting educational attainment and poverty rates across Appalachia. 
                                                These trends highlight the ongoing changes in socioeconomic factors across Appalachia, indicating the importance of continuing efforts to support 
                                                educational and economic advancements for all demographic groups."),
                                              p(style = "font-size: 16px; text-align: left; margin-left: 50px; margin-right: 50px;", "The 'Demographics' tab explores educational attainment (BA or higher) and poverty percentages across these demographic groups. This analysis helps 
                                                identify which populations are most affected and reveals patterns that can inform targeted interventions and policies. It is important to note that 
                                                there is no overview of how deaths of despair rates vary by age, race, and sex due to suppression of data. This is because several counties have 
                                                suppressed their data on deaths of despair to protect victims' identities and reduce stigma.")
                                     ),
                                     fluidRow(style = "font-family: 'Times New Roman', Times, serif;",
                                              column(width = 2,
                                                     offset = 5,
                                                     style = "text-align: center;",
                                                     selectInput("demographic_selector3", "Demographic:",
                                                                 choices = c("Age" = "Age",
                                                                             "Race" = "Race",
                                                                             "Sex" = "Gender"),
                                                                 selected = "Age")
                                              )
                                     ),
                                     fluidRow(style = "margin: 12px; font-family: 'Times New Roman', Times, serif;",
                                              column(width = 6,
                                                     fluidRow(style = "text-align: center;",
                                                              h4(style = "font-weight: normal; text-align: center;", "Appalachia Percentage with Bachelor's or Higher by Demographic (2018-2022)")),
                                                     plotOutput("app_plot_ba_higher_ratio", height = "400px"),
                                                     div(style = "margin-top: 60x;"),
                                                     p(style = "font-size: 16px; text-align: left;",
                                                       "There are several important insights to consider regarding educational attainment in Appalachia."),
                                                     tags$ul(
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "There is a continuous increase in both females and males receiving a bachelor’s degree (BA) or higher."),
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "All age groups have seen an increase in obtaining a BA or higher, with the 35-44 age group experiencing the highest growth."),
                                                       tags$li(style = "font-size: 16px; text-align: left;",
                                                               "Asian/Pacific Islanders have the highest rates of achieving a BA or higher, with Black, Native Americans, and White populations showing slight increases.")
                                                     )
                                              ),
                                              column(width = 6,
                                                     fluidRow(style = "text-align: center;",
                                                              h4(style = "font-weight: normal; text-align: center;", "Appalachia Percentage in Poverty by Demographic (2018-2022)")),
                                                     plotOutput("app_plot_poverty_ratio", height = "400px"),
                                                     div(style = "margin-top: 60x;"),
                                                     p(style = "font-size: 16px; text-align: left;",
                                                       "There are several key takeaways when addressing poverty rates in Appalachia.",
                                                       tags$ul(
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "There is a consistent decline in poverty rates for both females and males, with poverty rates for men stabilizing."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "The age group 25-34 has the highest poverty rates but has been declining over the years."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "Poverty rates for individuals aged 65+ have been rising."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "African Americans and Natives Americans are the most affected by poverty, although their rates have slightly decreased."),
                                                         tags$li(style = "font-size: 16px; text-align: left;",
                                                                 "Asian poverty rates saw a slight increase in 2021.")
                                                       )
                                                     )
                                              )
                                     ),
                                     div(style = "margin-top: 30px;")
                            )
                 ),
                 
                 # Analysis Dropdown Menu
                 navbarMenu("Analysis",
                            tabPanel("Methodology", 
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                              align = "center",
                                              h1("Methodology"),
                                              div(style = "margin-top: 30px;"),
                                              h4(strong("Research Question"), align = "left"),
                                              p(style = "font-size: 16px; text-align: left;", 
                                                "How do socioeconomic factors contribute to the rise in deaths of despair 
                                                among various demographics in Virginia from 2018 to 2022?"),
                                              div(style = "margin-top: 30px; "),
                                              h4(strong("Objectives"), align = "left"),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "Our primary objective is to identify significant correlates of deaths of despair. By determining 
                                                potential risk factors and the populations most affected by deaths of despair, we highlight the 
                                                need for targeted interventions and directions for future research."),
                                              div(style = "margin-top: 30px; "),
                                              h4(strong("Data"), align = "left"),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "We utilized county-level socioeconomic data from the 5-year American Community Survey (ACS) and Multiple Cause of 
                                                Death (MCOD) data from the CDC WONDER database to determine the county-level DOD death rates in Virginia from 2018 to 2022."),
                                              div(style = "margin-top: 30px; "),
                                              h4(strong("Programming"), align = "left"),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "We outline the methodology used to perform an Ordinary Least Squares (OLS) regression analysis 
                                                to examine the relationship between factors of deaths of despair rates in Virginia."),
                                              div(style = "margin-top: 30px;"),
                                              p(style = "font-size: 16px; text-align: left;", strong("I. Model Specification:")),
                                              p(style = "font-size: 16px; text-align: left;", "First, we specify the linear regression model. This involves identifying the dependent variable 
                                                (the outcome you are trying to predict or explain) and one or more independent variables (the predictors). 
                                                The model can be expressed as:"),
                                              h3(strong(HTML("Y<sub>it</sub> = 𝛼 + β X<sub>it</sub> +	𝜸 C<sub>it</sub> + ϵ<sub>it</sub>"))),
                                              p(style = "text-align: left; font-size: 16px;", "Where:",
                                                tags$ul(
                                                  style = "text-align: left; font-size: 16px;",
                                                  tags$li(HTML("Y<sub>it</sub>  is the dependent variable indicates the death rate per 100,000 population in Virginia county i and year t.")),
                                                  tags$li(HTML("X<sub>it</sub>  are our primary variables of interest include poverty rates, unemployment rates, and educational attainment rates (Bachelor's degree or higher) for each county i in year t.")),
                                                  tags$li(HTML("C<sub>it</sub>  are the control variables such as race, sex, and fixed effects for year and Health Service Provider Area (HSPA) codes. 
                                                               The fixed effects in our model determine if certain factors (such as categories or groups) have a significant impact compared to a baseline group. Fixed effects account for the differences between factors that remain constant over time.")),
                                                  tags$li(HTML("ϵ<sub>it</sub>  is the regression error term, capturing all other factors that influence Y<sub>it</sub>  but are not included in the model.")),
                                                )
                                              ),
                                              div(style = "margin-top: 30px;"),
                                              p(style = "font-size: 16px; text-align: left;", strong("II. Estimation of Coefficients")),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "The OLS method estimates the coefficients of the regression model by minimizing the sum 
                                                of the squared residuals. The residual for each observation is the difference between the observed value of the 
                                                dependent variable and the value predicted by the linear model. Mathematically, this is done by solving the 
                                                following minimization problem: "),
                                              div(style = "margin-top: 20px;"),
                                              img(src = "equation.jpg", style = "display: block; margin-left: auto; margin-right: auto;", width = "550px"),
                                              div(style = "margin-top: 20px;"),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "This leads to a set of normal equations, which are solved to find the estimates of the parameters."),
                                              div(style = "margin-top: 30px;"),
                                              p(style = "font-size: 16px; text-align: left;", strong("III. Model Evaluation")),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "After estimating the parameters, we evaluate the model to determine how well it fits the data.",
                                                tags$ul(
                                                  style = "text-align: left; font-size: 16px;",
                                                  tags$li(strong("R-squared: "), "Indicates the proportion of variance in the dependent variable explained by the independent variables."),
                                                  tags$li(strong("Adjusted R-squared:"), "Adjusts the R-squared for the number of predictors in the model, providing a more accurate measure in the presence of multiple predictors."),
                                                  tags$li(strong("F-statistic: "), "Tests whether at least one predictor variable has a non-zero coefficient."),
                                                )
                                              ),
                                              div(style = "margin-top: 30px;"),
                                     )
                            ),
                            tabPanel("Regression", 
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                              align = "center",
                                              h1("Regression"),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "Our primary objective is to determine the correlation between the percentage 
                                                of a county that is college-educated, in poverty, or unemployed, and deaths of 
                                                despair using multivariate ordinary least squares regression."),
                                              div(style = "margin-top: 20px;")
                                     ),
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                              align = "center",
                                              h4("DOD = 261.490 - 0.848(", em("Percent with Bachelor's or Higher"), ") + 1.227(", em("Percent in Poverty"), ") + 6.653(", em("Percent Unemployed"), ")"),
                                              div(style = "margin-top: 0px;"),
                                              h4("- 3.996(", em("Total Male Population"), ") - 0.008(", em("Total Black Population"), ") - 26.103(", em("Total Native Population"), ") - 0.678(", em("Total Asian/Pacific Population"), ")"),
                                              div(style = "margin-top: 0px;"),
                                              h4("- 6.618(", em("Part of County in Shortage"), ") + 11.937(", em("Whole County in Shortage"), ")"),
                                              div(style = "margin-top: 0px;"),
                                              h4("+ 8.069(", em("2019 Fixed Effect"), ") + 28.249(", em("2020 Fixed Effect"), ") + 35.857(", em("2021 Fixed Effect"), ") + 37.421(", em("2022 Fixed Effect"), ")"),
                                              div(style = "margin-top: 30px;")
                                              
                                     ),
                                     fluidRow(column(width = 4,
                                                     offset = 4,
                                                     style = "font-family: 'Times New Roman', Times, serif;",
                                                     align = "center",
                                                     h4("DOD Regression Variables"),
                                                     uiOutput("regression_vars"),
                                                     div(style = "margin-top: 30px;")
                                     )
                                     
                                     ),
                                     fluidRow(column(width = 4,
                                                     offset = 4,
                                                     style = "font-family: 'Times New Roman', Times, serif;",
                                                     align = "center",
                                                     h4("DOD Control Variables"),
                                                     tableOutput("regression_control"),
                                     )
                                     ),
                                     fluidRow(column(width = 12,
                                                     style = "font-family: 'Times New Roman', Times, serif;",
                                                     align = "center",
                                                     div(style = "margin-top: 20px;"),
                                                     p(style = "font-size: 16px;", 
                                                       em("Note: For the regression analysis, the deaths of despair rate is suppressed for 186 out of 665 counties. 
                                                       Since 60% percent of female sample is suppressed, the coefficient for total male population 
                                                       is biased.")),
                                                     div(style = "margin-top: 30px;")
                                     )
                                     )
                            ),
                            tabPanel("Limitations", 
                                     fluidRow(style = "margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                              align = "center",
                                              h1("Analysis and Data Limitations"),
                                              div(style = "margin-top: 30px;"),
                                              h4(align = "left", strong("Social Limitations")),
                                              p(style = "font-size: 16px; text-align: left;", 
                                              "Social factors play a crucial role in the accuracy and comprehensiveness of DOD data. 
                                                The stigma surrounding drug overdoses and suicides often leads to significant underreporting or 
                                                misclassification of these deaths. Families and communities, influenced by societal judgment, may 
                                                avoid reporting the true nature of these deaths, resulting in data that does not accurately reflect 
                                                the reality of the situation. This underreporting can distort the perceived prevalence of DOD, 
                                                leading to an underestimation of the problem and subsequently insufficient resource allocation 
                                                and intervention efforts."),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "Moreover, the underrepresentation of marginalized populations in data collection efforts exacerbates 
                                                this issue. Historical and ongoing disparities mean that racial and ethnic minorities, among other 
                                                vulnerable groups, are often left out of comprehensive data collection. This lack of representation 
                                                can skew results, giving a false sense of security regarding the impact of DOD on these groups. It masks 
                                                the true extent of the problem, leading to policies and interventions that may not address the specific 
                                                needs of these underrepresented populations."),
                                              div(style = "margin-top: 30px;"),
                                              h4(align = "left", strong("Data Suppression")),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "Data suppression, implemented to protect individual privacy, further complicates the analysis of DOD data. 
                                                While this suppression is necessary to maintain confidentiality, it also results in significant data gaps. 
                                                These gaps hinder our ability to fully understand the geographic and demographic distribution of DOD, 
                                                particularly in small or specific demographic groups where the data is most crucial."),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "Suppressed data can lead to skewed results. For example, if data from small rural communities with high 
                                                DOD rates is suppressed, it may appear that these areas are less affected by DOD than they are. This 
                                                lack of detailed information complicates efforts to create awareness and targeted interventions, as 
                                                policymakers and health professionals rely on accurate data to allocate resources and design programs. 
                                                Balancing the need for privacy with the demand for comprehensive data is a significant challenge that must be addressed."),
                                              p(style = "font-size: 16px; text-align: left;",
                                                "From a sociological perspective, the suppression of data creates a phenomenon known as “invisibility bias.” This is where 
                                                the absence of detailed data on distribution of DOD leads to certain demographics being overlooked or underestimated. For 
                                                instance, if high DOD rates in small rural communities are masked by data suppression, policymakers may not recognize the 
                                                need for targeted support and resources in these areas. This lack of visibility undermines the validity and representativeness 
                                                of the data, leading to less effective and equitable policy decisions. Understanding these limitations is critical for 
                                                interpreting DOD data accurately. Recognizing the impact of underreporting, misclassification, underrepresentation, and 
                                                data suppression helps to contextualize the findings and underscores the importance of developing more inclusive and transparent 
                                                data collection practices.")
                                     )
                            )
                 ),
                 
                 # Data Sources Tab
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
                                   h1("Data Sources", align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   fluidRow(style = "margin: 6px;", align = "left",
                                            column(4,
                                                   img(src = "acs.jpg", style = "display: inline; float: left; margin-right: 10px;", width = "200px"),
                                                   p(strong("American Community Survey (ACS)"), "The United States Census Bureau 
                                                     annually collects and releases comprehensive data on social, economic, housing, and 
                                                     demographic aspects, presented through both estimates and percentages. This data enabled 
                                                     us to conduct a deeper investigation and identify correlations between various factors 
                                                     and their prevalence in deaths of despair.")),
                                            column(4,
                                                   img(src = "cdc_mcod.jpg", style = "display: inline; float: left; margin-right: 10px;", width = "230px"),
                                                   p(strong("CDC National Center for Health Statistics (NCHS)"),"The National Center for Health Statistics (NCHS) 
                                                     compiles the Multiple Cause of Death (MCOD) data, which provides comprehensive information 
                                                     on mortality trends in the United States. This dataset includes detailed records of the 
                                                     underlying and contributing causes of death, covering a wide range of demographic and medical 
                                                     variables. By utilizing the MCOD data, we were able to conduct an in-depth analysis to uncover 
                                                     patterns and correlations between various causes of death amongst demographics and its overall 
                                                     connection to deaths of despair.")),
                                            column(4,
                                                   img(src = "county_health_rankings.jpg", style = "display: inline; float: left; margin-right: 10px;", width = "230px"),
                                                   p(strong("County Health Rankings & Roadmaps"), "The County Health Rankings & Roadmaps 
                                                     program annually collects and releases detailed data on health outcomes, behaviors, 
                                                     clinical care, and social and economic factors at the county level in Virginia. This 
                                                     robust dataset includes metrics on mortality, morbidity, access to care, quality of care, 
                                                     education, employment, income, and community safety, among others. This data provided us 
                                                     with the necessary information to investigate and identify correlations between these 
                                                     variables and the prevalence of deaths of despair.")),
                                   ), 
                                   
                                   fluidRow(style = "margin: 6px;", align = "left",
                                            column(4,
                                                   img(src = "hrsa.jpg", style = "display: inline; float: left;margin-right: 10px", width = "150px"),
                                                   p(strong("Health Resources and Services Administration (HRSA)"),"A Health Professional Shortage 
                                                     Area (HPSA) is a designation given by the Health Resources and Services Administration (HRSA) 
                                                     to areas, or groups that lack sufficient health professionals. By analyzing HPSA data, we were 
                                                     able to examine the distribution of healthcare resources and identify regions with significant 
                                                     healthcare access challenges. This analysis helps in understanding how the lack of healthcare 
                                                     professionals may contribute to the prevalence of deaths of despair, highlighting the critical 
                                                     role of accessible intervention/harm reduction sites in mitigating these mortality trends.")),
                                            column(4,
                                                   img(src = "vdh.jpg", style = "display: inline; float: left;", width = "230px"),
                                                   p(strong("Virginia Department of Health (VDH)"),"The Virginia Department of Health (VDH) collects 
                                                     and releases comprehensive data on opioid-related incidents, including the death count and 
                                                     rate by county. This dataset provides detailed insights into the scope and impact of the opioid 
                                                     crisis within the state, covering various demographic and geographic variables. By leveraging 
                                                     the VDH opioid data, we were able to conduct a thorough investigation into the patterns and 
                                                     correlations between opioid use and Deaths of Despair, enhancing our understanding of how this 
                                                     epidemic contributes to broader mortality trends.")),
                                   ),
                          )),
                 
                 # Meet the Team Tab
                 tabPanel("Meet the Team", 
                          fluidRow(style = "margin: 12px; margin-left: 50px; margin-right: 50px; font-family: 'Times New Roman', Times, serif;",
                                   align = "center",
                                   h1("Meet the Team"),
                                   p(style = "font-size: 16px; text-align: left;", 
                                     "The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the 
                                     Virginia Tech Department of Agricultural and Applied Economics and the Virginia Cooperative Extension Service. 
                                     In its fourth year, the program engages students from across the country to work together on projects that address 
                                     state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young 
                                     scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                     information generated within every community can be leveraged to improve quality of life and inform public policy. For more 
                                     information on program highlights, how to apply, and our annual symposium, please visit the official",
                                     tags$a(href = "https://aaec.vt.edu/academics/undergraduate/dspg.html","VT DSPG website"),
                                     ".")
                          ),
                          fluidRow(
                            div(style = "text-align: center; font-family: 'Times New Roman'; font-size: 28px; color: maroon; margin-top: 20px; margin-bottom: 30px;", "Undergraduate Interns")
                          ),
                          fluidRow(style = "margin: 12px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", align = "center",
                                   column(4,
                                          img(src = "FilzaMutaal.jpg", style = "display: block; margin-left: auto; margin-right: auto;", width = "400px"),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Filza Mutaal")),
                                          p(em("Berea College 2026")),
                                          p("Majors: Economics and Political Science"),
                                          p("Minors: Finance and Law, Ethics and Society"),
                                          p("Concentration: International Politics and Policy")),
                                   column(4,
                                          img(src = "ElisabethWasserman.jpg", style = "display: block; margin-left: auto; margin-right: auto;", width = "400px"),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Elisabeth Wasserman")),
                                          p(em("Virginia Tech 2026")),
                                          p("Majors: Computational Modeling & Data Analytics and Statistics"),
                                          p("Minor: Psychology")),
                                   column(4,
                                          img(src = "BrynnaWert.jpg", style = "display: block; margin-left: auto; margin-right: auto;", width = "400px"),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Brynna Wert")),
                                          p(em("Virginia Tech 2024")), 
                                          p("Majors: Criminology and Sociology"),
                                          p("Concentration: French"))
                          ), 
                          fluidRow(style = "text-align: center; font-family: 'Times New Roman'; font-size: 28px; color: maroon; margin-top: 20px; margin-bottom: 30px;",
                                   column(12, 
                                          h2("Graduate Fellow", style = "text-align: center;")
                                   )
                          ),
                          fluidRow(style = "margin: 12px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", align = "center",
                                   column(12,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "YifanWang.jpg", width = "400px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Yifan Wang")),
                                          p(em("Virginia Tech, PhD Student")),
                                          p("Major: Agricultural and Applied Economics"))
                          ),
                          fluidRow(style = "text-align: center; font-family: 'Times New Roman'; font-size: 28px; color: maroon; margin-top: 20px; margin-bottom: 30px;",
                                   column(12,
                                          h2("Faculty", style = "text-align: center;")
                                   )
                          ),
                          fluidRow(style = "margin: 12px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", align = "center",
                                   column(3,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "MichaelCary.jpg", height = "220px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Dr. Michael Cary")),
                                          p(em("Virginia Tech")),
                                          p("Research Assistant Professor"),
                                          p("Department of Agricultural and Applied Economics")),
                                   column(3,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "Bradburn.jpg", height = "220px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Dr. Isabel Bradburn")),
                                          p(em("Virginia Tech")),
                                          p("Research Director, Child Development Center"),
                                          p("Department of Human Development")),
                                   column(3,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "KathyHosig1.jpg", height = "220px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Dr. Kathy Hosig")),
                                          p(em("Virginia Tech")),
                                          p("Associate Professor"),
                                          p("Department of Population Health Sciences")
                                   ),
                                   column(3,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "SusanChen.jpg", height = "220px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Dr. Susan Chen")),
                                          p(em("Virginia Tech")),
                                          p("Associate Professor"),
                                          p("Department of Agricultural and Applied Economics"))
                          ),
                          
                          fluidRow(
                            style = "margin-top: 20px; margin-bottom: 30px; font-weight: bold; font-size: 28px; color: maroon; font-family: 'Times New Roman', Times, serif;",
                            align = "center",
                            column(12,
                                   h2("Stakeholder", style = "text-align: center;")
                            )         
                          ),
                          fluidRow(style = "margin: 12px; font-size: 16px; font-family: 'Times New Roman', Times, serif;", align = "center",
                                   column(12,
                                          div(
                                            style = "display: flex; justify-content: center;",
                                            img(src = "vce_long.jpg", width = "600px"), 
                                          ),
                                          div(style = "margin-top: 20px;"),
                                          p(strong("Virginia Cooperative Extension")),
                                          p(style = "margin-left: 310px; margin-right: 310px;"," 
                                          'Virginia Cooperative Extension offers a comprehensive collection of resources,
                                            programs, and services that are research-proven, accessible, and contain
                                            actionable information that supports the success and resilience of individuals
                                            and communities throughout the Commonwealth of Virginia and beyond.' - ",
                                            em("Virginia Cooperative Extension"))
                                   )
                          ),
                          div(style = "margin-top: 30px;")
                 )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on selected year
  filtered_data_virginia <- reactive({
    map_virginia_2018_2022 %>%
      filter(Year == input$year_virginia)
  })
  
  # Render Leaflet map for Virginia Factors
  output$map_virginia <- renderLeaflet({
    map_data <- filtered_data_virginia()
    
    # Determine which variable to use for coloring
    variable <- switch(input$variable_virginia,
                       "Percentage with Bachelor's or Higher" = map_data$BA_higher_ratio,
                       "Percentage Unemployed" = map_data$unemployed_pop_pe,
                       "Mean Household Income" = map_data$mean_household_income,
                       "Median Household Income" = map_data$Median_household_income,
                       "Percentage in Poverty" = map_data$below_poverty)
    
    # Determine bins based on variable selection
    mybins <- switch(input$variable_virginia,
                     "Percentage with Bachelor's or Higher" = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95),
                     "Percentage Unemployed" = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
                     "Mean Household Income" = c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000),
                     "Median Household Income" = c(45000, 60000, 75000, 105000, 120000, 135000, 150000, 165000),
                     "Percentage in Poverty" = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90))
    
    mypalette <- colorBin(viridis(10), domain = variable, bins = mybins)
    
    leaflet(map_data) %>%
      setView(lng = -77.4360, lat = 37.5407, zoom = 6) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~mypalette(variable),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 0.5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste("County: ", NAME, "<br>", input$variable_virginia, ": ", round(variable, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = mypalette, values = ~variable, opacity = 0.7, title = paste0(input$variable_virginia, " (", input$year_virginia, ")"), position = "bottomright")
  })
  
  # Render Leaflet map for Deaths of Despair
  output$map_another <- renderLeaflet({
    map_data <- filtered_data_virginia()
    
    mybins <- c(15, 45, 75, 105, 135, 165, 195, 225, 255, 285)
    mypalette <- colorBin(viridis(10), domain = map_data$DOD_death_per, bins = mybins)
    
    leaflet(map_data) %>%
      setView(lng = -77.4360, lat = 37.5407, zoom = 6) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~mypalette(DOD_death_per),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 0.5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste("County: ", NAME, "<br>DOD Rate per 100,000: ", round(DOD_death_per, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = mypalette, values = ~DOD_death_per, opacity = 0.7, 
                title = paste0("DOD Rate per 100,000 ", "(", input$year_virginia, ")"), 
                position = "bottomright")
  })
  
  # Render correlation plot
  output$corr_plot <- renderPlot({
    map_data <- map_virginia_2018_2022
    
    # Determine which variable to use for correlation
    correlation_variable <- switch(input$variable_correlation,
                                   "Mean Household Income" = "mean_household_income",
                                   "Median Household Income" = "Median_household_income",
                                   "Percentage Unemployed" = "unemployed_pop_pe",
                                   "BA or Higher Ratio" = "BA_higher_ratio",
                                   "Percentage in Poverty" = "below_poverty")
    
    ggplot(map_data, aes_string(x = correlation_variable, y = "DOD_death_per")) +
      geom_point(color = viridis(256)[50], size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", color = viridis(256)[200], size = 1.5, se = FALSE) +
      labs(title = NULL,
           caption = "Data obtained from the American Community Survey, the Virginia Health Rankings, and the National Center for Health Statistics (CDC).",
           x = input$variable_correlation,
           y = "Deaths of Despair Rate (per 100,000)") +
      theme_minimal(base_size = 15) +
      theme(
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5)# Remove the legend
      )
  })
  
  map_virginia_2022 <- map_virginia_2018_2022 %>%
    filter(Year == 2022)
  
  output$chr_sites_map <- renderLeaflet({
    # Define the color palette for the RUCC codes
    color_palette <- colorFactor(palette = viridis::viridis(3), domain = map_virginia_2022$HPSAcode)
    
    hpsa_descriptions <- c(
      '1' = "1: None",
      '2' = "2: Part",
      '3' = "3: Whole"
      # Add more descriptions as needed
    )
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = map_virginia_2022,
        weight = 2,
        fillColor = ~color_palette(HPSAcode),  # Color based on RUCC_2023 values with viridis palette
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste("<strong>County: </strong> ", NAME, "<br>", 
                       "<strong>HPSA Code: </strong>", HPSAcode),  # Label with county and RUCC code
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addMarkers(
        data = chr_sites_2023,
        lng = ~Long,
        lat = ~Lat,
        popup = ~paste(
          "<strong>Name: </strong>", Name, "<br>",
          "<strong>County: </strong>", County, "<br>"
        )
      ) %>%
      addLegend(
        pal = color_palette,
        values = map_virginia_2022$HPSAcode,
        title = "HPSA Codes",
        opacity = 0.7,
        position = "bottomright",
        labFormat = labelFormat(
          transform = function(x) {
            # Replace each HPSA code with its description
            sapply(x, function(code) hpsa_descriptions[as.character(code)])
          }
        )
      )
  })
  
  
  virginia_selected_data <- reactive({
    switch(input$demographic_selector,
           "Ten.Year.Age.Groups" = VA_DOD_AGE_1822,
           "Single.Race.6" = VA_DOD_RACE_1822,
           "Gender" = VA_DOD_SEX_1822,
           "DOD total" = VA_DOD_1822)
  })
  
  virginia_selected_variable <- reactive({
    switch(input$demographic_selector,
           "Ten.Year.Age.Groups" = "Ten.Year.Age.Groups",
           "Single.Race.6" = "Single.Race.6",
           "Gender" = "Gender",
           "DOD total" = NULL)
  })
  
  
  output$plot_output <- renderPlot({
    data <- virginia_selected_data()
    variable <- virginia_selected_variable()
    
    if (input$demographic_selector == "DOD total" || is.null(variable)) {
      # Plot for DOD total
      ggplot(data = data, aes(x = Year, y = Crude.Rate)) +
        geom_line(size = 1, color = "purple") +
        geom_point(size = 2, color = "purple") +
        labs(x = "Year",
             y = "Deaths of Despair Rate (per 100,000)",
             caption = paste("Data obtained by National Center of Health Statistics (CDC).")) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.7),
              axis.title = element_text(face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 12))
    } else {
      # Plot for other demographics
      ggplot(data = data, aes_string(x = "Year", y = "Crude.Rate", color = variable, group = variable)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(x = "Year",
             y = "Deaths of Despair Rate (per 100,000)",
             color = "Demographic",
             caption = paste("Data obtained by National Center of Health Statistics (CDC).")) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.7),
              axis.title = element_text(face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 12)) +
        scale_color_viridis(discrete = TRUE)
    }
  })
  
  #demographics factors line plots 
  get_variable_suffixes <- function(demographic) {
    switch(demographic,
           "Age" = c("2534", "3544", "4564", "65"),
           "Race" = c("white", "black", "asian_pacific", "native"),
           "Gender" = c("female", "male"))
  }
  
  new_legend_names <- c(
    "2534" = "Age 25-34",
    "3544" = "Age 35-44",
    "4564" = "Age 45-64",
    "65" = "Age 65+",
    "white" = "White",
    "black" = "Black",
    "asian_pacific" = "Asian/Pacific Islander",
    "native" = "Native American",
    "female" = "Female",
    "male" = "Male"
  )
  
  # Render plot for BA_higher_ratio
  output$plot_ba_higher_ratio <- renderPlot({
    suffixes <- get_variable_suffixes(input$demographic_selector2)
    ba_higher_vars <- paste0("BA_higher_ratio_", suffixes)
    
    data <- virginia_2018_2022 %>%
      select(Year, all_of(ba_higher_vars)) %>%
      pivot_longer(
        cols = all_of(ba_higher_vars),
        names_to = "Demographic",
        values_to = "Value"
      ) %>%
      mutate(Demographic = sub("BA_higher_ratio_", "", Demographic)) %>%
      group_by(Year, Demographic) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Mean_Value = Mean_Value * 100)
    
    ggplot(data, aes(x = Year, y = Mean_Value, color = Demographic, group = Demographic)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Year",
        y = "Percentage with a Bachelor's or Higher (%)",
        color = "Demographic",
        caption = "Data obtained by the American Community Survey (5 Year Estimates).") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.7),
            plot.caption = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 15),  # Customize legend title text size
            legend.text = element_text(size = 12)) +
      scale_color_viridis(discrete = TRUE) + 
      scale_color_manual(values = viridis::viridis(n = length(unique(data$Demographic)), option = "D"), labels = new_legend_names)
  })
  
  # Render plot for below_poverty_ratio
  output$plot_poverty_ratio <- renderPlot({
    suffixes <- get_variable_suffixes(input$demographic_selector2)
    poverty_vars <- paste0("below_poverty_ratio_", suffixes)
    
    data <- virginia_2018_2022 %>%
      select(Year, all_of(poverty_vars)) %>%
      pivot_longer(
        cols = all_of(poverty_vars),
        names_to = "Demographic",
        values_to = "Value"
      ) %>%
      mutate(Demographic = sub("below_poverty_ratio_", "", Demographic)) %>%
      group_by(Year, Demographic) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Mean_Value = Mean_Value * 100)
    
    ggplot(data, aes(x = Year, y = Mean_Value, color = Demographic, group = Demographic)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Year",
        y = "Percentage At or Below Poverty (%)",
        color = "Demographic",
        caption = "Data obtained by the American Community Survey (5 Year Estimates).") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.7),
            plot.caption = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 15),  # Customize legend title text size
            legend.text = element_text(size = 12)) +
      scale_color_viridis(discrete = TRUE) +
      scale_color_manual(values = viridis::viridis(n = length(unique(data$Demographic)), option = "D"), labels = new_legend_names)
  })
  
  #US map dashboard
  # Define the reactive expression to select the variable and bins based on user input
  selected_variable <- reactive({
    switch(input$death_type,
           "DOD_deaths_per" = states_merged$DOD_deaths_per,
           "drugs_deaths_per" = states_merged$drugs_deaths_per,
           "alcohol_deaths_per" = states_merged$alcohol_deaths_per,
           "suicide_deaths_per" = states_merged$suicide_deaths_per)
  })
  
  mybins <- reactive({
    switch(input$death_type,
           "DOD_deaths_per" = c(50, 65, 80, 95, 110, 125, 140, 165),
           "drugs_deaths_per" = c(10, 22, 34, 46, 58, 70, 82, 94),
           "alcohol_deaths_per" = c(12, 24, 36, 48, 60, 72, 84, 96),
           "suicide_deaths_per" = c(5, 10, 15, 20, 25, 30, 35, 40))
  })
  
  legend_title <- reactive({
    switch(input$death_type,
           "DOD_deaths_per" = "Deaths of Despair Rates",
           "drugs_deaths_per" = "Overdose Death Rates",
           "alcohol_deaths_per" = "Alcohol Death Rates",
           "suicide_deaths_per" = "Suicide Death Rates")
  })
  
  output$usmap1 <- renderLeaflet({
    req(states_merged)  # Ensure states_merged is available and not NULL
    
    pal <- colorBin(
      palette = "viridis",
      domain = selected_variable(),
      bins = mybins(),
      na.color = "transparent"
    )
    
    leaflet(states_merged) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(selected_variable()),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("State:", NAME, "<br>Death rate:", round(selected_variable(), 2))
      ) %>%
      addLegend(
        pal = pal,
        values = selected_variable(),
        opacity = 0.7,
        title = paste("U.S. Average", legend_title(), "per 100,000 people (2018-2022)<br><span style='font-size:10px; color: black;'>Data obtained from the National Center for Health Statistics (CDC)</span>"),
        position = "bottomleft"
      )
  })
  
  # Render the second map
  output$usmap2 <- renderLeaflet({
    req(states_merged)  # Ensure states_merged is available
    
    # Define color palette based on the most prevalent cause of death
    pal2 <- colorFactor(
      palette = viridis(3),
      domain = states_merged$max_cause,
      na.color = "transparent"
    )
    
    leaflet(states_merged) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal2(max_cause),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("State:", NAME, "<br>Most prevalent cause of death:", max_cause)
      ) %>%
      addLegend(
        pal = pal2,
        values = ~max_cause,
        opacity = 0.7,
        title = "U.S. Most Prevalent Cause of Deaths of Despair (2018-2022)<br><span style='font-size:10px; color: black;'>Data obtained from the National Center for Health Statistics (CDC)</span>",
        position = "bottomleft"
      )
  })
  
  #appalachia factors maps
  # Filter data based on selected year
  filtered_data_appalachia <- reactive({
    map_appalachia_2018_2022 %>%
      filter(Year == input$year_appalachia)
  })
  
  # Render Leaflet map for Appalachia Factors
  output$map_app_factors <- renderLeaflet({
    map_app_data <- filtered_data_appalachia()
    
    # Determine which variable to use for coloring
    app_variable <- switch(input$variable_appalachia,
                           "Percentage with Bachelor's or Higher" = map_app_data$BA_higher_ratio,
                           "Percentage Unemployed" = map_app_data$unemployed_pop_pe,
                           "Mean Household Income" = map_app_data$mean_household_income,
                           "Percentage in Poverty" = map_app_data$below_poverty)
    
    # Determine bins based on variable selection
    mybins <- switch(input$variable_appalachia,
                     "Percentage with Bachelor's or Higher" = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95),
                     "Percentage Unemployed" = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
                     "Mean Household Income" = c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000),
                     "Percentage in Poverty" = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90))
    
    mypalette <- colorBin(viridis(10), domain = app_variable, bins = mybins)
    
    leaflet(map_app_data) %>%
      setView(lng = -80.4549, lat = 37.8753, zoom = 5) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~mypalette(app_variable),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 0.5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste("County: ", County, "<br>", input$variable_appalachia, ": ", round(app_variable, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = mypalette, values = app_variable, opacity = 0.7, title = paste0(input$variable_appalachia, " (", input$year_appalachia, ")"), position = "bottomright")
  })
  
  # Render Leaflet map for Deaths of Despair
  output$map_app_dod <- renderLeaflet({
    map_app_data <- filtered_data_appalachia()
    
    mybins <- c(15, 45, 75, 105, 135, 165, 195, 225, 255, 285)
    mypalette <- colorBin(viridis(10), domain = map_app_data$DOD_death_rate, bins = mybins)
    
    leaflet(map_app_data) %>%
      setView(lng = -80.4549, lat = 37.8753, zoom = 5) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~mypalette(DOD_death_rate),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 0.5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste("County: ", County, "<br>DOD Rate per 100,000: ", round(DOD_death_rate, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = mypalette, values = map_app_data$DOD_death_rate, opacity = 0.7, 
                title = paste0("DOD Rate per 100,000 ", "(", input$year_appalachia, ")"), 
                position = "bottomright")
  })
  
  # Render correlation plot
  output$app_corr_plot <- renderPlot({
    map_app_data <- map_appalachia_2018_2022
    
    # Determine which variable to use for correlation
    app_correlation_variable <- switch(input$app_variable_correlation,
                                       "Mean Household Income" = "mean_household_income",
                                       "Percentage Unemployed" = "unemployed_pop_pe",
                                       "BA or Higher Ratio" = "BA_higher_ratio",
                                       "Percentage in Poverty" = "below_poverty")
    
    ggplot(map_app_data, aes_string(x = app_correlation_variable, y = "DOD_death_rate")) +
      geom_point(color = viridis(256)[50], size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", color = viridis(256)[200], size = 1.5, se = FALSE) +
      labs(title = NULL,
           caption = "Data obtained from the American Community Survey and the National Center for Health Statistics (CDC).",
           x = input$app_variable_correlation,
           y = "Deaths of Despair Rate (per 100,000)") +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5)# Remove the legend
      )
  })
  
  #Appalachia demographic plots
  
  #demographics factors line plots 
  app_variable_suffixes <- function(demographic) {
    switch(demographic,
           "Age" = c("2534", "3544", "4564", "65"),
           "Race" = c("white", "black", "asian_pacific", "native"),
           "Gender" = c("female", "male"))
  }
  
  app_line_legend_names <- c(
    "2534" = "Age 25-34",
    "3544" = "Age 35-44",
    "4564" = "Age 45-64",
    "65" = "Age 65+",
    "white" = "White",
    "black" = "Black",
    "asian_pacific" = "Asian/Pacific Islander",
    "native" = "Native American",
    "female" = "Female",
    "male" = "Male"
  )
  # Render plot for BA_higher_ratio
  output$app_plot_ba_higher_ratio <- renderPlot({
    app_suffixes <- app_variable_suffixes(input$demographic_selector3)
    ba_higher_vars <- paste0("BA_higher_ratio_", app_suffixes)
    
    data <- appalachia_2018_2022 %>%
      select(Year, all_of(ba_higher_vars)) %>%
      pivot_longer(
        cols = all_of(ba_higher_vars),
        names_to = "Demographic",
        values_to = "Value"
      ) %>%
      mutate(Demographic = sub("BA_higher_ratio_", "", Demographic)) %>%
      group_by(Year, Demographic) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Mean_Value = Mean_Value * 100)
    
    ggplot(data, aes(x = Year, y = Mean_Value, color = Demographic, group = Demographic)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Year",
        y = "Percentage with a Bachelor's or Higher (%)",
        color = "Demographic",
        caption = "Data obtained by the American Community Survey (5 Year Estimates).") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.7),
            plot.caption = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 15),  # Customize legend title text size
            legend.text = element_text(size = 12)) +
      scale_color_viridis(discrete = TRUE) + 
      scale_color_manual(values = viridis::viridis(n = length(unique(data$Demographic)), option = "D"), labels = app_line_legend_names)
  })
  
  # Render plot for below_poverty_ratio
  output$app_plot_poverty_ratio <- renderPlot({
    app_suffixes <- app_variable_suffixes(input$demographic_selector3)
    poverty_vars <- paste0("below_poverty_ratio_", app_suffixes)
    
    data <- appalachia_2018_2022 %>%
      select(Year, all_of(poverty_vars)) %>%
      pivot_longer(
        cols = all_of(poverty_vars),
        names_to = "Demographic",
        values_to = "Value"
      ) %>%
      mutate(Demographic = sub("below_poverty_ratio_", "", Demographic)) %>%
      group_by(Year, Demographic) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Mean_Value = Mean_Value * 100)
    
    ggplot(data, aes(x = Year, y = Mean_Value, color = Demographic, group = Demographic)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Year",
        y = "Percentage At or Below Poverty (%)",
        color = "Demographic",
        caption = "Data obtained by the American Community Survey (5 Year Estimates).") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.7),
            plot.caption = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 15),  # Customize legend title text size
            legend.text = element_text(size = 12)) +
      scale_color_viridis(discrete = TRUE) +
      scale_color_manual(values = viridis::viridis(n = length(unique(data$Demographic)), option = "D"), labels = app_line_legend_names)
  })
  
  output$regression_vars <- renderUI({
    # Sample data
    data <- data.frame(
      Variable = c("Intercept","Percent with Bachelor's or Higher", "Percent in Poverty", "Percent Unemployed"),
      Coefficient = c(261.490, -0.848, 1.227, 6.653),
      PValue = c("4.71e-10", "7.01e-05", "1.43e-05","5.13e-04"),
      Interpretation = c("The baseline rate of deaths of despair is 261.49 per 100,000 people when all other factors in the model are held at zero.",
                         "For every 1 percent increase in the college-educated population, DOD rate decreases by 0.848 per 100,000.",
                         "For every 1 percent increase in the population in poverty, DOD rate increases by 1.227 per 100,000.",
                         "For every 1 percent increase in the unemployment population, DOD rate increases by 6.653 per 100,000.")
    )
    
    # Create custom HTML table with borders
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid black;",
      tags$thead(
        tags$tr(
          tags$th(style = "width: 20%; border: 2px solid black; padding: 4px;", "Variable"),
          tags$th(style = "width: 17%; border: 2px solid black; padding: 4px;", "Coefficient"),
          tags$th(style = "width: 13%; border: 2px solid black; padding: 4px;", "PValue"),
          tags$th(style = "width: 50%; border: 2px solid black; padding: 4px;", "Interpretation")
        )
      ),
      tags$tbody(
        lapply(1:nrow(data), function(i) {
          tags$tr(
            tags$td(style = "width: 20%; border: 2px solid black; padding: 4px;", data$Variable[i]),
            tags$td(style = "width: 17%; border: 2px solid black; padding: 4px;", data$Coefficient[i]),
            tags$td(style = "width: 13%; border: 2px solid black; padding: 4px;", data$PValue[i]),
            tags$td(style = "width: 50%; border: 2px solid black; padding: 4px;", data$Interpretation[i])
          )
        })
      )
    )
  })
  
  output$regression_control <- renderUI({
    # Sample data
    data <- data.frame(
      Variable = c("Total Male Population", "Total Black Population", "Total Native Population", "Total Asian and Pacific Islander Population",
                   "Part of County in Shortage", "Whole County in Shortage", 
                   "2019 Fixed Effect", "2020 Fixed Effect", "2021 Fixed Effect", "2022 Fixed Effect"),
      Coefficient = c(-3.996, -0.008, -26.103, -0.678,
                      -6.618, 11.937,
                      8.068, 28.249, 35.857, 37.421),
      PValue = c("4.44e-07", "0.946",  "0.002", "0.302",
                 "0.365", "0.003",
                 "0.104", "1.11e-08", "1.04e-12", "3.80e-13"),
      Interpretation = c("For each unit increase in the percentage of the male population, the DOD death rate decreases by 3.996 deaths per 100,000, compared to females (the baseline group).",
                         "The coefficient for the Black population is not statistically significant (p-value = 0.946), suggesting that there is no meaningful difference in the DOD death rates between Black and White populations (the baseline group) in this analysis.",
                         "The DOD death rate is 26.103 deaths per 100,000 lower in counties with a higher percentage of Native American population compared to White populations, indicating a significantly lower rate of death.",
                         "The coefficient is not statistically significant (p-value = 0.302), indicating that the DOD death rates do not significantly differ between Asian Pacific and White populations.",
                         "There is no significant difference in the DOD death rates between parts of counties in medical professional shortage and those with no shortage areas (p-value = 0.365).",
                         "Counties entirely in medical professional shortage areas see an increase of 11.937 deaths per 100,000 compared to counties without shortage areas, indicating a significantly higher rate of death.",
                         "The DOD death rate in 2019 is 8.068 deaths per 100,000 higher compared to 2018, though this effect is not statistically significant (p-value = 0.104).",
                         "In 2020, the DOD death rate is significantly higher by 28.249 deaths per 100,000 compared to 2018, highlighting a substantial increase likely influenced by the COVID-19 pandemic.",
                         "Compared to 2018, 2021 shows a significant increase in the DOD death rate by 35.857 deaths per 100,000, continuing the trend of increased mortality possibly due to ongoing impacts of the pandemic.",
                         "The DOD death rate in 2022 is significantly higher by 37.421 deaths per 100,000 compared to 2018, suggesting sustained elevated death rates into the later stages of the pandemic.")
    )
    
    # Create custom HTML table with borders
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid black;",
      tags$thead(
        tags$tr(
          tags$th(style = "width: 20%; border: 2px solid black; padding: 4px;", "Variable"),
          tags$th(style = "width: 17%; border: 2px solid black; padding: 4px;", "Coefficient"),
          tags$th(style = "width: 13%; border: 2px solid black; padding: 4px;", "PValue"),
          tags$th(style = "width: 50%; border: 2px solid black; padding: 4px;", "Interpretation")
        )
      ),
      tags$tbody(
        lapply(1:nrow(data), function(i) {
          tags$tr(
            tags$td(style = "width: 20%; border: 2px solid black; padding: 4px;", data$Variable[i]),
            tags$td(style = "width: 17%; border: 2px solid black; padding: 4px;", data$Coefficient[i]),
            tags$td(style = "width: 13%; border: 2px solid black; padding: 4px;", data$PValue[i]),
            tags$td(style = "width: 50%; border: 2px solid black; padding: 4px;", data$Interpretation[i])
          )
        })
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
