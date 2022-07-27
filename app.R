library(dplyr)
library(janitor)
library(rgdal)
library(readr)
library(shiny)

# Load spatial data
sp <- readOGR(".", "data/gz_2010_21_140_00_500k")
sp <- sp[sp$COUNTY %in% c("111"),]
sp <- sp[order(as.numeric(as.character(sp$NAME))),]

# Load population data
p1 <- as.data.frame(read.csv("data/DEC_10_SF1_SF1DP1_with_ann.csv"))
p2 <- as.data.frame(read.csv("data/ACS_10_5YR_S1901_with_ann.csv"))
p <- merge(p1, p2)

# Clean and merge
idx <- which(p$GEO.id=="Id")
names(p) <- p[idx,]
p <- p[-idx,]
names(p) <- make_clean_names(names(p))
p <- p %>% select(id,
                  total_population = number_sex_and_age_total_population,
                  percent_male_population = percent_sex_and_age_male_population,
                  percent_female_population = percent_sex_and_age_female_population,
                  median_male_age = number_sex_and_age_male_population_median_age_years,
                  median_female_age = number_sex_and_age_female_population_median_age_years,
                  percent_households_with_child = percent_relationship_total_population_in_households_child,
                  number_vacant_housing_units = number_housing_occupancy_total_housing_units_vacant_housing_units,
                  percent_vacant_housing_units = percent_housing_occupancy_total_housing_units_vacant_housing_units,
                  median_household_income = households_estimate_median_income_dollars,
                  mean_household_income = households_estimate_mean_income_dollars,
                  percent_white = percent_race_total_population_one_race_white,
                  percent_black_or_african_american = percent_race_total_population_one_race_black_or_african_american,
                  percent_asian = percent_race_total_population_one_race_asian,
                  percent_american_indian_and_alaska_native = percent_race_total_population_one_race_american_indian_and_alaska_native,
                  percent_native_hawaiian_and_other_pacific_islander = percent_race_total_population_one_race_native_hawaiian_and_other_pacific_islander,
                  percent_some_other_race = percent_race_total_population_one_race_some_other_race,
                  percent_hispanic_or_latino = percent_hispanic_or_latino_total_population_hispanic_or_latino_of_any_race)

sp@data <- sp@data %>%
  merge(p, by.x = "GEO_ID", by.y = "id") %>%
  select(-GEO_ID, -STATE, -COUNTY, -TRACT, -NAME, -LSAD) %>%
  mutate(total_population = as.integer(total_population)) %>%
  mutate(population_density = total_population/CENSUSAREA) %>%
  select(-CENSUSAREA) %>%
  select(total_population, population_density, everything())

# Shiny app
ui <- fluidPage(
  titlePanel("Louisville Census Visualizer"),
  sidebarLayout(
    sidebarPanel(
      p("This app visualizes data collected from the 2010 US Census in Jefferson County, Kentucky, the seat of the Louisville metropolitan area."),
      selectInput(
        inputId = "variable",
        label = "Variable",
        choices = names(sp),
        selected = "total_population"
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    spplot(sp, z=input$variable)
  })
}

shinyApp(ui, server)