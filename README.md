# Vaccine distribuition accross US. (Story telling project)
* Author: Abel Debela
* web: http://www.abelable.com

## Data source
* [US Vaccine Data (.csv)](https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv)
* [US States by Region (.csv)](https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv)

## R - Packages
* Skimr - Compact and Flexible Summaries of Data
* readr - Read Rectangular Text Data
* dplyr - A Grammar of Data Manipulation
* lubridate - Make Dealing with Dates a Little Easier
* stringr - Simple, Consistent Wrappers for Common String Operations
* DataExplorer - Automate Data Exploration and Treatment
* corrplot - Visualization of a Correlation Matrix

## Problem Description
The purpose of this project is to analyze vaccine distribution data across United States, and be able to predict total number of fully vaccinated peoples in the nation. Mainly I use a single vaccine data source, from GitHub and the data is refreshed once in a day. I am joining the vaccine data with US states by region data set in order to look the data from multiple US geographical location. 

## Data Dictionary
### United States vaccination data

* `location`: name of the state or federal entity.
* `date`: date of the observation.
* `total_vaccinations`: total number of doses administered. This is counted as a single dose, and may not equal the total number of people vaccinated, depending on the specific dose regime (e.g. people receive multiple doses). If a person receives one dose of the vaccine, this metric goes up by 1. If they receive a second dose, it goes up by 1 again.
* `total_vaccinations_per_hundred`: `total_vaccinations` per 100 people in the total population of the state.
* `daily_vaccinations_raw`: daily change in the total number of doses administered. It is only calculated for consecutive days. This is a raw measure provided for data checks and transparency, but we strongly recommend that any analysis on daily vaccination rates be conducted using `daily_vaccinations` instead.
* `daily_vaccinations`: new doses administered per day (7-day smoothed). For countries that don't report data on a daily basis, we assume that doses changed equally on a daily basis over any periods in which no data was reported. This produces a complete series of daily figures, which is then averaged over a rolling 7-day window.
* `daily_vaccinations_per_million`: `daily_vaccinations` per 1,000,000 people in the total population of the state.
* `people_vaccinated`: total number of people who received at least one vaccine dose. If a person receives the first dose of a 2-dose vaccine, this metric goes up by 1. If they receive the second dose, the metric stays the same.
* `people_vaccinated_per_hundred`: `people_vaccinated` per 100 people in the total population of the state.
* `people_fully_vaccinated`: total number of people who received all doses prescribed by the vaccination protocol. If a person receives the first dose of a 2-dose vaccine, this metric stays the same. If they receive the second dose, the metric goes up by 1.
* `people_fully_vaccinated_per_hundred`: `people_fully_vaccinated` per 100 people in the total population of the state.
* `total_distributed`: cumulative counts of COVID-19 vaccine doses recorded as shipped in CDC's Vaccine Tracking System.
* `total_distributed_per_hundred`: cumulative counts of COVID-19 vaccine doses recorded as shipped in CDC's Vaccine Tracking System per 100 people in the total population of the state.
* `share_doses_used`: share of vaccination doses administered among those recorded as shipped in CDC's Vaccine Tracking System.


## Data Summary
The US vaccine data has 14 features, and the number of observations grows by 65 every day. I am explaining. Even though the data set contains all US territories, for simplicity, the analysis focuses on the fifty US states, including the District of Colombia (DC)

### Step 1, Select the desired variables (Feature selection)
```{r}

vaccine_df2 <-  vaccineDF %>%
    select(date,
           location,
           daily_vaccinations_per_million,
           distributed_per_hundred,
           share_doses_used,
           total_distributed,
           total_vaccinations,
           people_fully_vaccinated)
```		   

### Step 2, Since I am considering only US data, I am filtering only states within the United States. 
```{r}
vaccine_df2 <- filter(vaccine_df2, vaccine_df2$location != 'United States')
```
### Step 3, New York is entered as 'New York State' - Renaming the entries for NY
```{r}
vaccine_df2$location <- str_replace_all(string=vaccine_df2$location, pattern="New York State", repl="New York")

skim(vaccine_df2)
```
### Step 4, impute missing data for shares of doses used

```{r}
vaccine_df2$share_doses_used <- if_else(is.na(vaccine_df2$share_doses_used), 
                                        mean(vaccine_df2$share_doses_used, na.rm = TRUE), 
                                        vaccine_df2$share_doses_used)

vaccine_df2$daily_vacinations_per_million <- if_else(is.na(vaccine_df2$daily_vacinations_per_million),
                                                     median(vaccine_df2$daily_vacinations_per_million, 
                                                            na.rm = TRUE), vaccine_df2$daily_vacinations_per_million)
skim(vaccine_df2)
```
### Step 5, Adding new features
```{r}
minDate = min(vaccine_df2$date)
  vaccine_df2$date_since_dist <- vaccine_df2$date - minDate 
  # Dates passed since vaccine distribution started
  # Day of the week
  vaccine_df2$day_of_the_week <- wday(vaccine_df2$date, label = TRUE)
  # Weekday or Weekend
  vaccine_df2$is_weekend <- if_else(vaccine_df2$day_of_the_week %in% c("Sat","Sun"),"Y","N")
```
### Step 6, Join the two data sets with a common field (State) 

```{r}
vaccine_df3 <- left_join(vaccine_df2, USStatesByRegion, by = c("location" = "State"))
```

### Step 7,  Remove all observations with NA values and review the remaining data set.

```{r}
vaccine_df3 <- na.omit(vaccine_df3)
  skim(vaccine_df3)
```
### Now let's examine the data from different visuals.
```{r}
plot_intro(vaccine_df3) # Needed
```
```{r}
plot_histogram(vaccine_df3, ncol=3L) # Needed
```
```{r}
plot_boxplot(vaccine_df3, by = "Division", ncol=3L)

```
```{r}
M <- cor(select(vaccine_df3,
               daily_vaccinations_per_million, 
               distributed_per_hundred, 
               share_doses_used, 
               total_distributed,
               total_vaccinations,
               people_fully_vaccinated))
corrplot(M, method = 'circle', type = 'upper', order = 'hclust')

```

### After cleaning data, I am writing the cleaned dataset to a .csv file. 

```{r}
write_csv(vaccine_df3, "vaccine_df3.csv")
```

There are plenty of R packages that can help us plot observations & features in different ways. However, knowing which packages has these plot options is not that relatively easy. Also, plotting features in R may not give us the interactivity that industry-standard visualization platforms provide us. For this reason, I am using Tableau to get some insights out from these datasets quickly. Let???s look at median vaccine distribution through weekdays.

<p align="center">
  <img src="https://github.com/AbelDebela/TBANLT560/blob/main/fullyvacinatedbyregion.png"/>
</p>

We can see that vaccine distribution in the southern part of the United States is relatively well compared to the rest of the other regions. 

<p align="center">
  <img src="https://github.com/AbelDebela/TBANLT560/blob/main/medianTotalVaccinationByDayOfWeek.png"/>
</p>

Vaccines are distributed almost equally throughout the week; however, people seem to get more vaccines during Saturday.

<p align="center">
  <img src="https://github.com/AbelDebela/TBANLT560/blob/main/totalVaccineDistributionByDivision.png"/>
</p>

Looking at the maximum distribution count, the west part of the US seems to receive the maximum number of vaccines beginning of March 2021.

<p align="center">
  <img src="https://github.com/AbelDebela/TBANLT560/blob/main/averageShareofDosesUsedByState.png"/>
</p>
<p align="center">
  <img src="https://github.com/AbelDebela/TBANLT560/blob/main/totalPeopleFullyVaccinatedByState.png"/>
</p>


<p align="justify">
    <a href="https://public.tableau.com/profile/abel3532#!/vizhome/USCovid19VaccineDistributionDataAnalysis/USVaccineDistribution?publish=yes&:showVizHome=no">Tableau Dashboard</a>

         
Now, let me split the dataset as training and test data. I am using 75% training and 25% test data for this case.
Since I intend to predict the number of fully vaccinated peoples for the next date, I hope to refresh the data every day.

```{r, warning=F, message=F }

vaccine_df4 <- vaccine_df3 %>%
    group_by(location) %>%
    filter(date == max(date)) %>%
    select(daily_vaccinations_per_million,
           distributed_per_hundred,
           share_doses_used,
           people_fully_vaccinated,
           Region,
           location) %>%
    rename(vacc_per_mil = daily_vaccinations_per_million,
           dist_per_hund = distributed_per_hundred,
           fully_vacc = people_fully_vaccinated)

vaccine_df4$location <- NULL

# Splitting the data as training & test using the rsample package. The default
# method splits dataset ast 75% training and 25 test set.
set.seed(123)
vaccine_split <- initial_split(vaccine_df4)
```
Out of 51 states, I assign 39 states as training data and 12 of them as test sets.

```{r}
vaccine_train <- training(vaccine_split)
vaccine_test <- testing(vaccine_split)
```
### Linear regression model

```{r, warning=F, message=F}
lm_spec <- linear_reg() %>%
    set_engine('lm') %>%
    set_mode('regression')

lm_fit <- lm_spec %>%
    fit(fully_vacc ~., data = vaccine_train)

# Evaluating Test 
vaccine_test_lm_res <- predict(lm_fit, new_data = vaccine_test) %>%
    rename(lm_pred = .pred)

```
### Random Forest

```{r, warning=F, message=F}

set.seed(234)
vacc_rf_spec <- rand_forest(
    mtry = 2,
    trees = 10,
    min_n = 5) %>%
    set_mode("regression") %>%
    set_engine("ranger")


vacc_rf_fit <- vacc_rf_spec %>%
    fit(fully_vacc ~., data = vaccine_train)

# Evaluating Test 
vaccine_test_rf_res <- predict(vacc_rf_fit, new_data = vaccine_test) %>%
    rename(rf_pred = .pred)
```

### K Nearest Neighbours

```{r, warning=F, message=F}
vacc_knn_spec <- nearest_neighbor() %>%
    set_engine('kknn') %>%
    set_mode("regression")

vacc_knn_fit <- vacc_knn_spec %>%
    fit(fully_vacc ~ ., data = vaccine_train) 

# Evaluating Test 
vaccine_test_knn_res <- predict(vacc_knn_fit, new_data = vaccine_test) %>%
    rename(knn_pred = .pred)
```
### Below is an averaging forecast of the three prediction models (Linear, Random Forest, and Knn) to create an ensemble prediction.

```{r, warning=F, message=F }

ensembleDF <- bind_cols(vaccine_test$fully_vacc, vaccine_test_lm_res$lm_pred, vaccine_test_rf_res$rf_pred, vaccine_test_knn_res$knn_pred)
names(ensembleDF) <- c('fully_vacc','lm_pred','rf_pred','knn_pred')
ensembleDF$model_avg <- (ensembleDF$lm_pred + ensembleDF$rf_pred + ensembleDF$knn_pred) / 3

ensembleDF
```


