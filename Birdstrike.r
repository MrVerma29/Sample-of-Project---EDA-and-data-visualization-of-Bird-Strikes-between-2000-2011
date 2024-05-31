# Importing relevant libraries
# dplyr: for data manipulation tasks like filtering, selecting, mutating, and summarizing data.
library(dplyr)
# plotly: for creating interactive plots and visualizations.
library(plotly)
# tidyr: for data tidying tasks like reshaping data frames, gathering, and spreading columns.
library(tidyr)
# ggplot2: for creating static plots and visualizations based on the grammar of graphics.
library(ggplot2)
# reshape2: simplifies data reshaping and manipulation tasks in R. (melt function)
library(reshape2)
# stringr: used for string manipulation tasks.
library(stringr)
# lubridate: used for handling date-time objects and operations.
library(lubridate)
# gridExtra: used to make extra grids to combine the plots side by side
library(gridExtra)


# Import the dataset
birdstrike <- read.csv("C://Users//verma//Downloads//Bird Strikes data.csv")

# First 5 Rows
head(birdstrike)

#Last 5 Rows
tail(birdstrike)

# Dimension of the Dataset
dim(birdstrike)

# Generating Summary Statistics of each column of the dataset
summary(birdstrike)

# Datatype of the each of the column of the dataset
str(birdstrike)

# Count the number of the duplicates
sum(duplicated(birdstrike))         #There are no duplicates

# Count the number of null values in each column
na_count <- colSums(is.na(birdstrike))
na_count                            #There is no null values


# Our dataset still contains some non-finite values (e.g., Inf, -Inf, NaN) or values outside the scale range, which are causing the warnings in your plotting code.

# Identify columns with empty or blank values

# So first, identify the columns that contain empty or blank values
cols_with_blanks <- sapply(birdstrike, function(x) any(x == ""))
cols_with_blanks <- names(cols_with_blanks)[cols_with_blanks]

cols_with_blanks

# Now, replace the empty or blank values in those columns with NA using the na_if function from the dplyr package
birdstrike <- birdstrike %>%
  mutate_at(vars(one_of(cols_with_blanks)), ~na_if(., ""))
# We are not removing the empty cells because it is in a lots of columns, so if we'll remove it it can lead to distortion of dataset which can further lead to inaccurate data.


# The date column is unstructured and is not in a constant structure or format and also the value of time is zero, so it is better to remove it 

# Removing the time and restructuring the Date column into yyyy-mm-dd format
birdstrike <- birdstrike %>%
  mutate(FlightDate = str_replace(FlightDate, " \\d+:\\d+", "")) %>%  # Remove time portion
  mutate(FlightDate = coalesce(
    dmy(FlightDate, tz = "UTC"),
    mdy(FlightDate, tz = "UTC"),
    ymd(FlightDate, tz = "UTC")
  )) %>%
  mutate(FlightDate = date(FlightDate))

birdstrike$FlightDate


# Identify the columns with the numerical data
num_var <- birdstrike %>% select_if(is.numeric) %>% colnames() 
num_var

# Replacing the special characters from the columns "Cost: Total $"	& "Feet above ground"
birdstrike$Cost..Total.. <- str_replace_all(birdstrike$Cost..Total.., "," , "")             #Replace the special character
birdstrike$Feet.above.ground <- str_replace_all(birdstrike$Feet.above.ground, "," , "")     #Replace the special character
birdstrike$Aircraft..Airline.Operator <- str_replace_all(birdstrike$Aircraft..Airline.Operator, "\\*" , "")

head(birdstrike$Cost..Total..)
str(birdstrike$Cost..Total..)

head(birdstrike$Feet.above.ground)
str(birdstrike$Feet.above.ground)

# We'll convert the columns into numeric as it will help us in the analysis
birdstrike$Cost..Total.. <- as.numeric(birdstrike$Cost..Total..)
birdstrike$Feet.above.ground <- as.numeric(birdstrike$Feet.above.ground)

str(birdstrike$Cost..Total..)
str(birdstrike$Feet.above.ground)


# Again Identify the columns with the numerical data
num_var <- birdstrike %>% select_if(is.numeric) %>% colnames() 
num_var

# Removing the RecordID because it is not useful for analysis
num_var <- c("Wildlife..Number.Struck.Actual", "Cost..Total..", "Feet.above.ground", "Number.of.people.injured")
num_var

#Identify the columns with the categorical data
cat_var <- setdiff(names(birdstrike), num_var)
cat_var

# Subset the numerical columns from the birdstrike dataset
num_varss <- birdstrike[, sapply(birdstrike, is.numeric)]
cat_varss <- birdstrike[, sapply(birdstrike, is.character)]



# Check the outliers
# Before treatment boxplot
before_plot <- ggplot(melt(num_varss), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "Before Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
before_plot



# Analysing The Outliers to make the decision
ggplot(birdstrike, aes(x = Wildlife..Size, y = Cost..Total.., color = Is.Aircraft.Large., shape = Effect..Indicated.Damage, size = Wildlife..Number.Struck.Actual)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("blue", "red"), labels = c("Small", "Large"), name = "Aircraft Size") +
  scale_shape_manual(values = c(15, 16, 17, 18), labels = unique(birdstrike$Effect..Indicated.Damage), name = "Indicated Damage") +
  scale_size_continuous(range = c(1, 6), name = "Number of Wildlife Struck") +
  scale_y_continuous(labels = scales::comma, name = "Total Cost ($)") + # Formats y-axis labels as comma-separated values
  labs(title = "Total Cost vs. Multiple Variables", x = "Wildlife Size", y = "Total Cost ($)") +
  theme_minimal()
# We'll treat the outliers because it is in one of the major column that is the "Total Cost" which is a lot useful in finding out the various inisghts from the dataset. So, with aim to not distort out dataset we'll replace the value accordingly through IQR method.




# Treating the outliers using the IQR method
# Calculate IQR
Q1 <- quantile(num_varss$Cost..Total.., 0.25)
Q3 <- quantile(num_varss$Cost..Total.., 0.75)
IQR <- Q3 - Q1

# Determine upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the nearest non-outlier value within bounds
num_varss$Cost..Total..[num_varss$Cost..Total.. < lower_bound] <- lower_bound
num_varss$Cost..Total..[num_varss$Cost..Total.. > upper_bound] <- upper_bound

# After treatment boxplot
after_plot <- ggplot(melt(num_varss), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "After Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
after_plot

# Combine plots side by side (Treatment Comparison)
grid.arrange(before_plot, after_plot, nrow = 1)





# Univariate Analysis of Numerical Columns

# Analyzing the distribution of the number of wildlife struck in bird strikes
ggplot(birdstrike, aes(x = Wildlife..Number.Struck.Actual)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Number of Wildlife Struck",
       x = "Number of Wildlife Struck", y = "Frequency") +
  theme_minimal()


# Investigating the distribution of total cost incurred due to bird strikes
ggplot(birdstrike, aes(x = Cost..Total..)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "Distribution of Total Cost Incurred",
       x = "Total Cost ($)", y = "Frequency") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal()


# Understanding the distribution of altitude of airplanes at the time of strike
ggplot(birdstrike, aes(x = Feet.above.ground)) +
  geom_histogram(fill = "lightpink", color = "black", bins = 30) +
  labs(title = "Distribution of Altitude of Airplanes",
       x = "Altitude (feet)", y = "Frequency") +
  theme_minimal()


# Examining the distribution of the number of people injured in bird strike incidents
ggplot(birdstrike, aes(x = Number.of.people.injured)) +
  geom_histogram(fill = "lightyellow", color = "black", bins = 30) +
  labs(title = "Distribution of Number of People Injured",
       x = "Number of People Injured", y = "Frequency") +
  theme_minimal()



# Univariate Analysis of Categorical Columns


# 1. Yearly Analysis of Birdstrike

# Calculate yearly strikes
yearly_strikes_us <- birdstrike %>%
  filter(!is.na(Origin.State)) %>%
  mutate(Year = format(as.Date(FlightDate, "%m/%d/%Y"), "%Y")) %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  filter(!is.na(Year)) %>%  # Filter out NA values in the Year column
  arrange(Year)

# Plot as line chart with values
ggplot(yearly_strikes_us, aes(x = Year, y = count, label = count)) +
  geom_line(aes(group = 1), color = "#3182bd") +  # Line chart
  geom_point(color = "#3182bd", size = 2) +  # Points for each year
  geom_text(vjust = -0.7, size = 3) +  # Add labels above points
  labs(title = "Yearly Analysis of Birdstrike", x = "Year", y = "Frequency") +
  theme_minimal()
# There is an immediate hike in number of strikes from year 2008 to 2009. But to be noted then some of the major steps had been taken after year 2009 as then there is a continuous decrease in the strikes number in 2010 and 2011. But overall there is a increase in the number of strikes if we see from the year 2000 to 2011.



# 2. Top 10 US Airlines that have encountered bird strikes
top_airlines <- birdstrike %>%
  group_by(Aircraft..Airline.Operator) %>%
  filter(!is.na(Aircraft..Airline.Operator)) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# Calculate percentage
top_airlines <- mutate(top_airlines, percentage = count/sum(count) * 100)

# Create pie chart
ggplot(top_airlines, aes(x = "", y = count, fill = Aircraft..Airline.Operator)) +
  geom_col(color = 'black', stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Airlines with Most Bird Strikes", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "right")
# Maximum share for number of strike is taken by group of major airlines (that operates in the passenger segment) with "Southwest Airlines" having highest share of strike. Then, in second is the group of regional airlines like "American Eagle", "Skywest Airlines" , "US Airlines". But, one socking spot is of "UPS Airlines" as it is the only "Cargo Airline" that is in this list.



# 3. Airports with most incidents of bird strikes - Top 50
top_airports <- birdstrike %>%
  group_by(Airport..Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(50)

ggplot(top_airports, aes(x = reorder(Airport..Name, count), y = count)) +
  geom_col() +
  labs(title = "Top 50 Airports with Most Birdstrike", x = "Airport", y = "Frequency") +
  coord_flip()
# I think airlines need to take extra safety in the the top two airport that are "DALLAS/FORT WORTH INTL Airport" and "SACRAMENTO Airport" beacuse their strikes number is shockingly huge with respect to the other 48 airports.



# 4. Different wildlife species involved in bird strikes.

# Filter data to include only wildlife species with strikes
species_strikes <- birdstrike %>%
  filter(!is.na(Wildlife..Species)) %>%
  group_by(Wildlife..Species) %>%
  summarise(strike_count = n(), .groups = "drop") %>%
  arrange(desc(strike_count)) %>%
  top_n(15)

# Generate a vector of 15 distinct colors
color_palette <- scales::hue_pal()(15)

# Plot top 15 wildlife species by strike count
ggplot(species_strikes, aes(x = strike_count, y = reorder(Wildlife..Species, strike_count), fill = Wildlife..Species)) +
  geom_col(color = "black") +
  labs(title = "Top 15 Wildlife Species Involved in Birdstrike", x = "Frequency", y = "Wildlife Species") +
  geom_text(aes(label = scales::comma(strike_count)), hjust = -0.5, color = "black", size = 3) + # Add value labels above bars
  scale_fill_manual(values = color_palette) +  # Using a manual color palette
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))  # Align y-axis labels to the left
# In univariate analysis of "Total Cost" the reason for high distribution on zero total cost can be because of the reason of strike of unknown small bird as its no of strike is incredibly huge and the second spot is taken by an unknown medium sized bird. Overall, We can see that in the Top 15 species maximum are the small sized bird which be ba a major reason to high distribution of zero "Total Cost" & "People Injured" but then it is followed bu a bit of medium to large sized wildlife or birds.



# 5. The impact of bird strikes on flights.

# Filter data to include only impact of strike to flights
strike_impact <- birdstrike %>%
  filter(!is.na(Effect..Impact.to.flight)) %>%
  group_by(Effect..Impact.to.flight) %>%
  summarise(count = n(), .groups = "drop")

ggplot(strike_impact, aes(x = "", y = count, fill = Effect..Impact.to.flight)) +
  geom_col(color = 'black', stat = "identity") +  # Stacked bar chart
  labs(title = "Birdstrikes Impact on Flights ", x = "", y = "Frequency") +
  scale_fill_brewer(palette = "Paired") +  # Using the "Paired" color palette
  theme_minimal()
# As from above chart we got insight that strike of small bird is most in number this can be the major reason that mostly there is no effect or impact of the flight after the strike, but since number of medium to large sized wildlife or birds is more it has impacted the airplane.




# 6. Frequency of Bird Strikes at Different Heights Above Ground

# Filter the data to include only the column Feet.above.ground and remove NA values
feet_data <- birdstrike %>%
  filter(!is.na(Feet.above.ground))

# Calculate the average feet above the ground for each bird strike
feet_data <- feet_data %>%
  group_by(Feet.above.ground) %>%
  summarise(Frequency = n()) %>%
  filter(!is.null(Feet.above.ground)) %>%
  top_n(10, Frequency) %>%
  arrange(desc(Frequency))

# Use the filtered data to create the graph
ggplot(feet_data, aes(x = reorder(Feet.above.ground, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = Frequency), vjust = -0.5, color = "black", size = 3) +  # Display values on top of bars
  labs(title = "Frequency of Bird Strikes at Different Heights Above Ground",
       x = "Average Feet Above Ground", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# We can get the overall information that major strike has happened at the ground level. Also, if we see the range of 0 to 500 there are maximum number of strikes.





# 7. The phase of flight during which bird strikes occur.

# Convert When..Phase.of.flight to factor
birdstrike$When..Phase.of.flight <- as.factor(birdstrike$When..Phase.of.flight)

# Average Altitude of the aeroplanes in different phases at the time of strike
phase_of_flight <- birdstrike %>%
  filter(!is.na(When..Phase.of.flight)) %>%
  count(When..Phase.of.flight) %>%
  arrange(desc(n))  # Arrange by count in descending order

ggplot(phase_of_flight, aes(x = reorder(When..Phase.of.flight, -n), y = n, fill = When..Phase.of.flight)) +
  geom_col(color = "black") +  # Add black outline to the bars
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3) +  # Add value labels above the bars
  labs(title = "Phase of Flight During Birdstrike", x = "Phase of Flight", y = "Frequency") +
  scale_fill_brewer(palette = "Paired") +  # Using the "Paired" color palette
  theme_minimal()
# The phase of flight satisfy the altitude range that we have got from the above chart as these all phase lies uder the altitude range of 0 to 500 ft. with approach phase as the major strike prone with a higenumber difference than the rest of the phases.





# 8. Effect of Sky Condition on Bird Strike

# Sky conditions effect
sky_conditions_effect <- birdstrike %>%
  filter(!is.na(Conditions..Sky)) %>%
  count(Conditions..Sky) %>%
  arrange(desc(n))  # Arrange by count in descending order

# Reorder the levels of Conditions..Sky
sky_conditions_effect$Conditions..Sky <- factor(sky_conditions_effect$Conditions..Sky, levels = sky_conditions_effect$Conditions..Sky)

# Create the pie chart
ggplot(sky_conditions_effect, aes(x = "", y = n, fill = Conditions..Sky)) +
  geom_bar(stat = "identity", color = "black") +  # Use geom_bar with stat = "identity" to plot values directly
  coord_polar("y", start = 0) +  # Convert cartesian coordinates to polar coordinates
  geom_text(aes(label = paste0(round(n / sum(n) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
  labs(title = "Sky Condition During Birdstrike", x = NULL, y = NULL) +  # Remove x and y axis labels
  theme_void() +  # Remove gridlines, background, etc.
  scale_fill_brewer(palette = "Dark2")  # Using a professional color palette
# Major time the sky condition was clear when the strike happened and second with some amount of the cloud. Thus, more safety and precautions is needed to be taken during the major time. The "Overcast" sky condition has the  lowest strike rate as compare to the both.




# 9. Effect of Precipitation on Birdstrike

# Filter data to include only precipitation effect
precipitation_effect <- birdstrike %>%
  filter(!is.na(Conditions..Precipitation)) %>%
  group_by(Conditions..Precipitation) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) # Arrange by descending count

# Create line chart with value labels
ggplot(precipitation_effect, aes(x = Conditions..Precipitation, y = count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +
  geom_text(aes(label = count), vjust = -0.9, size = 3, color = "black") +  # Add value labels
  labs(title = "Precipitation Condition During Birdstrike", x = "Precipitation Conditions", y = "Frequency") +
  theme_minimal()
# Maximum time there was no precipitation with rain at second and fog at third which can also be confirmed from the "SkY Condition" chart as max time the sky was clear with with only some cloud.




# 10. If pilots were warned about birds or wildlife before strikes.

# Were Pilots Informed?
pilot_warned <- birdstrike %>%
  filter(!is.na(Pilot.warned.of.birds.or.wildlife.)) %>%
  group_by(Pilot.warned.of.birds.or.wildlife.) %>%
  summarise(count = n(), .groups = "drop")

# Calculate percentages
pilot_warned <- pilot_warned %>%
  mutate(percentage = count / sum(count) * 100)

# Plot stacked bar chart with percentages
ggplot(pilot_warned, aes(x = "", y = count, fill = factor(Pilot.warned.of.birds.or.wildlife.))) +
  geom_col(color = "black" , stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "black") +
  labs(title = "Were Pilots Warned About Birds?", x = NULL, y = "Frequency") +
  scale_fill_manual(values = c("#FF9999", "#66CCFF"), 
                    name = "Pilot Warned",
                    labels = c("No", "Yes")) +
  theme_minimal() +
  theme(legend.position = "bottom")
# As till now we have go to know that maximum time the sky was clear and with the less cloud the ATC of the airports should already inform the pilots about the birds or the wildlife because as we can see which is node done by them maximum times.





# Bivariate Analysis


# 1. Total Cost Incurred Each Year Due To Birdstrike
yearly_cost <- birdstrike %>%
  filter(!is.na(FlightDate) & !is.na(Cost..Total..)) %>%
  mutate(Year = format(as.Date(FlightDate, "%m/%d/%Y"), "%Y")) %>%
  group_by(Year) %>%
  summarise(total_cost = sum(Cost..Total..))

# Calculate average cost
avg_cost <- mean(yearly_cost$total_cost)
avg_cost

ggplot(yearly_cost, aes(x = Year, y = total_cost)) +
  geom_col(color = "black", fill = "#3182bd") +
  geom_hline(yintercept = avg_cost, color = "red", linetype = "dashed") +  # Add average line
  geom_text(aes(label = scales::comma(total_cost)), vjust = -0.5, color = "black", size = 3) + # Add value labels above bars
  labs(title = "Total Cost Incurred Each Year Due To Birdstrike", x = "Year", y = "Total Cost ($)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
# Average cost for the year from 2000 to 2011 is $ 1,17,95,598 and three years has crossed this mark 2001 with highest, 2006 with the second highest and 2003 with thirs highest year according to "Tota Cost" of the year. Also, to be noted from the year 2007 to 2011 the "Total Cost" is fluctuating around the the "Average Total Cost" line which is needed to be investigated.




# 2. Effect of Strike at Different Altitude
altitude_effect <- birdstrike %>%
  filter(!is.na(Effect..Impact.to.flight)) %>%
  group_by(Altitude.bin, Effect..Impact.to.flight) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count, na.rm = TRUE))

ggplot(altitude_effect, aes(x = Altitude.bin, y = proportion, fill = Effect..Impact.to.flight)) +
  geom_col(color = "black", position = "fill") +
  labs(title = "Effect of Bird Strikes at Different Altitudes", x = "Altitude Bin", y = "Proportion") +
  scale_fill_brewer(palette = "Dark2")
# Regardless the altitude that maximum times there is no impact to the flight as maximum proportion of stikes is done by the small sized birds. Alo, one thing to be noticed the proportion of precautionary landing ia approximately same in both < 10000 or more that 10000ft of altitude.




# 3. Average Altitude of aeroplane at different Phase of Strike
# Convert When..Phase.of.flight to factor
birdstrike$When..Phase.of.flight <- as.factor(birdstrike$When..Phase.of.flight)

# Average Altitude of the aeroplanes in different phases at the time of strike
avg_altitude_by_phase <- birdstrike %>%
  filter(!is.na(Feet.above.ground) & !is.na(When..Phase.of.flight)) %>%
  group_by(When..Phase.of.flight) %>%
  summarise(avg_altitude = mean(Feet.above.ground), .groups = "drop")

# Create an area chart with a line
ggplot(avg_altitude_by_phase, aes(x = When..Phase.of.flight, y = avg_altitude, group = 1)) +
  geom_area(fill = "skyblue", color = "black", alpha = 0.5) +  # Fill the area with skyblue color and adjust transparency
  geom_line(color = "blue", size = 0.5) +  # Add a black line with thicker size
  geom_point(color = "blue", size = 2) +  # Add points with black color and larger size
  geom_text(aes(label = round(avg_altitude, 2)), vjust = -0.7, color = "black", size = 3) +  # Display altitude values on top of the points
  labs(title = "Average Altitude of Birdstrike at Different Phases",
       x = "Phase of Flight", y = "Average Altitude (feet)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# 4. Prior Warning and Effect of Strike Relation
warning_effect <- birdstrike %>%
  filter(!is.na(Pilot.warned.of.birds.or.wildlife.)) %>%
  group_by(Pilot.warned.of.birds.or.wildlife., Effect..Impact.to.flight) %>%
  summarise(count = n(), .groups = "drop")

# Create a bar chart with dodged bars
ggplot(warning_effect, aes(x = Pilot.warned.of.birds.or.wildlife., y = count, fill = Effect..Impact.to.flight)) +
  geom_col(color = "black", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) +  # Add value labels on top of each bar
  labs(title = "Prior Warning and Effect of Birdstrike", x = "Pilot Warned", y = "Frequency") +
  scale_fill_brewer(palette = "Set2")
# As, I told you before that pilots are not being informed about the birds or wildlife despite of sky being clear or little cloudy 80% of the time, we can see if they does this there is good amount of decrease in the number of strike that usually happens when not informed.





# 5. Average Cost Costed by Top Wildlife Species
# Filter data to include only top species based on strike
species_cost <- birdstrike %>%
  filter(!is.na(Wildlife..Species) & !is.na(Cost..Total..)) %>%
  group_by(Wildlife..Species) %>%
  summarise(avg_cost = mean(Cost..Total.., na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_cost)) %>%
  top_n(10) #WE have removed NA so it will show 9

# Wildlife species vs. cost
ggplot(species_cost, aes(x = reorder(Wildlife..Species, avg_cost), y = avg_cost, fill = factor(Wildlife..Species))) +
  geom_col(color = "black") +  # Add black outline to the bars
  geom_text(aes(label = paste0("$", format(avg_cost, big.mark = ","))), stat = "identity", hjust = 0, size = 3) +  # Add text labels with average cost values
  labs(title = "Average Cost Costed by Top Wildlife Species", x = "Wildlife Species", y = "Average Cost ($)") +
  scale_fill_discrete() +  # Using different color for each bar
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  coord_flip()
# Based on the average cost the medium to large sized birds costs the airlines more than the other. But here one exception case of dogs also which costs the airlines.





# Multivariate Analysis


# 1. Year v/s No of Strikes v/s Top Wildlife Species
# Extract year from FlightDate
birdstrike$Year <- format(as.Date(birdstrike$FlightDate, "%m/%d/%Y"), "%Y")

# Filter out NA values in Wildlife Species column and Year column
birdstrike_filtered <- birdstrike %>%
  filter(!is.na(Wildlife..Species) & !is.na(Year))

# Group by Year and Wildlife Species, count occurrences, and select top 5 species for each year
top_species_by_year <- birdstrike_filtered %>%
  group_by(Year, Wildlife..Species) %>%
  summarise(Count = n()) %>%
  group_by(Year) %>%
  top_n(4, Count) %>%
  ungroup() %>%
  arrange(Year, desc(Count))

# Plot stacked bar chart with professional colors
ggplot(top_species_by_year, aes(x = Year, y = Count, fill = factor(Wildlife..Species))) +
  geom_col(color = "black", position = "stack") +
  labs(title = "Year v/s No of Strikes v/s Top Wildlife Species",
       x = "Year", y = "Number of Strikes",
       fill = "Top Wildlife Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Over the period of 2000 to 2011 there is increase in number of strike of small birds (unknown) though they don't have any impact on the aiplanes, but then also the continuous increase in their strike numbers needs to be considered and taken care.And with medium bird (unknown) having usually same not of strikes in the current period of 2000 to 2011. And at last to be noted "Barn Swallow" has good number of strikes but onlu in the year of 2010 but "European Starling" though small in number but have continuous number of strike in every year which could be a big threat in the coming years.




# 2. Year v/s Total Cost Incurred Each Year v/s Top Wildlife Species

# Filter out NA values
birdstrike_filtered <- birdstrike %>%
  filter(!is.na(FlightDate) & !is.na(Cost..Total..) & !is.na(Wildlife..Species))

# Group by year and wildlife species, calculate total cost for each species
total_cost_by_species <- birdstrike_filtered %>%
  group_by(Year, Wildlife..Species) %>%
  summarise(total_cost = sum(Cost..Total..)) %>%
  ungroup()

# Identify top 10 wildlife species based on total cost
top_10_species <- total_cost_by_species %>%
  group_by(Wildlife..Species) %>%
  summarise(total_cost = sum(total_cost)) %>%
  top_n(8, total_cost) %>%
  arrange(desc(total_cost)) %>%
  pull(Wildlife..Species)

# Filter data to include only top 10 wildlife species
filtered_data <- total_cost_by_species %>%
  filter(Wildlife..Species %in% top_10_species)

# Reorder Wildlife..Species in descending order within each year
filtered_data <- filtered_data %>%
  mutate(Wildlife..Species = factor(Wildlife..Species, levels = rev(top_10_species)))

# Group filtered data by year and summarize total cost
yearly_cost_by_species <- filtered_data %>%
  group_by(Year) %>%
  summarise(total_cost = sum(total_cost)) %>%
  ungroup()

# Create stacked bar chart
ggplot(filtered_data, aes(x = factor(Year), y = total_cost, fill = Wildlife..Species)) +
  geom_col(color = "black") +
  labs(title = "Year v/s Total Cost Incurred Each Year v/s Top Wildlife Species",
       x = "Year", y = "Total Cost ($)",
       fill = "Wildlife Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma) # Convert y-axis notation to regular values
# The "Canada Goose has the continuously impacted and costed the airlines but in the recent year its share of costing has decreased. But in the recent year the "Bald Eagle" has becaome a big threat to the airlines as it is having the share of more than 40% of the cost is incurred during the recent years of 2010 and 2011.




# 3. Wildlife Species v/s No of Strikes v/s Impact on Flight
species_impact <- birdstrike %>%
  filter(Wildlife..Species %in% species_cost$Wildlife..Species & !is.na(Effect..Impact.to.flight)) %>%
  count(Wildlife..Species, Effect..Impact.to.flight) %>%
  arrange(desc(n))  # Arrange by count in descending order

ggplot(species_impact, aes(x = Wildlife..Species, y = n, fill = Effect..Impact.to.flight)) +
  geom_col(position = "stack", color = "black") +
  labs(title = "Wildlife Species v/s No of Strikes v/s Impact on Flight", x = "Wildlife Species", y = "Number of Strikes") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()
# The medium to large sized wildlife or bird has impacted the flight majorly leading to the precautionary landing where "American White Pelican" has 75% rate of precautionary landing. Only "Trumpeter Swan" is the one whih leads to the engine shut donw else the left other has no impact on the flight majorly.




# 4. Top Wildlife Species v/s Number of People Injured v/s Phase of Flight

# Filter out rows where Number.of.people.injured is greater than 0
birdstrike_filtered <- birdstrike %>%
  filter(Number.of.people.injured > 0)

# Get the top 10 species based on the number of people injured
top_10_species <- birdstrike_filtered %>%
  group_by(Wildlife..Species) %>%
  summarise(total_injuries = sum(Number.of.people.injured)) %>%
  arrange(desc(total_injuries)) %>%
  head(10)

# Filter birdstrike data for the top 10 species
birdstrike_top_10_species <- birdstrike_filtered %>%
  filter(Wildlife..Species %in% top_10_species$Wildlife..Species)

# Filter out NA values from When..Phase.of.flight
birdstrike_top_10_species <- birdstrike_top_10_species %>%
  filter(!is.na(When..Phase.of.flight))

# Create the stacked bar chart filled with the phase of flight
ggplot(birdstrike_top_10_species, aes(x = Wildlife..Species, fill = When..Phase.of.flight)) +
  geom_bar() +
  labs(title = "Top Wildlife Species v/s Number of People Injured v/s Phase of Flight",
       x = "Wildlife Species", y = "Number of People Injured", fill = "Phase of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# These are the low frequency animals bit with high injury rate as "Black Vulture" with the highest rate of 5 people and "Turkey Vulture" with the second highest with 2 person. Overall medium to large sized wildlife has caused any injury but majorly the medium sized wildlife.





