####################################################
###### FAA Damaging Strikes Over Time (Table) ######
####################################################

# Filter rows where Damaging.Strike is "Yes"
faa <- faa %>%
  filter(INDICATED_DAMAGE == "TRUE")

# Count the number of occurrences of each unique value in the year column (to sum strikes).
faa_damage_counts_df <- as.data.frame(table(faa$INCIDENT_YEAR))
colnames(faa_damage_counts_df) <- c("Year", "FAA.Count") # Rename the columns for clarity

# Look at FAA damaging strikes.
# Note that there are inconsistencies between WCAA and FAA strike data.
print(faa_damage_counts_df)


#####################################################
###### WCAA Damaging Strikes Over Time (Table) ######
#####################################################

# Filter rows where Damaging.Strike is "Yes".
damage <- dat %>%
  filter(Damaging.Strike == "Yes")

# Count the number of occurrences of each unique value in the year column.
wcaa_damage_counts_df <- as.data.frame(table(damage$Strike.Year))
colnames(wcaa_damage_counts_df) <- c("Year", "WCAA.Count") # Rename the columns for clarity

# Look at WCAA damaging strikes.
# Note that there are inconsistencies between WCAA and FAA strike data.
print(wcaa_damage_counts_df)


##########################################################
###### Combine WCAA, Operations, & FAA Data (Table) ######
##########################################################

# Ensure Year is the same type in both dataframes before merge
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

# Reformat Year in Ops data.
ops <- ops %>%
  mutate(Year = as.integer(as.character(Year)))

# Add the missing year 2023 to wcaa_damage_count_df.
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  complete(Year = unique(ops$Year), fill = list(WCAA.Count = 0))

# Perform the left join and select relevant columns
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  left_join(ops, by = "Year") %>%
  select(Year, WCAA.Count, Operations)  # Keep only the relevant columns

# View the updated dataframe.
print(wcaa_damage_counts_df)

# Convert Year columns to integer for both dataframes
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

faa_damage_counts_df <- faa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

# Ensure both data frames contain the same range of years without overwriting real data
all_years <- unique(c(wcaa_damage_counts_df$Year, faa_damage_counts_df$Year))
faa_damage_counts_df <- faa_damage_counts_df %>%
  complete(Year = all_years, fill = list(FAA.Count = NA)) # Preserve existing values, fill only missing rows with NA

# Perform the left join to add FAA.Count to wcaa_damage_counts_df
damage_counts_df <- wcaa_damage_counts_df %>%
  left_join(faa_damage_counts_df %>% select(Year, FAA.Count), by = "Year")

# Set FAA.Count equal to WCAA.Count for the year 2024.
# This step will account for the time lag with FAA data. 
# Note that this should not be done if FAA data is available for the entire year.
damage_counts_df <- damage_counts_df %>%
  mutate(FAA.Count = if_else(Year == 2024, WCAA.Count, FAA.Count))

damage_counts_df <- damage_counts_df %>%
  select(Year, Operations, WCAA.Count, FAA.Count)  # Reorder column

# Verify the updated dataframe
print(damage_counts_df)

# Save table.
write.csv(damage_counts_df, "Table_Annual_Damage_And_Operations.csv", row.names = FALSE)


#####Prep for control plot

# Calculate annual rates and summarize the data
summary_data <- damage_counts_df %>%
  group_by(Year) %>%
  summarize(
    total_count = sum(FAA.Count),           # Total count by year
    total_operations = sum(Operations) # Total operations by year
  ) %>%
  mutate(
    Rate = (total_count / total_operations) * 100000 # Calculate rate per 100,000
  )

# Calculate the center line (mean) and control limits (2 SDs)
CL <- mean(summary_data$Rate)                       # Center Line (mean rate)
UCL <- CL + 2 * sd(summary_data$Rate)              # Upper Control Limit (95.45%)
LCL <- max(CL - 2 * sd(summary_data$Rate), 0)      # Lower Control Limit, set to 0 if negative

# Add control limits to the summary_data dataframe
summary_data <- summary_data %>%
  mutate(
    CL = CL,
    UCL = UCL,
    LCL = LCL
  )

# View the summarized data
print(summary_data)

# Save table
write.csv(summary_data, "Table_Annual_Damage_Rates.csv", row.names = FALSE)

# Plot
# Count per year--control chart.
damage.rate <- ggplot(summary_data, aes(x = Year, y = Rate)) +
  geom_hline(aes(yintercept = CL), linetype = "solid", color = "gray", linewidth = 1.5) +
  geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "gray", linewidth = 1.5) +
  geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "gray", linewidth = 1.5) +
  geom_line(color = "firebrick4", size = 2, alpha = 0) +
  geom_point(color = "firebrick4", size = 5, alpha = 0) +
  #geom_line(color = "firebrick4", size = 2) +
  #geom_point(color = "firebrick4", size = 5) +
  labs(
    x = "Year",
    y = "Damaging strikes\n per 100,000 operations"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y =element_text(size = 16, color = "black"),
    axis.text.x =element_text(size = 16, color = "black"))

# Take a look.
damage.rate

# Save plot.
ggsave("Plot_Annual_Damage_Rate.jpeg", plot = damage.rate, width = 8, height = 6, dpi = 300)



############################
##### I NEED TO CLEAN UP AND BETTER ANNOTATE STUFF BELOW ###
############################



############################
##### Quick Summary Data ###
############################

# Count the number of occurrences of each unique value in the Guild column
guild_counts_df <- as.data.frame(table(dat$Guild))
colnames(guild_counts_df) <- c("Guild", "Count") # Rename the columns for clarity
print(guild_counts_df) # Print the data frame

# Count the number of occurrences of each unique value in the species column
species_counts_df <- as.data.frame(table(dat$Species.Name))
colnames(species_counts_df) <- c("Species", "Count") # Rename the columns for clarity
print(species_counts_df) # Print the data frame


#############################
##### Strikes by Runway #####
#############################

# Count the number of occurrences of each unique value in the  column.
runway_counts_df <- as.data.frame(table(dat$Runway.Taxiway))
colnames(runway_counts_df) <- c("Runway", "Count") # Rename the columns for clarity

# Resort in descending order.
runway_counts_df <- runway_counts_df %>%
  arrange(desc(Count))

# Plot strike count by guild.
ggplot(runway_counts_df, aes(x = reorder(Runway, Count), Count)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  labs(
    x = "Runway",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) + coord_flip()







# AI suggestion for above code, below trying to have 2 bars, 2024 and other (proportions).
# MAybe use FAA data for other years and WCAA for 2024.

# Group data by Year and Runway.Taxiway and count occurrences
runway_counts_df <- dat %>%
  group_by(Strike.Year, Runway.Taxiway) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Category = ifelse(Strike.Year == 2024, "2024", "Other Years")
  )

# Calculate the total Count per Category (2024 vs. Other Years) and Runway
proportions_df <- runway_counts_df %>%
  group_by(Category, Runway.Taxiway) %>%
  summarise(TotalCount = sum(Count), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Proportion = TotalCount / sum(TotalCount))

# Plot the proportions as a grouped bar chart
ggplot(proportions_df, aes(x = reorder(Runway.Taxiway, -TotalCount), y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Runway",
    y = "Proportion of Total Strikes",
    fill = "Year Category"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) +
  coord_flip()





############################
##### Strikes by Guild #####
############################

# Count the number of occurrences of each unique value in the Guild column.
guild_counts_df <- as.data.frame(table(dat$Guild))
colnames(guild_counts_df) <- c("Guild", "Count") # Rename the columns for clarity

# Resort in descending order.
guild_counts_df <- guild_counts_df %>%
  arrange(desc(Count))

# Save full list as csv for copying over numbers later.
#write.csv(guild_counts_df, "Table_Strikes_by_Guild_2024.csv", row.names = FALSE)

# Plot strike count by guild.
ggplot(guild_counts_df, aes(x = reorder(Guild, Count), Count)) +
  geom_bar(stat = "identity", fill = "palegreen4") +
  labs(
    x = "Guild",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) + coord_flip()



#######################################
##### Strike Composition by Guild #####
#######################################

# Step 1: Count occurrences per Guild and Strike.Year
guild_yearly_counts <- dat %>%
  group_by(Strike.Year, Guild) %>%
  summarise(Count = n(), .groups = "drop")

# Step 2: Compute historical averages and confidence intervals per Guild
guild_avg_df <- guild_yearly_counts %>%
  group_by(Guild) %>%
  summarise(
    Avg_Count = mean(Count),    # Mean yearly count
    SD = sd(Count),             # Standard deviation
    SE = SD / sqrt(n()),        # Standard error
    CI_Lower = Avg_Count - 1.96 * SE,  # 95% CI Lower Bound
    CI_Upper = Avg_Count + 1.96 * SE   # 95% CI Upper Bound
  )

# Step 3: Get the most recent year's data
latest_year <- max(guild_yearly_counts$Strike.Year)
guild_latest_df <- guild_yearly_counts %>%
  filter(Strike.Year == latest_year) %>%
  left_join(guild_avg_df, by = "Guild") %>%
  mutate(
    Pct_Change = (Count - Avg_Count) / Avg_Count * 100  # Percent Change
  )

# Step 4: Plot percent change with confidence intervals
ggplot(guild_latest_df, aes(x = reorder(Guild, Pct_Change), y = Pct_Change, fill = Pct_Change > 0)) +
  geom_col() +
  geom_errorbar(aes(ymin = (CI_Lower - Avg_Count) / Avg_Count * 100, 
                    ymax = (CI_Upper - Avg_Count) / Avg_Count * 100), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +  # Flip for better readability
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("red", "green"), guide = "none") +
  labs(title = paste("Percent Change from Historical Average with 95% CIs (", latest_year, ")", sep = ""),
       x = "Guild",
       y = "Percent Change (%)",
       caption = "Error bars represent 95% confidence intervals for the historical average.")


##############################
##### Strikes by Species #####
##############################

# Count the number of occurrences of each unique value in the Species. Name column.
species_counts_df <- as.data.frame(table(dat$Species.Name))
colnames(species_counts_df) <- c("Species", "Count") # Rename the columns for clarity
species_counts_df <- species_counts_df %>%
  arrange(desc(Count)) # Put in descending order.
print(species_counts_df) # Print the new data frame (use for plotting later).

# Save full list as csv for copying over numbers later.
#write.csv(species_counts_df, "Table_Strikes_by_Species_2024.csv", row.names = FALSE)

### Strikes by year

year_counts <- dat %>%
  group_by(Strike.Year) %>%
  summarize(Count = n(), .groups = "drop") 

# Plot counts by month.
ggplot(year_counts, aes(x = Strike.Year, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  labs(
    x = "Year",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  )

# Strikes by year and 10,000 operations
# Count strikes per year
strikes_per_year <- dat %>%
  group_by(Strike.Year) %>%
  summarise(Strikes = n())

# Rename column for merging
strikes_per_year <- strikes_per_year %>%
  rename(Year = Strike.Year)

# Merge with operations data
merged_data <- ops %>%
  left_join(strikes_per_year, by = "Year") %>%
  mutate(
    Strikes = replace_na(Strikes, 0),  # Handle years with no strikes
    Rate = (Strikes / Operations) * 10000  # Calculate rate per 10,000 operations
  )

# View merged data
print(merged_data)

# Plot the rate of strikes per 10,000 operations
ggplot(merged_data, aes(x = Year, y = Rate)) +
  geom_line(color = "firebrick4", size = 1.5) +
  geom_point(size = 4, color = "firebrick4") +
  labs(
    x = "Year",
    y = "Strikes per 10K operations"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  )



#



# Strikes by month


# Count rows for each Strike.Month
month_counts <- dat %>%
  group_by(Strike.Month) %>%
  summarize(Count = n(), .groups = "drop") 

# Plot counts by month.
ggplot(month_counts, aes(x = Strike.Month, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  labs(
    x = "Month",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  )


########################################
###### Strikes by Guild and Month ######
########################################

# Create summary table grouped by Month and Guild.
guild_month_df <- dat %>%
  group_by(Strike.Month, Guild) %>%
  summarize(Count = n(), .groups = "drop") # Count occurrences and ungroup.

# Facet-wrap plot.
guild.month <- ggplot(guild_month_df, aes(x = Strike.Month, y = Count)) +
  geom_bar(stat = "identity", fill = "violetred4") +
  facet_wrap(~ Guild, ncol = 2) +
  labs(
    #title = "Count by Month for Each Guild",
    x = "Month",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    # Axis text and titles
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
    # Add a border and light gray fill to the facet title box
    strip.background = element_rect(color = "black", fill = "lightgray", size = 1),
    strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
    # Add a border around each panel (excluding the facet title)
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    # Adjust spacing to avoid overlap between the strip and panel border
    panel.spacing = unit(0.2, "lines")
  )

# Print the plot
print(guild.month)

# Save plot.
#ggsave("Plot_Monthly_Strikes_Guild_2024.jpeg", plot = guild.month, width = 10, height = 10, dpi = 300)
