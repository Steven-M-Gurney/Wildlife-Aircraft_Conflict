#######################################
####### 2024 DTW Strike Summary #######
############## 09Jan2025 ##############
########## Steven M. Gurney ###########
#######################################

# The purpose of this code is to clean, wrangle, and summarize DTW's 2024
# strike data. WCAA data was used for these analyses. Some records may be 
# pending species ID or other information.
# Additional strike information can be found on the FAA database, including cost
# and damage code, but there is a 3-month lag with updates.

# Load packages.
library(tidyverse)
library(stringr)

# Read in data exported from WCAA Wildlife Command Center.
# Note that these data may be pending information and may be later updated in the database.. 
dat <- read.csv("WCAA_Export_2024_Strikes_07Jan2025.csv")

# Look at column names for reference.
print(colnames(dat))

# How many Unique IDs?
length(unique(dat$Unique.ID))

# How many FAA database numbers?
length(unique(dat$FAA.Database.Number))

# How many records set to revise?
sum(dat$Course.of.Action == "Revise", na.rm = TRUE)

############################
###### DATA CLEANING #######
############################

# Filter out archived records (i.e., records not submitted to FAA).
# Note that some Course.of.Action entries may be blank or Revise,
# assume these are Submit to FAA records.
dat <- dat %>%
  filter(Course.of.Action. != "Archive (do not submit to FAA)")

# How many occurances when multiple species were struck?
dat %>%
  filter(Were.Multiple.Species.Struck. == "Yes") %>%
  summarise(count = n())

# Convert Species.Name column to sentence case.
dat$Species.Name<- str_to_sentence(dat$Species.Name)

# Aggregate Unknown Bird variants (Species).
# Rename rows with both "UNKNOWN" and "BIRD" to "UNKNOWN BIRD".
dat <- dat %>%
  mutate(Species.Name = if_else(
    str_detect(toupper(Species.Name), "UNKNOWN") & str_detect(toupper(Species.Name), "BIRD"),
    "Unknown bird",
    Species.Name
  ))

# Add UNKNOWN labels for blank GUILD (n = 6) and blank Species.Name. (n = 6).
# These were looked into and were occurrences when no biological samples were available.
# Replace blanks and NA values as specified.
dat <- dat %>%
  mutate(
    Guild = if_else(Guild == "" | is.na(Guild), "Unknown", Guild),
    Species.Name = if_else(Species.Name == "" | is.na(Species.Name), "Unknown bird", Species.Name)
  )

# Fix month format.
# Remove the month number and dash, keeping only the month abbreviation.
dat$Strike.Month <- str_replace(dat$Strike.Month, "^\\d+-", "")

# Ensure Jan-Dec order.
dat <- dat %>%
  mutate(Strike.Month = factor(Strike.Month, levels = month.abb)) 

# Fix year format and remove commas from the Year column.
dat$Strike.Year <- str_replace_all(dat$Strike.Year, ",", "")
dat$Strike.Year <- as.numeric(dat$Strike.Year)

# Modify the Runway.Taxiway column to combine names or list as Other.
dat <- dat %>%
  mutate(Runway.Taxiway = case_when(
    Runway.Taxiway %in% c("22R", "4L") ~ "22R-4L",
    Runway.Taxiway %in% c("22L", "4R") ~ "22L-4R",
    Runway.Taxiway %in% c("21R", "3L") ~ "21R-3L",
    Runway.Taxiway %in% c("21L", "3R") ~ "21L-3R",
    Runway.Taxiway %in% c("27R", "9L") ~ "27R-9L",
    Runway.Taxiway %in% c("27L", "9R") ~ "27L-9R",
    TRUE ~ "Other"
  ))

# Look at the list of unique runway entries.
print(unique(dat$Runway.Taxiway))

#############################
###### DATA WRANGLING #######
#############################

# Select columns of interest.
dat <- dat %>% select(Unique.ID, FAA.Database.Number, 
                      Strike.Year, Strike.Month,
                      Guild, Species.Name, Total.Struck,
                      Runway.Taxiway,
                      # Ingredients for Disruptive Event selected below. 
                      # Note, disruptive event defined as "strike records that indicated repair costs,
                      # non-repair (other) costs, damage, a negative effect on the flight, aircraft 
                      # downtime, or any combination of these." (Altringer et al. 2024).
                      # Also note that cost (and damage code) will need to come from the FAA database.
                      Damaging.Strike,
                      Effect.On.Flight,
                      Other.Effect,
                      Estimated.Cost.of.Repairs...., 
                      Aircraft.Time.Out.of.Service..hrs.,
                      Other.Costs....,
                      Course.of.Action.) # Helps derive number of pending IDs.


#############################
##### Strikes by Runway #####
#############################

# Count the number of occurrences of each unique value in the  column.
runway_counts_df <- as.data.frame(table(dat$Runway.Taxiway))
colnames(runway_counts_df) <- c("Runway", "Count") # Rename the columns for clarity

# Resort in descending order.
runway_counts_df <- runway_counts_df %>%
  arrange(desc(Count))

# Save full list as csv for copying over numbers later.
write.csv(runway_counts_df, "Table_Strikes_by_Runway_2024.csv", row.names = FALSE)

# Plot strike count by runway.
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

# Save plot.
ggsave("Plot_Runway_Strikes_2024.jpeg", plot = runway_counts_df, width = 10, height = 10, dpi = 300)


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
write.csv(guild_counts_df, "Table_Strikes_by_Guild_2024.csv", row.names = FALSE)

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

# Save plot.
ggsave("Plot_Annual_Strikes_Guild_2024.jpeg", plot = guild.annual, width = 10, height = 10, dpi = 300)

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
write.csv(species_counts_df, "Table_Strikes_by_Species_2024.csv", row.names = FALSE)


##############################
###### Strikes by Month ######
##############################

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

# Save plot.
ggsave("Plot_Monthly_Strikes_2024.jpeg", plot = month.counts, width = 10, height = 10, dpi = 300)


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
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    # Adjust spacing to avoid overlap between the strip and panel border
    panel.spacing = unit(0.2, "lines")
  )

# Print the plot
print(guild.month)

# Save plot.
ggsave("Plot_Monthly_Strikes_Guild_2024.jpeg", plot = guild.month, width = 10, height = 10, dpi = 300)

##########################################
###### Strikes by Species and Month ######
##########################################

# Create summary table grouped by Strike.Month and Species.
species_month_df <- dat %>%
  group_by(Strike.Month, Species.Name) %>%
  summarize(Count = n(), .groups = "drop") # Count occurrences and ungroup.
print(species_month_df)

# Filter data to only include if species had 3 or more strikes (arbitrary #, will need to revise for larger datasets).
species_month_df <- species_month_df %>%
  group_by(Species.Name) %>%                 # Group by Species.Name
  filter(sum(Count) >= 3) %>%               # Keep only groups where sum of Count is >= 3
  ungroup()                                 # Ungroup the data

# Abbreviate long species name here
dat <- dat %>%
  mutate(Species.Name = str_replace(Species.Name, 
                                    "Northern rough-winged swallow", 
                                    "N. rough-winged swallow"))

# Facet-wrap plot.
ggplot(species_month_df, aes(x = Strike.Month, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Species.Name, ncol = 4) +
  labs(
    x = "Month",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    # Axis text and titles
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
    # Add a border and light gray fill to the facet title box
    strip.background = element_rect(color = "black", fill = "lightgray", size = 1),
    strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
    # Add a border around each panel (excluding the facet title)
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    # Adjust spacing to avoid overlap between the strip and panel border
    panel.spacing = unit(0.2, "lines")
  )




