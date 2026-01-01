###############################################################################
# 2024 DTW Strike Summary
# Author: Steven M. Gurney
# Last updated: 01 JAN 2025
#
# Purpose:
#   • Clean, wrangle, and summarize DTW 2024 strike data
#   • Use WCAA Wildlife Command Center data as the primary 2024 source
#   • Generate summary tables by runway, guild, species, and month
#   • Produce publication-ready plots for reporting and internal summaries
#   • Flag where FAA database (cost/damage) is needed for follow-up analyses
###############################################################################

# 📦 Load Required Packages & Data---------------------------------------------
library(tidyverse)
library(stringr)
library(ggplot2)

faa <- read.csv("faa_strikes_2016_2024_clean.csv")
dat <- read.csv("wcaa_strikes_2016_2024_clean.csv")



# =============================================================================
# 📁 1. Import and Quick QA Checks
# =============================================================================

# Read in data exported from WCAA Wildlife Command Center.
# Note: these data may contain pending information that can be updated later.
dat <- read.csv("WCAA_Export_2024_Strikes_07Jan2025.csv")

# Look at column names for reference.
print(colnames(dat))

# How many Unique IDs?
length(unique(dat$Unique.ID))

# How many FAA database numbers?
length(unique(dat$FAA.Database.Number))

# How many records set to "Revise"?
sum(dat$Course.of.Action. == "Revise", na.rm = TRUE)


# =============================================================================
# 🧹 2. Data Cleaning
# =============================================================================

# Filter out archived records (i.e., not submitted to FAA).
# Some Course.of.Action. entries may be blank or "Revise";
# assume these are "Submit to FAA" (likely user error).
dat <- dat %>%
  filter(Course.of.Action. != "Archive (do not submit to FAA)")

# How many occurrences where multiple species were struck?
dat %>%
  filter(Were.Multiple.Species.Struck. == "Yes") %>%
  summarise(count = n())

# Convert species names to sentence case.
dat$Species.Name <- str_to_sentence(dat$Species.Name)

# Aggregate "Unknown bird" variants (Species).
# Any species containing both "UNKNOWN" and "BIRD" → "Unknown bird".
dat <- dat %>%
  mutate(
    Species.Name = if_else(
      str_detect(toupper(Species.Name), "UNKNOWN") &
        str_detect(toupper(Species.Name), "BIRD"),
      "Unknown bird",
      Species.Name
    )
  )

# Add "Unknown" labels for blank/NA Guild and Species.Name.
# These were occurrences when no biological samples were available.
dat <- dat %>%
  mutate(
    Guild = if_else(Guild == "" | is.na(Guild), "Unknown", Guild),
    Species.Name = if_else(Species.Name == "" | is.na(Species.Name),
                           "Unknown bird", Species.Name)
  )

# Fix month format: remove leading number + dash, keep only month abbrev.
dat$Strike.Month <- str_replace(dat$Strike.Month, "^\\d+-", "")

# Ensure Jan–Dec order.
dat <- dat %>%
  mutate(Strike.Month = factor(Strike.Month, levels = month.abb))

# Fix year format: remove commas and convert to numeric.
dat$Strike.Year <- str_replace_all(dat$Strike.Year, ",", "")
dat$Strike.Year <- as.numeric(dat$Strike.Year)

# Combine paired runways; all others labeled "Other".
dat <- dat %>%
  mutate(
    Runway.Taxiway = case_when(
      Runway.Taxiway %in% c("22R", "4L") ~ "22R-4L",
      Runway.Taxiway %in% c("22L", "4R") ~ "22L-4R",
      Runway.Taxiway %in% c("21R", "3L") ~ "21R-3L",
      Runway.Taxiway %in% c("21L", "3R") ~ "21L-3R",
      Runway.Taxiway %in% c("27R", "9L") ~ "27R-9L",
      Runway.Taxiway %in% c("27L", "9R") ~ "27L-9R",
      TRUE ~ "Other"
    )
  )

# Inspect runway groupings.
print(unique(dat$Runway.Taxiway))


# =============================================================================
# 🔁 3. Data Wrangling: Select Columns of Interest
# =============================================================================

dat <- dat %>%
  select(
    Unique.ID,
    FAA.Database.Number,
    Strike.Year,
    Strike.Month,
    Guild,
    Species.Name,
    Total.Struck,
    Runway.Taxiway,
    # Disruptive Event ingredients (Altringer et al. 2024):
    # strike records indicating repair costs, other costs, damage,
    # negative effect on flight, downtime, or any combination.
    # Cost and damage code will ultimately be pulled from FAA database.
    Damaging.Strike,
    Effect.On.Flight,
    Other.Effect,
    Estimated.Cost.of.Repairs....,
    Aircraft.Time.Out.of.Service..hrs.,
    Other.Costs....,
    Course.of.Action.
  )


# =============================================================================
# 📊 4. Strikes by Runway
# =============================================================================

# Count strikes per runway.
runway_counts_df <- as.data.frame(table(dat$Runway.Taxiway))
colnames(runway_counts_df) <- c("Runway", "Count")

# Sort descending.
runway_counts_df <- runway_counts_df %>%
  arrange(desc(Count))

# Save full table for reference.
write.csv(
  runway_counts_df,
  "Table_Strikes_by_Runway_2024.csv",
  row.names = FALSE
)

# Build runway plot.
runway_plot <- ggplot(runway_counts_df,
                      aes(x = reorder(Runway, Count),
                          y = Count)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  labs(
    x = "Runway",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.text.x  = element_text(size = 12, color = "black")
  ) +
  coord_flip()


# Preview plot
runway_plot

# Save runway plot.
ggsave(
  "Plot_Runway_Strikes_2024.jpeg",
  plot   = runway_plot,
  width  = 10,
  height = 10,
  dpi    = 300
)


# =============================================================================
# 📊 5. Strikes by Guild
# =============================================================================

# Count strikes per guild.
guild_counts_df <- as.data.frame(table(dat$Guild))
colnames(guild_counts_df) <- c("Guild", "Count")

# Sort descending.
guild_counts_df <- guild_counts_df %>%
  arrange(desc(Count))

# Save full table for reference.
write.csv(
  guild_counts_df,
  "Table_Strikes_by_Guild_2024.csv",
  row.names = FALSE
)

# Build guild plot.
guild_annual <- ggplot(guild_counts_df,
                       aes(x = reorder(Guild, Count),
                           y = Count)) +
  geom_bar(stat = "identity", fill = "palegreen4") +
  labs(
    x = "Guild",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.text.x  = element_text(size = 12, color = "black")
  ) +
  coord_flip()


# Preview plot
guild_annual

# Save guild plot.
ggsave(
  "Plot_Annual_Strikes_Guild_2024.jpeg",
  plot   = guild_annual,
  width  = 10,
  height = 10,
  dpi    = 300
)


# =============================================================================
# 📊 6. Strikes by Species
# =============================================================================

# Count strikes per species.
species_counts_df <- as.data.frame(table(dat$Species.Name))
colnames(species_counts_df) <- c("Species", "Count")

# Sort descending.
species_counts_df <- species_counts_df %>%
  arrange(desc(Count))

# Print for quick inspection.
print(species_counts_df)

# Save full table for reference.
write.csv(
  species_counts_df,
  "Table_Strikes_by_Species_2024.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 7. Strikes by Month
# =============================================================================

# Count strikes per month.
month_counts <- dat %>%
  group_by(Strike.Month) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

# Build monthly plot.
month_plot <- ggplot(month_counts,
                     aes(x = Strike.Month,
                         y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  labs(
    x = "Month",
    y = "Strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.text.x  = element_text(size = 12, color = "black")
  )

# Preview plot
month_plot

# Save monthly plot.
ggsave(
  "Plot_Monthly_Strikes_2024.jpeg",
  plot   = month_plot,
  width  = 10,
  height = 10,
  dpi    = 300
)


# =============================================================================
# 📊 8. Strikes by Guild and Month (Faceted)
# =============================================================================

# Summarize by month × guild.
guild_month_df <- dat %>%
  group_by(Strike.Month, Guild) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

# Build guild × month faceted plot.
guild_month <- ggplot(guild_month_df,
                      aes(x = Strike.Month,
                          y = Count)) +
  geom_bar(stat = "identity", fill = "violetred4") +
  facet_wrap(~ Guild, ncol = 2) +
  labs(
    x = "Month",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    axis.title.y     = element_text(face = "bold", size = 20),
    axis.title.x     = element_text(face = "bold", size = 20),
    axis.text.y      = element_text(size = 12, color = "black"),
    axis.text.x      = element_text(size = 12, color = "black", angle = 45, hjust = 1),
    strip.background = element_rect(color = "black", fill = "lightgray", size = 1),
    strip.text       = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing    = unit(0.2, "lines")
  )

# Print the plot to graphics device.
print(guild_month)

# Save guild × month plot.
ggsave(
  "Plot_Monthly_Strikes_Guild_2024.jpeg",
  plot   = guild_month,
  width  = 10,
  height = 10,
  dpi    = 300
)


# =============================================================================
# 📊 9. Strikes by Species and Month (Faceted, Common Species Only)
# =============================================================================

# Summarize by month × species.
species_month_df <- dat %>%
  group_by(Strike.Month, Species.Name) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

print(species_month_df)

# Keep only species with ≥ 3 total strikes (across all months).
species_month_df <- species_month_df %>%
  group_by(Species.Name) %>%
  filter(sum(Count) >= 3) %>%
  ungroup()

# Abbreviate long species name in the plotting data.
species_month_df <- species_month_df %>%
  mutate(
    Species.Name = str_replace(
      Species.Name,
      "Northern rough-winged swallow",
      "N. rough-winged swallow"
    )
  )

# Build species × month faceted plot.
species_month_plot <- ggplot(species_month_df,
                             aes(x = Strike.Month,
                                 y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Species.Name, ncol = 4) +
  labs(
    x = "Month",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    axis.title.y     = element_text(face = "bold", size = 20),
    axis.title.x     = element_text(face = "bold", size = 20),
    axis.text.y      = element_text(size = 10, color = "black"),
    axis.text.x      = element_text(size = 10, color = "black", angle = 45, hjust = 1),
    strip.background = element_rect(color = "black", fill = "lightgray", size = 1),
    strip.text       = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing    = unit(0.2, "lines")
  )

# Print plot
species_month_plot

# (Optional) Save species × month plot.
ggsave(
  "Plot_Monthly_Strikes_Species_2024.jpeg",
  plot   = species_month_plot,
  width  = 12,
  height = 10,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################

