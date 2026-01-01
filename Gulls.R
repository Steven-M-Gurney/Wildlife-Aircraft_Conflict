###############################################################################
# Gull Strike Events by Day of Week
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Import and preprocess FAA wildlife-strike data (2016–2024)
#   • Standardize species names and assign guilds
#   • Identify gull/tern strikes
#   • Summarize strike counts by day of the week
#   • Plot weekday strike counts with mean and control limit
###############################################################################

# 📦 Load Required Packages & Data----------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

faa <- read.csv("faa_strikes_2016_2024_clean.csv")


# =============================================================================
# 🧹 1. Clean & Standardize Species Names
# =============================================================================

# Inspect original species field
print(unique(faa$SPECIES))

# Ensure species names are in sentence case and trimmed
faa <- faa %>%
  mutate(
    SPECIES = str_to_sentence(SPECIES),
    SPECIES = str_trim(SPECIES)
  )

# Aggregate gull variants
faa <- faa %>%
  mutate(
    SPECIES = if_else(
      str_detect(SPECIES, "Gulls") | str_detect(SPECIES, "gull"),
      "Gull sp.",
      SPECIES
    )
  )

# Aggregate bat variants
faa <- faa %>%
  mutate(
    SPECIES = if_else(
      str_detect(SPECIES, "Bats") | str_detect(SPECIES, "bat"),
      "Bat sp.",
      SPECIES
    )
  )

# Clean up inconsistent entries
faa <- faa %>%
  mutate(
    SPECIES = case_when(
      SPECIES == "Perching birds (y)"                  ~ "Unknown bird",
      SPECIES == "Mallard/american black duck complex" ~ "Waterfowl sp.",
      SPECIES == "Swallows"                            ~ "Swallow sp.",
      SPECIES == "Shorebirds"                          ~ "Shorebird sp.",
      SPECIES == "Eastern cottontail"                  ~ "Eastern cottontail rabbit",
      SPECIES == "American barn owl"                   ~ "Barn owl",
      SPECIES == "Redpoll"                             ~ "Common redpoll",
      SPECIES == "Turtles"                             ~ "Turtle sp.",
      TRUE                                             ~ SPECIES
    )
  )

# Inspect species after cleanup
print(unique(faa$SPECIES))


# =============================================================================
# 🪶 2. Join Guild Assignments & Filter for Gulls/Terns
# =============================================================================

guild_table <- read.csv("Guild_Assignments_12Feb2025.csv")

guild_table <- guild_table %>%
  mutate(Species = str_to_sentence(Species))

guilded <- faa %>%
  left_join(guild_table, by = c("SPECIES" = "Species"))

# Check any species that did not match a guild
guilded %>%
  filter(is.na(Guild)) %>%
  distinct(SPECIES) %>%
  pull(SPECIES) %>%
  print()

# Filter to gulls/terns
gull <- guilded %>%
  filter(Guild == "Gulls/Terns")


# =============================================================================
# 📆 3. Summarize Gull Strikes by Day of Week
# =============================================================================

gull <- gull %>%
  mutate(DAY = weekdays(as.Date(INCIDENT_DATE)))

day_order <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

day_counts <- gull %>%
  group_by(DAY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(DAY = day_order, fill = list(Count = 0))

day_stats <- day_counts %>%
  summarise(
    Avg_Count = mean(Count),
    SD        = sd(Count),
    SE        = SD / sqrt(n()),
    UCL       = Avg_Count + 1.96 * SD,
    LCL       = pmax(Avg_Count - 1.96 * SD, 0)
  )

print(day_counts)
print(day_stats)


# =============================================================================
# 📊 4. Plot Gull Strike Events by Day of Week
# =============================================================================

gull_plot <- ggplot(day_counts,
                    aes(x = factor(DAY, levels = day_order),
                        y = Count)) +
  geom_hline(yintercept = day_stats$Avg_Count,
             linetype  = "solid",
             color     = "gray",
             linewidth = 1.5) +
  geom_hline(yintercept = day_stats$UCL,
             linetype  = "dashed",
             color     = "gray",
             linewidth = 1.5) +
  geom_col(fill = "darkseagreen4") +
  labs(
    x = "Day of the week",
    y = "Gull strike events"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.text.x  = element_text(size = 16, color = "black",
                                angle = 45, hjust = 1)
  )

print(gull_plot)

ggsave(
  "Plot_GullStrikes.jpeg",
  plot   = gull_plot,
  width  = 8,
  height = 6,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################
