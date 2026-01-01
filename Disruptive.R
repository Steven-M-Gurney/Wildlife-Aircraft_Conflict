###############################################################################
# Annual Disruptive-Strike Rates and Control Chart
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Import FAA wildlife-strike data (2016–2024)
#   • Import WCAA aircraft-operations data (2016–2024)
#   • Identify disruptive wildlife strikes using FAA fields (damage, effect, cost)
#   • Summarize disruptive events by year and merge with annual operations
#   • Calculate disruptive-strike rates per 100,000 operations
#   • Produce a control-chart style plot with mean and ±2 SD limits
###############################################################################

# 📦 Load Required Packages & Data----------------------------------------------
library(tidyverse)
library(stringr)

faa <- read.csv("faa_strikes_2016_2024_clean.csv")
dat <- read.csv("wcaa_strikes_2016_2024_clean.csv")
ops <- read.csv("ops_2016_2024.csv")


# =============================================================================
# 📁 1. Inspect FAA Fields Relevant to Disruptive Events
# =============================================================================

print(unique(faa$INDICATED_DAMAGE))
print(unique(faa$EFFECT))
print(unique(faa$EFFECT_OTHER))
print(unique(faa$COST_REPAIRS))
print(unique(faa$AOS))
print(unique(faa$COST_OTHER))


# =============================================================================
# 🧹 2. Prepare Disruptive-Event Indicators
# =============================================================================

# Normalize EFFECT (convert " None" to truly blank text first)
faa <- faa %>%
  mutate(
    EFFECT = dplyr::if_else(
      EFFECT == " None",
      "",
      as.character(EFFECT)
    )
  )

# Convert relevant fields to logical TRUE/FALSE flags
faa <- faa %>%
  mutate(
    EFFECT       = !is.na(EFFECT)       & EFFECT       != "",
    EFFECT_OTHER = !is.na(EFFECT_OTHER) & EFFECT_OTHER != "",
    COST_REPAIRS = !is.na(COST_REPAIRS),
    AOS          = !is.na(AOS),
    COST_OTHER   = !is.na(COST_OTHER)
  )

# Gut check
print(colSums(
  faa[, c("INDICATED_DAMAGE",
          "EFFECT",
          "EFFECT_OTHER",
          "COST_REPAIRS",
          "AOS",
          "COST_OTHER")],
  na.rm = TRUE
))

# Derive disruptive flag if ANY indicator = TRUE
faa <- faa %>%
  mutate(
    disruptive = rowSums(
      dplyr::across(
        c(INDICATED_DAMAGE,
          EFFECT,
          EFFECT_OTHER,
          COST_REPAIRS,
          AOS,
          COST_OTHER),
        ~ as.numeric(.)
      )
    ) > 0
  )

# Filter dataset to disruptive strikes only
faa_disruptive <- faa %>%
  filter(disruptive)


# =============================================================================
# 📊 3. Disruptive Events Over Time (Table)
# =============================================================================

# Count disruptive strikes by year
faa_disruptive_counts_df <- as.data.frame(
  table(faa_disruptive$INCIDENT_YEAR)
)

colnames(faa_disruptive_counts_df) <- c("Year", "FAA.Count")

print(faa_disruptive_counts_df)

# Ensure Year matches type in ops table
faa_disruptive_counts_df <- faa_disruptive_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

ops <- ops %>%
  mutate(Year = as.integer(as.character(Year)))

# Join operations to disruptive counts
faa_disruptive_counts_df <- faa_disruptive_counts_df %>%
  left_join(ops, by = "Year") %>%
  select(Year, FAA.Count, Operations)

print(faa_disruptive_counts_df)

# Save table for reporting
write.csv(
  faa_disruptive_counts_df,
  "Table_Annual_Disruptive_And_Operations.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 4. Calculate Annual Disruptive-Strike Rates and Control Limits
# =============================================================================

summary_data <- faa_disruptive_counts_df %>%
  group_by(Year) %>%
  summarise(
    total_count      = sum(FAA.Count, na.rm = TRUE),
    total_operations = sum(Operations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rate = (total_count / total_operations) * 100000
  )

# Control chart statistics
CL  <- mean(summary_data$Rate, na.rm = TRUE)
SDr <- sd(summary_data$Rate,  na.rm = TRUE)
UCL <- CL + 2 * SDr
LCL <- max(CL - 2 * SDr, 0)

summary_data <- summary_data %>%
  mutate(
    CL  = CL,
    UCL = UCL,
    LCL = LCL
  )

print(summary_data)

# Save rate table
write.csv(
  summary_data,
  "Table_Annual_Disruptive_Rates.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 5. Plot Annual Disruptive-Strike Rate (Control Chart)
# =============================================================================

disruptive_rate <- ggplot(summary_data,
                          aes(x = Year, y = Rate)) +
  geom_hline(aes(yintercept = CL),
             linetype  = "solid",
             color     = "gray",
             linewidth = 1.5) +
  geom_hline(aes(yintercept = UCL),
             linetype  = "dashed",
             color     = "gray",
             linewidth = 1.5) +
  geom_hline(aes(yintercept = LCL),
             linetype  = "dashed",
             color     = "gray",
             linewidth = 1.5) +
  geom_line(color = "firebrick4", size = 2) +
  geom_point(color = "firebrick4", size = 5) +
  labs(
    x = "Year",
    y = "Disruptive strikes\nper 100,000 operations"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.text.x  = element_text(size = 16, color = "black")
  )

# Preview plot
disruptive_rate

# Save output figure
ggsave(
  "Plot_Annual_Disruptive_Rate.jpeg",
  plot   = disruptive_rate,
  width  = 8,
  height = 6,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################
