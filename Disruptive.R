###############################################################################
# Annual Disruptive-Strike Rates and Control Chart
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Identify disruptive wildlife strikes using FAA fields (damage, effect, cost)
#   • Summarize disruptive events by year and merge with annual operations
#   • Calculate disruptive-strike rates per 100,000 operations
#   • Produce a control-chart style plot with mean and ±2 SD limits
###############################################################################

# 📦 Assumes these are already loaded earlier in the script --------------------
# library(tidyverse)
# library(stringr)


# =============================================================================
# 📁 1. Inspect FAA Fields Relevant to Disruptive Events
# =============================================================================

# Quick checks (note whitespace and NA patterns)
print(unique(faa$INDICATED_DAMAGE))  # TRUE / FALSE (or Yes/No depending on export)
print(unique(faa$EFFECT))            # Includes " None" and blanks
print(unique(faa$EFFECT_OTHER))      # Includes blanks
print(unique(faa$COST_REPAIRS))      # Includes NAs
print(unique(faa$AOS))               # Includes NAs
print(unique(faa$COST_OTHER))        # Includes NAs


# =============================================================================
# 🧹 2. Prepare Disruptive-Event Indicators
# =============================================================================

# Normalize EFFECT: treat " None" as blank, keep everything as character first
faa <- faa %>%
  mutate(
    EFFECT = dplyr::if_else(EFFECT == " None",
                            "",
                            as.character(EFFECT))
  )

# Convert key fields to logical "has info / no info" flags
#   • EFFECT / EFFECT_OTHER: TRUE if non-blank, FALSE if blank/NA
#   • COST_REPAIRS / AOS / COST_OTHER: TRUE if not NA, FALSE if NA
faa <- faa %>%
  mutate(
    EFFECT       = !is.na(EFFECT)       & EFFECT       != "",
    EFFECT_OTHER = !is.na(EFFECT_OTHER) & EFFECT_OTHER != "",
    COST_REPAIRS = !is.na(COST_REPAIRS),
    AOS          = !is.na(AOS),
    COST_OTHER   = !is.na(COST_OTHER)
  )

# Gut check: number of TRUEs by column
print(colSums(faa[, c("INDICATED_DAMAGE",
                      "EFFECT",
                      "EFFECT_OTHER",
                      "COST_REPAIRS",
                      "AOS",
                      "COST_OTHER")],
              na.rm = TRUE))

# Derive "disruptive" flag:
# If any of these fields are TRUE in a row, call the strike disruptive.
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

# Keep only disruptive strikes (logical filter, not "TRUE" string)
faa_disruptive <- faa %>%
  filter(disruptive)


# =============================================================================
# 📊 3. Disruptive Events Over Time (Table)
# =============================================================================

# Count disruptive strikes per year
faa_disruptive_counts_df <- as.data.frame(table(faa_disruptive$INCIDENT_YEAR))
colnames(faa_disruptive_counts_df) <- c("Year", "FAA.Count")

# Inspect disruptive-strike counts
print(faa_disruptive_counts_df)

# Ensure Year types are consistent before join
faa_disruptive_counts_df <- faa_disruptive_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

ops <- ops %>%
  mutate(Year = as.integer(as.character(Year)))

# Join operations to disruptive-strike counts
faa_disruptive_counts_df <- faa_disruptive_counts_df %>%
  left_join(ops, by = "Year") %>%
  select(Year, FAA.Count, Operations)

print(faa_disruptive_counts_df)

# Save table
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
    Rate = (total_count / total_operations) * 100000  # per 100,000 ops
  )

# Control-chart statistics
CL  <- mean(summary_data$Rate, na.rm = TRUE)   # center line
SDr <- sd(summary_data$Rate, na.rm = TRUE)     # std dev of rates
UCL <- CL + 2 * SDr                            # upper control limit
LCL <- max(CL - 2 * SDr, 0)                    # lower control limit (bounded at 0)

summary_data <- summary_data %>%
  mutate(
    CL  = CL,
    UCL = UCL,
    LCL = LCL
  )

print(summary_data)

# Save rates table
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

# Take a look
disruptive_rate

# Save plot
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
