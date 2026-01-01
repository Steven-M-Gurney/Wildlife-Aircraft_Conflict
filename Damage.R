###############################################################################
# Annual Damaging-Strike Rates and Control Chart
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Summarize damaging wildlife strikes over time (FAA and WCAA)
#   • Merge damaging-strike counts with annual aircraft operations
#   • Adjust 2024 FAA counts using WCAA data (reporting lag)
#   • Calculate annual damaging-strike rates per 100,000 operations
#   • Produce a control-chart style plot with mean and ±2 SD limits
###############################################################################

# 📦 Load Required Packages & Data---------------------------------------------
library(tidyverse)
library(stringr)
library(tidyr)
library(ggplot2)

faa <- read.csv("faa_strikes_2016_2024_clean.csv")
dat <- read.csv("wcaa_strikes_2016_2024_clean.csv")
ops <- read.csv("ops_2016_2024.csv")



# =============================================================================
# 📁 1. FAA Damaging Strikes Over Time (Table)
# =============================================================================

# Filter FAA records with indicated damage
faa_damage <- faa %>%
  filter(INDICATED_DAMAGE == TRUE)   # if INDICATED_DAMAGE is logical
# If instead it's coded as "Yes"/"No", use:
# filter(INDICATED_DAMAGE == "Yes")

# Count damaging strikes per year
faa_damage_counts_df <- as.data.frame(table(faa_damage$INCIDENT_YEAR))
colnames(faa_damage_counts_df) <- c("Year", "FAA.Count")

# Inspect FAA damaging strikes
print(faa_damage_counts_df)


# =============================================================================
# 📁 2. WCAA Damaging Strikes Over Time (Table)
# =============================================================================

# Filter WCAA records where Damaging.Strike == "Yes"
damage <- dat %>%
  filter(Damaging.Strike == "Yes")

# Count damaging strikes per year
wcaa_damage_counts_df <- as.data.frame(table(damage$Strike.Year))
colnames(wcaa_damage_counts_df) <- c("Year", "WCAA.Count")

# Inspect WCAA damaging strikes
print(wcaa_damage_counts_df)


# =============================================================================
# 🔁 3. Combine WCAA, Operations, and FAA Data
# =============================================================================

# Ensure Year is integer type
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

ops <- ops %>%
  mutate(Year = as.integer(as.character(Year)))

# Ensure all years in ops appear in WCAA summary (fill missing with 0)
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  complete(Year = unique(ops$Year), fill = list(WCAA.Count = 0))

# Join in operations and keep relevant columns
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  left_join(ops, by = "Year") %>%
  select(Year, WCAA.Count, Operations)

print(wcaa_damage_counts_df)

# Ensure Year is integer in both FAA and WCAA summaries
wcaa_damage_counts_df <- wcaa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

faa_damage_counts_df <- faa_damage_counts_df %>%
  mutate(Year = as.integer(as.character(Year)))

# Get full set of years represented in either dataset
all_years <- unique(c(wcaa_damage_counts_df$Year, faa_damage_counts_df$Year))

# Make sure FAA table has rows for all years (missing filled as NA)
faa_damage_counts_df <- faa_damage_counts_df %>%
  complete(Year = all_years, fill = list(FAA.Count = NA))

# Join FAA counts into WCAA/operations table
damage_counts_df <- wcaa_damage_counts_df %>%
  left_join(faa_damage_counts_df %>% select(Year, FAA.Count), by = "Year")

# For 2024, substitute WCAA damaging-strike count for FAA (due to reporting lag)
damage_counts_df <- damage_counts_df %>%
  mutate(
    FAA.Count = if_else(Year == 2024, WCAA.Count, FAA.Count)
  ) %>%
  select(Year, Operations, WCAA.Count, FAA.Count)

print(damage_counts_df)

# Save combined table
write.csv(
  damage_counts_df,
  "Table_Annual_Damage_And_Operations.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 4. Calculate Annual Rates and Control Limits
# =============================================================================

summary_data <- damage_counts_df %>%
  group_by(Year) %>%
  summarise(
    total_count      = sum(FAA.Count, na.rm = TRUE),
    total_operations = sum(Operations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rate = (total_count / total_operations) * 100000   # per 100,000 ops
  )

# Control-chart statistics
CL  <- mean(summary_data$Rate, na.rm = TRUE)          # center line
SDr <- sd(summary_data$Rate, na.rm = TRUE)            # std dev of rates
UCL <- CL + 2 * SDr                                   # upper control limit
LCL <- max(CL - 2 * SDr, 0)                           # lower control limit (bounded at 0)

# Add control limits to data frame
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
  "Table_Annual_Damage_Rates.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 5. Plot Annual Damaging-Strike Rate (Control Chart)
# =============================================================================

damage_rate <- ggplot(summary_data,
                      aes(x = Year, y = Rate)) +
  geom_hline(aes(yintercept = CL),
             linetype = "solid",
             color    = "gray",
             linewidth = 1.5) +
  geom_hline(aes(yintercept = UCL),
             linetype = "dashed",
             color    = "gray",
             linewidth = 1.5) +
  geom_hline(aes(yintercept = LCL),
             linetype = "dashed",
             color    = "gray",
             linewidth = 1.5) +
  geom_line(color = "firebrick4", size = 2) +
  geom_point(color = "firebrick4", size = 5) +
  labs(
    x = "Year",
    y = "Damaging strikes\nper 100,000 operations"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.text.x  = element_text(size = 16, color = "black")
  )

# Preview plot
damage_rate

# Save plot
ggsave(
  "Plot_Annual_Damage_Rate.jpeg",
  plot   = damage_rate,
  width  = 8,
  height = 6,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################
