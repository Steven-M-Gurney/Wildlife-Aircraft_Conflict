###############################################################################
# Annual Pilot-Reporting Rate Summary
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Estimate the percentage of wildlife strikes reported by pilots each year
#   • Use FAA strike data for all years except 2024
#   • Use WCAA strike-report data to estimate 2024 values (due to FAA reporting lag)
#   • Standardize aircraft-registration fields across datasets
#   • Export a clean summary table and visualization for reporting
###############################################################################

# =============================================================================
# 📊 1. Preprocess FAA REG Field (Pilot-Reporting Flag)
# =============================================================================

# Convert REG to a logical pilot-reporting flag:
#   • FALSE = blank or NA (not pilot-reported)
#   • TRUE  = any non-blank text (aircraft registration present)
faa <- faa %>%
  mutate(
    REG = if_else(
      is.na(REG) | trimws(REG) == "",
      FALSE,
      TRUE
    )
  )


# =============================================================================
# 📊 2. FAA Pilot-Reporting Summary (All Years Except 2024)
# =============================================================================

result_faa <- faa %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(
    strike_total  = n(),              # total strikes per year
    total_pilot   = sum(REG),         # count of pilot-reported strikes
    percent_pilot = mean(REG) * 100,  # % pilot-reported
    .groups = "drop"
  ) %>%
  filter(INCIDENT_YEAR != 2024)       # exclude 2024 (use WCAA instead)


# =============================================================================
# 📁 3. Prepare WCAA REG Field for Pilot-Reporting (2024)
# =============================================================================

# Rename Aircraft.Registration to match FAA naming convention (REG)
dat <- dat %>%
  rename(REG = Aircraft.Registration)

# Inspect raw REG values (blanks, whitespace, unknowns, coordinates, etc.)
print(unique(dat$REG))

# Standardize REG values:
#   • Trim whitespace
#   • Convert blanks + "unknown" variants to FALSE
#   • Treat all other values as TRUE (registration present)
dat <- dat %>%
  mutate(REG = trimws(REG)) %>%
  mutate(
    REG = case_when(
      REG %in% c("", "UNKNOWN", "Unknown", "unknown") ~ FALSE,
      TRUE ~ TRUE
    )
  )

# Check cleaned REG values
print(unique(dat$REG))


# =============================================================================
# 📊 4. WCAA Pilot-Reporting Summary for 2024
# =============================================================================

result_dat <- dat %>%
  filter(Strike.Year == 2024) %>%
  summarise(
    INCIDENT_YEAR = 2024,
    strike_total  = n(),
    total_pilot   = sum(REG),
    percent_pilot = mean(REG) * 100,
    .groups = "drop"
  )


# =============================================================================
# 🔁 5. Combine FAA (≤2023) + WCAA (2024) Pilot-Reporting Results
# =============================================================================

result <- bind_rows(result_faa, result_dat)

# Inspect combined results
print(result)

# Save table
write.csv(
  result,
  "Table_Annual_Pilot_Reporting.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 6. Plot Annual Pilot-Reporting Percentage
# =============================================================================

pr <- ggplot(result,
             aes(x = INCIDENT_YEAR,
                 y = percent_pilot)) +
  geom_line(color = "skyblue4", size = 2) +
  geom_point(color = "skyblue4", size = 5) +
  labs(
    x = "Year",
    y = "Airline-reported strikes (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.text.x  = element_text(size = 16, color = "black")
  )

# Preview plot
pr

# Save plot
ggsave(
  "Plot_Annual_Pilot_Percent.jpeg",
  plot   = pr,
  width  = 8,
  height = 6,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################






















############################################
###### Pilot-Reporting Rate Over Time ######
############################################

# Preprocess the REG column: Convert blanks and NAs to FALSE, everything else to TRUE
faa <- faa %>%
  mutate(REG = ifelse(is.na(REG) | trimws(REG) == "", FALSE, TRUE))

# Summarize data from faa for all years except 2024
result_faa <- faa %>%
  group_by(INCIDENT_YEAR) %>%
  summarize(
    strike_total = n(),
    total_pilot = sum(REG),  # Count TRUE values in REG
    percent_pilot = (mean(REG)) * 100,  # Proportion of TRUE values in REG
    .groups = "drop"
  ) %>%
  filter(INCIDENT_YEAR != 2024) # Exclude 2024 from the faa data

# Since there is a lag with FAA data reporting, we need to adjust 2024.
# Use data from WCAA in this step (first clean it up).

# Rename column to match FAA.
dat <- dat %>%
  rename(REG = Aircraft.Registration)

# Look at what is in the REG column.
print(unique(dat$REG)) # Note blanks, white space, coordinates, and variants of "unknown".

# Go into spreadsheet and manually fix any errors identified.
#write.csv(dat, "R_WCAA_Strikes_29Jan2025.csv", row.names = FALSE)

####### STILL NEED TO FIX 3 ERRORS ##############

# Trim white space and change blanks and all variants of unknown to False.
# All other text changed to TRUE (to capture plane registrations).
dat <- dat %>%
  mutate(REG = trimws(REG)) %>%  # Remove leading/trailing spaces
  mutate(REG = case_when(
    REG %in% c("", "UNKNOWN", "Unknown", "unknown") ~ FALSE,  
    TRUE ~ TRUE  
  ))
  
# Look at what is in the REG column.
print(unique(dat$REG))

# Summarize data for 2024.
result_dat <- dat %>%
  filter(Strike.Year == 2024) %>%
  summarize(
    INCIDENT_YEAR = 2024,
    strike_total = n(),
    total_pilot = sum(REG),
    percent_pilot = (mean(REG)) * 100,
    .groups = "drop"
  )

# Combine the two dataframes.
result <- bind_rows(result_faa, result_dat)

# Print the combined result.
print(result)

# Save table.
write.csv(result, "Table_Annual_Pilot_Reporting.csv", row.names = FALSE)


# Create the line chart.
pr <- ggplot(result, aes(x = INCIDENT_YEAR, y = percent_pilot)) +
  geom_line(color = "skyblue4", size = 2) +
  geom_point(color = "skyblue4", size = 5) +
  labs(
    x = "Year",
    y = "Airline-reported strikes (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y =element_text(size = 16, color = "black"),
    axis.text.x =element_text(size = 16, color = "black"))

# Save plot.
ggsave("Plot_Annual_Pilot_Percent.jpeg", plot = pr, width = 8, height = 6, dpi = 300)

