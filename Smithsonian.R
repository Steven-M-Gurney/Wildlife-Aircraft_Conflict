###############################################################################
# Annual Smithsonian Submission Summary
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Summarize annual bird-strike remains sent to the Smithsonian
#   • Use FAA data for all years except 2024
#   • Use WCAA strike-report data to estimate 2024 values
#   • Export a clean summary table and visualization for reporting
###############################################################################

# 📦 Load Required Packages & Data---------------------------------------------
library(dplyr)
library(ggplot2)

faa <- read.csv("faa_strikes_2016_2024_clean.csv")
dat <- read.csv("wcaa_strikes_2016_2024_clean.csv")


# =============================================================================
# 📁 1. FAA Data Summary (All Years Except 2024)
# =============================================================================

result_faa <- faa %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(
    strike_total = n(),                                  # total strikes
    total_sent   = sum(REMAINS_SENT == TRUE),            # count sent
    percent_sent = mean(REMAINS_SENT == TRUE) * 100,     # % sent
    .groups = "drop"
  ) %>%
  filter(INCIDENT_YEAR != 2024)                          # exclude 2024


# =============================================================================
# 📁 2. WCAA Strike-Report Summary (2024 Only)
# =============================================================================

# Inspect Remains field values
unique(dat$Remains)

# Summarize 2024 WCAA data
result_dat <- dat %>%
  filter(Strike.Year == 2024) %>%
  summarise(
    INCIDENT_YEAR = 2024,
    strike_total  = n(),
    total_sent    = sum(Remains == "Sent to Smithsonian"),
    percent_sent  = mean(Remains == "Sent to Smithsonian") * 100,
    .groups = "drop"
  )


# =============================================================================
# 🔁 3. Combine FAA + WCAA Results
# =============================================================================

result <- bind_rows(result_faa, result_dat)

# View combined results
print(result)

# Save table
write.csv(
  result,
  "Table_Annual_Smithsonian.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 4. Create Annual % Submission Plot
# =============================================================================

si <- ggplot(result,
             aes(x = INCIDENT_YEAR,
                 y = percent_sent)) +
  geom_line(color = "palegreen4", size = 2) +
  geom_point(color = "palegreen4", size = 5) +
  labs(
    x = "Year",
    y = "Samples sent to the\nSmithsonian Institution (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.text.x  = element_text(size = 16, color = "black")
  )

# Preview plot
si

# Save plot
ggsave(
  "Plot_Annual_Smithsonian_Percent.jpeg",
  plot   = si,
  width  = 8,
  height = 6,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################
