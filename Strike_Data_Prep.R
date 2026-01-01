###############################################################################
# WCAA–FAA Strike Data Preparation (2016–2024)
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Wrangle, clean, and format WCAA (2016–2024) strike data
#   • Prepare corresponding FAA strike data for the same period
#   • Create annual aircraft-operations totals for rate calculations
#   • Harmonize key fields (species, guild, runway groups, etc.) across sources
#   • Ensure analyses use the most current and authoritative data (FAA > WCAA)
#
# Notes:
#   • There are inconsistencies between WCAA and FAA strike data; FAA has final say.
#   • FAA data can lag behind WCAA’s by 4+ months.
#   • Historical records may be updated over time; always re-pull most recent exports.
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(tidyverse)
library(stringr)


# =============================================================================
# 📊 1. Aircraft Operations by Year (WCAA)
# =============================================================================

# Sum operations for 2024 month-by-month (annual report not available yet).
twenty.four <- 21898 + 21315 + 25089 + 24946 +
  26089 + 26502 + 26576 + 27299 +
  26047 + 27009 + 25088 + 25583

# Create data frame with annual operations (hard-coded from WCAA online data).
ops <- data.frame(
  Year = 2016:2024,
  Operations = c(
    393427, 395357, 393681, 396909,
    238574, 286909, 284606, 290238,
    twenty.four
  )
)

# View the operations data frame
print(ops)

# Save prepped data
write.csv(ops,
          "ops_2016_2024.csv",
          row.names = FALSE)

# =============================================================================
# 📁 2. Prepare WCAA Strike Data
# =============================================================================

# Read in data exported from WCAA Wildlife Command Center (sensitive data).
# Note: these data may contain pending information that is later updated.
dat <- read.csv("WCAA_Export_2016-2024_Strikes_08Jan2025.csv")

# Inspect structure for potential issues.
print(colnames(dat))           # Column names
print(unique(dat$Species.Name)) # Species list (check for typos/goofs)

# Filter out archived records (i.e., not submitted to FAA).
# Some Course.of.Action entries may be blank or "Revise";
# assume these are "Submit to FAA" (likely user error).
dat <- dat %>%
  filter(Course.of.Action. != "Archive (do not submit to FAA)")

# Convert species names to sentence case.
dat$Species.Name <- str_to_sentence(dat$Species.Name)

# Aggregate "Unknown bird" variants.
# Any species that includes both "UNKNOWN" and "BIRD" → "Unknown bird".
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
# These typically occurred when no biological samples were available.
dat <- dat %>%
  mutate(
    Guild = if_else(Guild == "" | is.na(Guild), "Unknown", Guild),
    Species.Name = if_else(Species.Name == "" | is.na(Species.Name),
                           "Unknown bird", Species.Name)
  )

# Standardize "Mammal" to "Mammals" (for consistency).
dat <- dat %>%
  mutate(Guild = if_else(Guild == "Mammal", "Mammals", Guild))

# Fix month format:
# remove the leading month number and dash, keep only the 3-letter month abbreviation.
dat$Strike.Month <- str_replace(dat$Strike.Month, "^\\d+-", "")

# Ensure months are ordered Jan–Dec.
dat <- dat %>%
  mutate(Strike.Month = factor(Strike.Month, levels = month.abb))

# Fix year format: remove commas and convert to numeric.
dat$Strike.Year <- str_replace_all(dat$Strike.Year, ",", "")
dat$Strike.Year <- as.numeric(dat$Strike.Year)

# Combine paired runways; all others labeled as "Other".
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

# Select columns of interest for downstream analyses.
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
    Aircraft.Registration,   # Helps identify pilot-reported strikes.
    Remains,                 # Helps identify Smithsonian submissions (2024+).
    # Ingredients for "Disruptive Event" below:
    # Defined as records indicating repair costs, other costs, damage,
    # negative effect on flight, downtime, or any combination
    # (Altringer et al. 2024).
    # Cost and damage code will ultimately be derived from FAA database.
    Damaging.Strike,
    Effect.On.Flight,
    Other.Effect,
    Estimated.Cost.of.Repairs....,
    Aircraft.Time.Out.of.Service..hrs.,
    Other.Costs....,
    Course.of.Action.,       # Helps derive number of pending IDs.
    Notes                    # Helpful reference.
  )

# Save prepped data
write.csv(dat,
          "wcaa_strikes_2016_2024_clean.csv",
          row.names = FALSE)


# =============================================================================
# 📁 3. Prepare FAA Strike Data
# =============================================================================

# Read in data exported from FAA database (public data).
# Note: these data may also be pending and subject to later updates.
faa <- read.csv("FAA_Export_All Strikes_09Jan2025.csv")

# Inspect column names.
print(colnames(faa))

# Filter to align with WCAA wildlife-program inception (2016–2024).
faa <- faa %>%
  filter(INCIDENT_YEAR >= 2016 & INCIDENT_YEAR <= 2024)

# Select columns of interest (similar to WCAA subset, with FAA-specific fields).
faa <- faa %>%
  select(
    INDX_NR,             # No unique ID to directly tie to WCAA records.
    INCIDENT_DATE,
    INCIDENT_YEAR,
    INCIDENT_MONTH,
    TIME,
    SPECIES,             # Species field (no guild).
    NUM_STRUCK,
    RUNWAY,
    REG,                 # Helps identify pilot-reported strikes.
    REMAINS_SENT,        # TRUE = sent to Smithsonian.
    # Disruptive Event ingredients (Altringer et al. 2024):
    INDICATED_DAMAGE,
    DAMAGE_LEVEL,        # FAA categorical ranking.
    EFFECT,
    EFFECT_OTHER,
    COST_REPAIRS,
    AOS,                 # Time out of service (blank = unknown).
    COST_OTHER,
    COST_REPAIRS_INFL_ADJ,  # Inflation-adjusted repairs (CPI-based).
    COST_OTHER_INFL_ADJ,    # Inflation-adjusted other costs.
    REMARKS,             # Helpful reference.
    COMMENTS             # Additional reference.
  )

# Inspect species contents.
print(unique(faa$SPECIES))

# Save prepped data
write.csv(faa,
          "faa_strikes_2016_2024_clean.csv",
          row.names = FALSE)

