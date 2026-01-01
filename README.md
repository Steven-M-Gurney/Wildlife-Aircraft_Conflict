# DTW Wildlife–Aircraft Strike Analysis

### [Steven Gurney](https://linktr.ee/stevenmgurney)

### Data: Some data used in this study are sensitive and not included here.

#### Please contact the first author for questions about the code or data: Steven M. Gurney (stevenmgurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Summary
This project provides a reproducible workflow for summarizing and reporting wildlife–aircraft strike activity at Detroit Metropolitan Wayne County Airport (DTW). By compiling and standardizing multiple years of strike data (2016–2024), the project supports annual reporting, safety performance monitoring, and wildlife-hazard risk assessment. The resulting summaries and visualizations help inform airport wildlife-management decisions and regulatory compliance.
__________________________________________________________________________________________________________________________________________

## Repository Directory


### [faa_strikes_2016_2024_clean.csv](./faa_strikes_2016_2024_clean.csv): Prepped FAA strike data for DTW, used in the workflow here and created using Strike_Data_Prep.R.

### [ops_2016_2024.csv](./ops_2016_2024.csv): Prepped DTW aircraft-operations data, used in the workflow here and created using Strike_Data_Prep.R.

### [Strike_Data_Prep.R](./Strike_Data_Prep.R): Code for prepping FAA and WCAA data, including DTW aircraft operations.

### [Smithsonian.R](./Smithsonian.R): Code for summarizing annual strike samples sent to the Smithsonian lab for testing.

### [Pilot_Reporting.R](./Pilot_Reporting.R): Code for summarizing annual strike resports submitted by pilots.

### [Summaries.R](./Summaries.R): Code for summarizing annual strike resports by runway, guild, species, and month.

### [Damage.R](./Damage.R): Code for summarizing annual damaging strike resports, calculating annual damaging rates, and producing a control chart.

### [Disruptive.R](./Disruptive.R): Code for summarizing annual disruptive strike reports, calculating annual disruptive rates, and producing a control chart.

### [Gulls.R](./Gulls.R): Code for prepping gull data and summarizing gull strikes by day of week.

## Supplemental Information

### [Strike_Summaries_2024.pdf](./Strike_Summaries_2024.pdf): High-level presentation given to the DTW Wildlife Hazard Management Committee, February 2025.
