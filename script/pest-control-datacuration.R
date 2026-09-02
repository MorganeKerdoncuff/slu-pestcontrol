##
#### DESCRIPTION ####
##
## Purpose: Data curating script for wp1 Vulnerability Analysis
## Author: Morgane KERDONCUFF
## ORCID: 0000-0003-2223-1857
## github
## Date created: 09/2026
## Project: Att förbereda svenskt jordbruk för en allt mindre tillgång till kemiska insekticider - sårbarhetsanalys och möjliga alternativ
## Project number: O-25-20-074
## Funding: Stiftelsen Lantbruksforskning
## Institution: Swedish University of Agricultural Sciences

#### PACKAGES ####

library(tidyverse) # R language
library(janitor) # Data cleaning
library(readxl) # Read xl files
library(purrr) # Merge tables
library(ggplot2) # Graphical representation
library(forcats) # Plot reorder

#### RAW DATA ####

yieldha_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-yieldhaxcountyxcrop2015-2025.xlsx")
totyieldton_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-totyieldtonxcountyxcrop2015-2025.xlsx")
areaha_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-areahaxcountyxcrop2015-2025.xlsx")

#### PRELIMINARY DATA CLEANING EXCEL ####

# removal two first head rows and meta data
# removal "." and ".." for NAs
# expand county and cropType cells over whole associated cell range

#### TARGET CROPS ####

# Data cleaning

## New "year" variable
yieldha_county <- yieldha_county |> 
  pivot_longer(
    cols = c(-county, -cropType),
    names_to = "year",
    values_to = "yieldHectarKgHa"
  )
totyieldton_county <- totyieldton_county |> 
  pivot_longer(
    cols = c(-county, -cropType),
    names_to = "year",
    values_to = "totalYieldTon"
  )
areaha_county <- areaha_county |> 
  pivot_longer(
    cols = c(-county, -cropType),
    names_to = "year",
    values_to = "areaHa"
  )

# Variable distribution

## Summarise at national-level
yieldha_natio <- yieldha_county |>
  group_by(cropType) |> 
  summarise(yieldHectarKgHa = sum(yieldHectarKgHa, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(yieldHectarKgHa)))
totyieldton_natio <- totyieldton_county |>
  group_by(cropType) |> 
  summarise(totalYieldTon = sum(totalYieldTon, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(totalYieldTon)))
areaha_natio <- areaha_county |>
  group_by(cropType) |> 
  summarise(areaHa = sum(areaHa, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(areaHa)))

## Barplot

### Yield Ha
plot_yieldha_natio <- ggplot(
  yieldha_natio, aes(x = cropType, y = yieldHectarKgHa)
) +
  geom_bar(
    stat = "identity"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    axis.text = element_text(
      angle = 90
    )
  )
plot_yieldha_natio

### Total yield ton
plot_totyieldton_natio <- ggplot(
  totyieldton_natio, aes(x = cropType, y = totalYieldTon)
) +
  geom_bar(
    stat = "identity"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    axis.text = element_text(
      angle = 90
    )
  )
plot_totyieldton_natio

### Area Ha
plot_areaha_natio <- ggplot(
  areaha_natio, aes(x = cropType, y = areaHa)
) +
  geom_bar(
    stat = "identity"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    axis.text = element_text(
      angle = 90
    )
  )
plot_areaha_natio