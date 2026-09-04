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
library(dplyr) # Formatting
library(janitor) # Data cleaning
library(readxl) # Read xl files
library(purrr) # Merge tables
library(ggplot2) # Graphical representation
library(forcats) # Plot reorder

#### RAW DATA ####

## County level
yieldha_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-yieldhaxcountyxcrop2015-2025.xlsx")
totyieldton_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-totyieldtonxcountyxcrop2015-2025.xlsx")
areaha_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-areahaxcountyxcrop2015-2025.xlsx")
nbfarm_county <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-nbfarmxcountyxcrop2015-2025.xlsx")

## Production area level
totyieldton_prodarea <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-totyieldtonxprodareaxcrop2021-2025.xlsx")
areaha_prodarea <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-areahaxprodareaxcrop2021-2025.xlsx")
nbfarm_prodarea <- read_excel(path = "data/rawdata/slupesticidefree-wp1va-jordbruksverket-nbfarmxprodareaxcrop2021-2025.xlsx")

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
nbfarm_county <- nbfarm_county |> 
  pivot_longer(
    cols = c(-county, -cropType),
    names_to = "year",
    values_to = "numberFarms"
  )

## Dataset
crop_county <- purrr::reduce(list(
  yieldha_county,
  totyieldton_county,
  areaha_county,
  nbfarm_county
), dplyr::left_join)

# Variable distribution

## Summarise at national-level for 2025
yieldha_natio <- yieldha_county |>
  filter(year == "2025") |> 
  group_by(cropType) |> 
  summarise(yieldHectarKgHa = sum(yieldHectarKgHa, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(yieldHectarKgHa)))
totyieldton_natio <- totyieldton_county |>
  filter(year == "2025") |> 
  group_by(cropType) |> 
  summarise(totalYieldTon = sum(totalYieldTon, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(totalYieldTon)))
areaha_natio <- areaha_county |>
  filter(year == "2025") |> 
  group_by(cropType) |> 
  summarise(areaHa = sum(areaHa, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(areaHa)))
nbfarm_natio <- nbfarm_county |>
  filter(year == "2025") |> 
  # remove total farm number per county
  filter(cropType != "Total åkermark") |> 
  group_by(cropType) |> 
  summarise(numberFarms = sum(numberFarms, na.rm = TRUE)) |> 
  mutate(cropType = fct_reorder(cropType, desc(numberFarms)))

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

### Number farms
plot_nbfarm_natio <- ggplot(
  nbfarm_natio, aes(x = cropType, y = numberFarms)
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
plot_nbfarm_natio