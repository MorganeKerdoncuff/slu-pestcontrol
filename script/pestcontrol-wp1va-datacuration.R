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

### County level
# yieldha_county <- yieldha_county |> 
#   pivot_longer(
#     cols = c(-county, -cropType),
#     names_to = "year",
#     values_to = "yieldHectarKgHa"
#   )
# totyieldton_county <- totyieldton_county |> 
#   pivot_longer(
#     cols = c(-county, -cropType),
#     names_to = "year",
#     values_to = "totalYieldTon"
#   )
# areaha_county <- areaha_county |> 
#   pivot_longer(
#     cols = c(-county, -cropType),
#     names_to = "year",
#     values_to = "areaHa"
#   )
# nbfarm_county <- nbfarm_county |> 
#   pivot_longer(
#     cols = c(-county, -cropType),
#     names_to = "year",
#     values_to = "numberFarms"
#   )

### Production area level
totyieldton_prodarea <- totyieldton_prodarea |> 
  pivot_longer(
    cols = c(-productionArea, -cropType),
    names_to = "year",
    values_to = "totalYieldTon"
  )
areaha_prodarea <- areaha_prodarea |> 
  pivot_longer(
    cols = c(-productionArea, -cropType),
    names_to = "year",
    values_to = "areaHa"
  )
nbfarm_prodarea <- nbfarm_prodarea |> 
  pivot_longer(
    cols = c(-productionArea, -cropType),
    names_to = "year",
    values_to = "numberFarms"
  )

## Dataset

### County level
# crop_county <- purrr::reduce(list(
#   yieldha_county,
#   totyieldton_county,
#   areaha_county,
#   nbfarm_county
# ), dplyr::left_join)

### Production area
crop_prodarea <- purrr::reduce(list(
  totyieldton_prodarea,
  areaha_prodarea,
  nbfarm_prodarea
), dplyr::left_join)

# Variable distribution

## Crop types without farm number & area data
yieldonly <- crop_prodarea |> 
  group_by(cropType) |> 
  summarise(
    numberFarms = sum(numberFarms, na.rm = TRUE)
  ) |> 
  filter(numberFarms == 0)
unique(yieldonly$cropType)

## Summarise at national-level for 2025

### County level
# crop_county_natio <- crop_county |>
#   filter(year == "2025") |> 
#   group_by(cropType) |>
#   summarise(
#     yieldHectarKgHa = sum(yieldHectarKgHa, na.rm = TRUE),
#     totalYieldTon = sum(totalYieldTon, na.rm = TRUE),
#     areaHa = sum(areaHa, na.rm = TRUE),
#     numberFarms = sum(numberFarms, na.rm = TRUE)
#   )

### Production area level
crop_prodarea_natio <- crop_prodarea |>
  # selection only common crop types between all variables
  filter(numberFarms > 0) |> 
  # selection only 2025
  filter(year == "2025") |> 
  group_by(cropType) |>
  summarise(
    totalYieldTon = sum(totalYieldTon, na.rm = TRUE),
    areaHa = sum(areaHa, na.rm = TRUE),
    numberFarms = sum(numberFarms, na.rm = TRUE)
  )

## Barplot
bardistri <- function(data) {
  distriplot <- data |> 
    # mutate(var1 = fct_reorder(var1, desc(var2))) |> 
    ggplot() +
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
  
  distriplot  
}

## County level

# ### Yield Ha
# plot_yieldha_countynatio <- bardistri(crop_county_natio, cropType, yieldHectarKgHa)
# ### Total yield ton
# plot_totyieldton_countynatio <- bardistri(crop_county_natio, cropType, totalYieldTon)
# ### Area Ha
# plot_areaha_countynatio <- bardistri(crop_county_natio, cropType, areaHa)
# ### Number farms
# plot_nbfarm_countynatio <- bardistri(crop_county_natio, cropType, numberFarms)

## Production area level

### Total yield ton - more crop types than for farm & area
totyieldton_prodarea_2025 <- totyieldton_prodarea |> 
  filter(year == "2025") |> 
  group_by(cropType) |>
  summarise(
    totalYieldTon = sum(totalYieldTon, na.rm = TRUE),
  )

plot_totyield_natio <- totyieldton_prodarea_2025 |> 
  mutate(cropType = fct_reorder(cropType, desc(totalYieldTon))) |> 
  ggplot(
    aes(x = cropType, y = totalYieldTon)
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
plot_totyield_natio

### Area Ha
plot_area_natio <- crop_prodarea_natio |> 
  mutate(cropType = fct_reorder(cropType, desc(areaHa))) |> 
  ggplot(
    aes(x = cropType, y = areaHa)
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
plot_area_natio

### Number farms
plot_nbfarm_natio <- crop_prodarea_natio |> 
  mutate(cropType = fct_reorder(cropType, desc(numberFarms))) |> 
  ggplot(
    aes(x = cropType, y = numberFarms)
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

### All grouped
plot_areayieldfarm_natio <- crop_prodarea_natio |> 
  mutate(
  totalYieldTon = totalYieldTon/10,
  numberFarms = numberFarms*10
  ) |> 
  pivot_longer(
    cols = c(totalYieldTon, areaHa, numberFarms),
    names_to = "variables",
    values_to = "value"
  ) |> 
  mutate(cropType = fct_reorder(cropType, desc(value))) |> 
  ggplot(
    aes(x = cropType, y = value, fill = variables)
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_grey() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    axis.text = element_text(
      angle = 90
    )
  )
plot_areayieldfarm_natio
ggsave("outputs/exploratory/area+yield+farm.png", plot = get_last_plot(), width = 30, height = 20, units = "cm")
    
## Scatterplot

### farm x area
plot_areaxfarm <- crop_prodarea_natio |> 
  ggplot(
    aes(x = areaHa, y = numberFarms)
  ) +
  geom_text(
    label = crop_prodarea_natio$cropType
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey")
  )
plot_areaxfarm
ggsave("outputs/exploratory/areaxfarm.png", plot = get_last_plot(), width = 30, height = 20, units = "cm")

### farm x yield
plot_yieldxfarm <- crop_prodarea_natio |> 
  ggplot(
    aes(x = totalYieldTon, y = numberFarms)
  ) +
  geom_text(
    label = crop_prodarea_natio$cropType
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey")
  )
plot_yieldxfarm
ggsave("outputs/exploratory/farmxyield.png", plot = get_last_plot(), width = 30, height = 20, units = "cm")

### yield x area
plot_areaxyield <- crop_prodarea_natio |> 
  ggplot(
    aes(x = areaHa, y = totalYieldTon)
  ) +
  geom_text(
    label = crop_prodarea_natio$cropType
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey")
  )
plot_areaxyield
ggsave("outputs/exploratory/areaxyield.png", plot = get_last_plot(), width = 30, height = 20, units = "cm")
