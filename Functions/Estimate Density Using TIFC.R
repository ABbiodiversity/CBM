#-----------------------------------------------------------------------------------------------------------------------

# Title:       Functions to improve the TIFC estimation workflow
# Description:
# Date:        November 2022
# Author:      Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Install required packages if needed
if (!requireNamespace("dplyr"))
  install.packages("dplyr")
if (!requireNamespace("readr"))
  install.packages("readr")
if (!requireNamespace("tidyr"))
  install.packages("tidyr")
if (!requireNamespace("stringr"))
  install.packages("stringr")
if (!requireNamespace("purrr"))
  install.packages("purrr")
if (!requireNamespace("lubridate"))
  install.packages("lubridate")

# Attach required packages
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

#-----------------------------------------------------------------------------------------------------------------------

#' Consolidate tags
#' Run this function of the tag report to consolidate tags from the same image into the same row.
#' Note: There is an issue with respect to multiple images with the same timestamp.
#'
#' @param main_report Directly from WildTrax
#'

consolidate_tags <- function(main_report) {

  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

  # Load native_sp tags
  load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

  # Species tags w/ full information.
  y <- main_report |>
    filter(species_common_name %in% native_sp,
           !individual_count == "VNA") |>
    mutate(individual_count = as.numeric(individual_count)) |>
    mutate(age_class = trimws(strrep(str_c(age_class, ", "), individual_count), whitespace = ", "),
           sex_class = trimws(strrep(str_c(sex_class, ", "), individual_count), whitespace = ", ")) |>
    # Now grouping my project_id, location_id, and image_id
    group_by(project_id, location_id, image_id, species_common_name) |>
    mutate(individual_count = sum(individual_count),
           age_class = paste0(age_class, collapse = ", "),
           sex_class = paste0(sex_class, collapse = ", ")) |>
    distinct(project_id, location, image_id, species_common_name, .keep_all = TRUE) |>
    ungroup() |>
    mutate(individual_count = as.character(individual_count))

  # Tags of species that have VNA - usually we don't care about these, but don't want to lose info here.
  z <- main_report |>
    filter(species_common_name %in% native_sp,
           individual_count == "VNA")

  # All the STAFF/SETUP, etc, then bound back together.
  x <- main_report |>
    filter(!species_common_name %in% native_sp) |>
    bind_rows(y, z)

  return(x)

}

#-----------------------------------------------------------------------------------------------------------------------

#' Make VegForDetDist
#' Make VegForDetectionDistance column out of VEGHFAGEclass information
#'
#' @param x A dataframe with VEGHFAGEclass information
#'

make_vegfordetdist <- function(x) {

  x <- x |>
    mutate(VegForDetectionDistance = case_when(
      str_detect(VEGHFAGEclass, "Fen|Bog|Marsh|dSwamp") ~ "WetTreed",
      str_detect(VEGHFAGEclass, "Decid") ~ "Decid",
      str_detect(VEGHFAGEclass, "Spruce|Pine|Mixed") ~ "Conif",
      str_detect(VEGHFAGEclass, "Water") ~ "Water",
      str_detect(VEGHFAGEclass, "Grass") ~ "Grass",
      str_detect(VEGHFAGEclass, "Shrub") ~ "WetGrass",
      TRUE ~ "HF"
    ))

  return(x)

}

#-----------------------------------------------------------------------------------------------------------------------

#' Obtain N gap class
#' a 'NONE' image is used to demarcate when a series should be truncated because an animal left the field of view.
#'
#' @param main_report_clean A cleaned up tags report dataframe
#'

obtain_n_gap_class <- function(main_report_clean) {

  # Load native_sp tags
  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"
  load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

  y <- main_report_clean |>
    select(project, location, image_date_time, species_common_name) |>
    arrange(project, location, image_date_time) |>
    # Create gap class column
    mutate(species_common_name_next = lead(species_common_name),
           gap_class = ifelse(species_common_name != "NONE" & species_common_name_next == "NONE", "N", NA)) |>
    # Include only N gap class for native mammals
    filter(gap_class == "N",
           species_common_name %in% native_sp) |>
    select(-c(species_common_name_next))

  return(y)

}

#-----------------------------------------------------------------------------------------------------------------------

#' Get operating days
#' Option to summarise according to ABMI seasonal definitions
#'
#' @param image_report Image report from WildTrax; must include the columns `location`, `field_of_view`, and ``
#' @param include_project logical; Summarise across project, or just by location?
#' @param summarise logical; Summarise the total number of days?
#' @param .abmi_seasons logical; Include ABMI seasonal periods? Defaults to FALSE
#'

get_operating_days <- function(image_report, include_project = TRUE, summarise = FALSE, .abmi_seasons = FALSE) {

  # Julian days for the new seasonal EDD values
  spring.start <- 99
  summer.start <- 143
  winter.start <- 288

  if(include_project) {
    x <- image_report |>
      unite("project_location", project, location, sep = "_", remove = TRUE)
  } else {
    x <- image_report |>
      rename(project_location = location)
  }

  # Locations which started operation again after an intermediate pause
  inter <- x |>
    filter(image_fov == "START - First Good Image in FOV" | image_fov == "END - Last Good Image in FOV") |>
    group_by(project_location) |>
    tally() |>
    # Locations that have n = 1 will only have an 'END' (they did not re-start operation)
    filter(n > 1) |>
    select(project_location)

  if(nrow(inter) > 0) {

    inter_pairs <- x |>
      filter(image_fov == "START - First Good Image in FOV" | image_fov == "END - Last Good Image in FOV") |>
      # Return all rows with a match in inter
      semi_join(inter, by = c("project_location")) |>
      arrange(project_location, image_date_time) |>
      select(project_location, image_date_time, image_fov) |>
      group_by(project_location) |>
      # This code is gross. Do we still need to do this for camera data pre-2019?
      mutate(starts_again = ifelse(lead(image_fov) == "START - First Good Image in FOV" & image_fov == "END - Last Good Image in FOV", 1, NA),
             restart = ifelse(lag(starts_again) == "1" & lag(image_fov) == "END - Last Good Image in FOV", 1, NA)) |>
      filter(starts_again == "1" | restart == "1") |>
      select(-c(starts_again, restart)) |>
      ungroup() |>
      group_split(image_fov) |>
      bind_cols(.name_repair = "unique") |>
      mutate(time_diff = difftime(`image_date_time...5`, `image_date_time...2`, units = "hours")) |>
      filter(time_diff > 12) |>
      select(1:3, 5, 6)

    to_remove <- inter_pairs |>
      mutate(end_date = as.Date(`image_date_time...2`),
             start_date = as.Date(`image_date_time...5`)) |>
      select(project_location = 1, end_date, start_date) |>
      rowwise() |>
      mutate(date = list(seq(from = end_date, to = start_date, by = "day"))) |>
      unnest(date) |>
      select(project_location, date)

    range <- x |>
      # Remove Out of Range images
      filter(image_fov == "WITHIN") |>
      group_by(project_location) |>
      summarise(start_date = as.Date(min(image_date_time)),
                end_date = as.Date(max(image_date_time))) |>
      filter(!is.na(end_date)) |>
      group_by(project_location) |>
      mutate(date = list(seq(from = start_date, to = end_date, by = "day"))) |>
      unnest(date) |>
      ungroup() |>
      select(project_location, date) |>
      # Remove days where the camera was not working
      anti_join(to_remove, by = c("project_location", "date")) |>
      mutate(operating = 1)

  } else {

    range <- x |>
      # Remove Out of Range images
      filter(image_fov == "WITHIN") |>
      group_by(project_location) |>
      summarise(start_date = as.Date(min(image_date_time)),
                end_date = as.Date(max(image_date_time))) |>
      filter(!is.na(end_date)) |>
      group_by(project_location) |>
      mutate(date = list(seq(from = start_date, to = end_date, by = "day"))) |>
      unnest(date) |>
      ungroup() |>
      select(project_location, date) |>
      mutate(operating = 1)

  }

  if(summarise) {
    if(.abmi_seasons) {
      range <- range |>
        # Create variable for Julian date
        mutate(julian = as.numeric(format(ymd(date), "%j"))) |>
        # Create variable for new season definitions
        mutate(season = as.factor(case_when(
          julian >= spring.start & julian <= summer.start ~ "Spring",
          julian > summer.start & julian <= winter.start ~ "Summer",
          TRUE ~ "Winter"))) |>
        mutate_at(c("project_location", "season"), factor) |>
        group_by(project_location, season, .drop = FALSE) |>
        summarise(operating_days = sum(operating)) |>
        ungroup() |>
        mutate_if(is.factor, as.character) |>
        group_by(project_location) |>
        mutate(total_days = sum(operating_days)) |>
        pivot_wider(id_cols = c(project_location, total_days), names_from = season, values_from = operating_days) |>
        ungroup()
    } else {
      range <- range |>
        group_by(project_location) |>
        summarise(total_days = sum(operating)) |>
        ungroup()
    }
  } else {
    range <- range |> select(-operating)
  }

  if(include_project) {
    #range <- range |>
    #  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE)
    return(range)
  } else {
    range <- range |>
      rename(location = project_location)
    return(range)
  }

}

#-----------------------------------------------------------------------------------------------------------------------

# Define function to extract and summarise number of operating days based on custom date ranges (i.e., monitoring periods)
# @param x The dataframe of all deployments and operating days
# @param date_start The start date; will be converted to a Date automatically
# @param date_end The end date; will be converted to a Date automatically

# Issue: the filter removes 0 combinations, which we probably (?) want to keep.

summarise_op_days_by_mp <- function(x, date_start, date_end, season = TRUE) {

  # Convert date_start and date_end to Date
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)

  # Summarise based on custom date range
  if(season) {
    y <- x |>
      filter(date >= date_start,
             date <= date_end) |>
      mutate_at(c("location", "season"), factor) |>
      group_by(location, season, .drop = FALSE) |>
      summarise(operating_days = sum(operating)) |>
      ungroup() |>
      pivot_wider(id_cols = location, names_from = season, values_from = operating_days)

    if("winter" %in% names(y)) {
      y
    } else {
      y <- y |> mutate(winter = 0)
    }

    if("summer" %in% names(y)) {
      y
    } else {
      y <- y |> mutate(summer = 0)
    }

    z <- y |>
      mutate(total_days = summer + winter) |>
      select(location, total_days, total_summer_days = summer, total_winter_days = winter)
  } else {
    z <- x |>
      filter(date >= date_start,
             date <= date_end) |>
      mutate_at(c("location"), factor) |>
      group_by(location, .drop = FALSE) |>
      summarise(total_days = sum(operating)) |>
      ungroup()
  }

  return(z)

}


#-----------------------------------------------------------------------------------------------------------------------

# This function is not suited for our internal purposes, but might be useful code for wildRtrax at some point?

#'
#' @param tag_report
#'

group_tags_into_series <- function(tag_report, threshold, summarise = FALSE) {

  series <- tag_report |>
    # Remove records with VNA as number_individuals -> these are misc human & birds
    filter(!number_individuals == "VNA") |>
    # Turn into numeric
    mutate(number_individuals = as.numeric(number_individuals)) |>
    # Order the dataframe
    arrange(project, location, date_detected, common_name) |>
    group_by(project, location, common_name) |>
    # Calculate the time difference between subsequent images
    mutate(interval = abs(int_length(date_detected %--% lag(date_detected)))) |>
    # Is this considered a new detection?
    mutate(new_detection = ifelse(is.na(interval) | interval >= threshold, TRUE, FALSE)) |>
    ungroup() |>
    # Number the series
    mutate(series = c(1, cumsum(new_detection[-1]) + 1))
  # Flag gaps that will need

  # "Naive" summary - no probabilistic gaps, etc.
  if(summarise) {
    series_summary <- series |>
      select(project, location, date_detected, common_name, age_class, sex, number_individuals, series) |>
      group_by(series, project, location, common_name) %>%
      summarise(start_time = min(date_detected),
                end_time = max(date_detected),
                total_duration_seconds = int_length(start_time %--% end_time),
                n_images = n(),
                avg_animals = mean(number_individuals))
    return(series_summary)
  } else {
    return(series)
  }

}

#-----------------------------------------------------------------------------------------------------------------------

#' Calculate time by series
#' This is for internal purposes only - unless that lookup information could be included as data in the package?
#'
#' @param main_report_clean
#'

calculate_time_by_series <- function(main_report_clean) {

  # Path to Google Drive
  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

  # Load lookup information:

  # Native sp strings in WildTrax
  load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))
  # Gap groups
  gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))
  # Leaving probability predictions
  leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
  # Time between images
  tbi <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))

  # Retrieve gap class NONES
  nones <- obtain_n_gap_class(main_report_clean)

  series <- main_report_clean |>
    # Remove records with VNA as individual_count -> these are misc human & birds
    filter(!individual_count == "VNA",
           species_common_name %in% native_sp) |>
    # Convert individual_count to numeric
    mutate(individual_count = as.numeric(individual_count)) |>
    # Join gap class
    # NOTE: Need to fix the N/tagged gap class issue. TO DO. <- What is this?!
    left_join(nones, by = c("location", "project", "image_date_time", "species_common_name")) |>
    # Order observations
    arrange(project, location, image_date_time, species_common_name) |>
    # Identify series and gaps requiring probabilistic time assignment
    mutate(series_num = 0,
           # Lagged time stamp
           image_date_time_previous = lag(image_date_time),
           # Lead time stamp
           image_date_time_next = lead(image_date_time),
           # Calculate difference in time between ordered images
           diff_time_previous = as.numeric(image_date_time - image_date_time_previous),
           diff_time_next = as.numeric(abs(image_date_time - image_date_time_next)),
           # Lagged species
           species_common_name_previous = lag(species_common_name),
           # Was is a different species?
           diff_sp = ifelse(species_common_name != species_common_name_previous, TRUE, FALSE),
           # Lagged deployment
           location_previous = lag(location),
           # Was is a different deployment?
           diff_location = ifelse(location != location_previous, TRUE, FALSE),
           # Flag gaps that will need checking
           gap_check = ifelse(diff_location == FALSE & diff_sp == FALSE & (diff_time_previous <= 120 & diff_time_previous >= 20), 1, 0),
           # Lagged gap class
           gap_class_previous = replace_na(lag(gap_class), ""),
           # Identify new series, based on being different deployment, species, greater than 120 seconds, and approp gaps
           diff_series = ifelse(diff_location == TRUE | diff_sp == TRUE | diff_time_previous > 120 | (gap_class_previous == "L" | gap_class_previous == "N"), 1, 0),
           # Number series
           series_num = c(1, cumsum(diff_series[-1]) + 1),
           # Flag gaps that require probabilistic time assignment
           gap_prob = replace_na(ifelse(gap_check == 1 & (gap_class_previous == "" | gap_class_previous == "U"), 1, 0), 0)) |>
    group_by(series_num) |>
    mutate(diff_time_previous = ifelse(row_number() == 1, 0, diff_time_previous),
           diff_time_next = ifelse(row_number() == n(), 0, diff_time_next)) |>
    ungroup() |>
    # Join gap group lookup table
    left_join(gap_groups, by = "species_common_name") |>
    # Join gap leaving predictions
    left_join(leave_prob_pred, by = c("gap_group", "diff_time_previous" = "diff_time")) |>
    # Adjust time difference between ordered images that require probabilistic time assignment
    mutate(pred = replace_na(pred, 1),
           diff_time_previous_adj = ifelse(gap_prob == 1, diff_time_previous * (1 - pred), diff_time_previous),
           diff_time_next_adj = ifelse(lead(gap_prob == 1), diff_time_next * (1 - lead(pred)), diff_time_next),
           diff_time_next_adj = ifelse(is.na(diff_time_next_adj), 0, diff_time_next_adj))

  # Calculate total time in front of the camera, by series
  tts <- series %>%
    left_join(tbi, by = "species_common_name") |>
    group_by(series_num) |>
    mutate(# Check whether the image was first or last in a series
      bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
      # Calculate time for each individual image
      image_time = ifelse(bookend == 1,
                          ((diff_time_previous_adj + diff_time_next_adj) / 2) + (tbp / 2),
                          (diff_time_previous_adj + diff_time_next_adj) / 2),
      # Multiply image time by the number of animals present
      image_time_ni = image_time * individual_count) |>
    # Group by common name as well to add it as a variable to output
    group_by(project, location, species_common_name, .add = TRUE) |>
    # Calculate total time and number of images for each series
    summarise(n_images = n(),
              series_total_time = sum(image_time_ni),
              series_start = min(image_date_time),
              series_end = max(image_date_time)) |>
    ungroup() |>
    # Double the series time of single-series images (halved in an earlier step when it shouldn't be)
    mutate(series_total_time = ifelse(n_images < 2, series_total_time * 2, series_total_time)) |>
    select(project, location, series_num, species_common_name, series_start, series_end, series_total_time, n_images)

  return(tts)

}

#-----------------------------------------------------------------------------------------------------------------------

#' Calculate total time in front of the camera, by deployment, project, and species.
#'
#' @param series Time by series dataframe
#' @param tbd Time by day summary; assume 'project_location' field.
#' @param summer.start.j Julian date of summer period start; defaults to 106 (April 16)
#' @param summer.end.j Julian date of summer period end; defaults to 288 (October 15)
#'

sum_total_time <- function(series, tbd, spring.start = 99, summer.start = 143, winter.start = 288) {

  # Summarise total time
  tt <- series |>
    unite("project_location", project, location, sep = "_", remove = TRUE) |>
    mutate(date = date(series_start)) |>
    # Create variable for Julian date
    mutate(julian = as.numeric(format(ymd(date), "%j"))) |>
    # Create variable for new season definitions
    mutate(season = as.factor(case_when(
      julian >= spring.start & julian <= summer.start ~ "Spring",
      julian > summer.start & julian <= winter.start ~ "Summer",
      TRUE ~ "Winter"))) |>
    mutate_at(c("project_location", "species_common_name", "season"), factor) |>
    group_by(project_location, species_common_name, season, .drop = FALSE) |>
    summarise(total_duration = sum(series_total_time)) |>
    ungroup() |>
    mutate_if(is.factor, as.character) |>
    left_join(tbd, by = c("project_location"))

  # Unique species seen
  sp <- as.character(sort(unique(tt$species_common_name)))

  tt_nn <- tbd |>
    # Retrieve only those that had no images of animals
    anti_join(tt, by = "project_location") |>
    crossing(season = c("Spring", "Summer", "Winter"), species_common_name = sp) |>
    # Add total_duration column, which is zero in these cases
    mutate(total_duration = 0)

  tt_full <- tt |>
    bind_rows(tt_nn) |>
    arrange(project_location, species_common_name, season) |>
    mutate(total_season_days = case_when(
      season == "Spring" ~ Spring,
      season == "Summer" ~ Summer,
      season == "Winter" ~ Winter)) |>
    separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
    select(project, location, species_common_name, season, total_season_days, total_duration)

  return(tt_full)

}

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

#' @param tt Total time in front of the camera by location, season, and species.
#' @param veg Lookup table of the VegForDetectionDistance category of each camera location.
#' @param cam_fov_angle Numeric; defaults to 40.

calc_density_by_loc <- function(tt, veg, cam_fov_angle = 40, format = "long", include_project = TRUE) {

  # Path to Google Drive
  g_drive <- "G:/Shared drives/ABMI Mammals/"
  # Effective detection distance (EDD) predictions lookup
  edd <- read_csv(paste0(g_drive, "Data/Detection Distance/EDD Predictions by Species Group Season Veg Category.csv"))
  # EDD species groups
  dist_groups <- read_csv(paste0(g_drive, "Data/Detection Distance/Species Detection Distance Groups.csv"))

  d <- tt |>
    unite("project_location", project, location, sep = "_", remove = TRUE) |>
    # Join species EDD groups
    left_join(dist_groups, by = "species_common_name") |>
    # Join detection distance vegetation values
    left_join(veg, by = "project_location") |>
    # Join EDD predictions
    left_join(edd, by = c("dist_group", "season", "overall_category")) |>
    # Remove random species (mostly birds)
    # Note: Also got rid of ACME 2023 (for now).
    filter(!is.na(edd)) |>
    # Calculate density
    mutate(effort = total_season_days * (edd ^ 2 * pi * (cam_fov_angle / 360)) / 100,
           # Catch per unit effort
           cpue = total_duration / effort,
           # Catch per unit effort in km2
           cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
    separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
    # All the NaNs are just combinations where the total_seasons_days is 0.
    select(project, location, species_common_name, season, total_season_days, total_duration, density_km2 = cpue_km2)

  if(format == "long") {
    return(d)
  } else {
    t <- d |>
      mutate(season = ifelse(season == "winter", "Winter", "Summer")) |>
      select(project, location, season, total_season_days) |>
      distinct() |>
      pivot_wider(id_cols = c(project, location), names_from = season, values_from = total_season_days)
    d <- d |>
      mutate(season = ifelse(season == "winter", "Winter", "Summer")) |>
      pivot_wider(id_cols = c(project, location), names_from = c(species_common_name, season), values_from = density_km2) |>
      left_join(t, by = c("project", "location")) |>
      select(project, location, Summer_Days = Summer, Winter_Days = Winter, everything())
    return(d)
  }

}

#-----------------------------------------------------------------------------------------------------------------------
