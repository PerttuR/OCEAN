library(readxl)
library(dplyr)
library(tidyr)

# Path to the excel file
file_path <- "/Users/janiluke/Downloads/table2_statistics_testiversio.xlsx"

# 1. Inspect sheets
sheets <- excel_sheets(file_path)
cat("Sheets found in workbook:\n")
for (i in seq_along(sheets)) {
  cat(sprintf("  [%d] %s\n", i, sheets[i]))
}
cat("\n")

sheet1_name <- sheets[1]
sheet2_name <- sheets[2]

df1 <- read_excel(file_path, sheet = sheet1_name)
df2 <- read_excel(file_path, sheet = sheet2_name)

cat(sprintf("Sheet 1 ['%s'] columns: %s\n", sheet1_name, paste(names(df1), collapse = ", ")))
cat(sprintf("Sheet 2 ['%s'] columns: %s\n\n", sheet2_name, paste(names(df2), collapse = ", ")))

# Trawler gear types (OTM, OTB, PTM, OTT)
trawler_gears <- c("OTM", "OTB", "PTM", "OTT")

# Optional lookup from ICES rectangles to subdivisions 30 & 31 if needed
rect_lut_file <- "/Users/janiluke/Library/CloudStorage/OneDrive-Luonnonvarakeskus/VMS data/VMS2026/OCEAN/orig/ices_data/ICESrectangles_to_tilastoruutu_to_subdivisions.csv"
rect_lut <- if (file.exists(rect_lut_file)) {
  read.csv(rect_lut_file, stringsAsFactors = FALSE)
} else {
  NULL
}

# Function to prepare data: keeps standard column names Year, VE_ID, Area_clean
prepare_trawler_data <- function(df, sheet_label) {
  cols <- names(df)
  
  # Detect VE_ID or VE_REF or VesselID
  ve_col <- intersect(c("VE_ID", "VE_REF", "VesselID", "Vessel_ID", "ve_id", "ve_ref"), cols)
  if (length(ve_col) == 0) {
    stop("Could not find a vessel ID column in sheet: ", sheet_label)
  }
  
  # Detect Year
  year_col <- intersect(c("Year", "YEAR", "year", "vuosi", "Vuosi"), cols)
  if (length(year_col) == 0) {
    stop("Could not find a Year column in sheet: ", sheet_label)
  }
  
  # Detect Gear / MetierL4
  gear_col <- intersect(c("Gear", "GEAR", "LE_GEAR", "MetierL4", "METIER_L4", "gear", "metier_l4"), cols)
  
  # Detect Area / Subdivision / ICES rectangle
  area_col <- intersect(c("ICES_area", "Area", "Subdivision", "SubDivision", "SubDivisio", "ICESarea", "ICES_Area", "subdivision", "area"), cols)
  rect_col <- intersect(c("ICESrectangle", "ICES_RECT", "LE_RECT", "Rectangle", "ices_rectangle", "rect"), cols)
  
  work <- df
  work$VE_ID <- work[[ve_col[1]]]
  work$Year  <- work[[year_col[1]]]
  
  # Filter by Gear if present
  if (length(gear_col) > 0) {
    work <- work %>%
      filter(toupper(as.character(.data[[gear_col[1]]])) %in% trawler_gears)
  }
  
  # Assign Subdivision (SD30 / SD31)
  if (length(area_col) > 0) {
    area_vals <- as.character(work[[area_col[1]]])
    work <- work %>%
      mutate(
        Area_clean = case_when(
          grepl("30", area_vals) ~ "SD30",
          grepl("31", area_vals) ~ "SD31",
          TRUE ~ NA_character_
        )
      )
  } else if (length(rect_col) > 0 && !is.null(rect_lut)) {
    # Match via ICES rectangle lookup table
    rect_col_name <- rect_col[1]
    work <- work %>%
      left_join(
        rect_lut %>% 
          dplyr::select(ices_Data, Subdivision = sub_area) %>% 
          distinct(),
        by = setNames("ices_Data", rect_col_name)
      ) %>%
      mutate(
        Area_clean = case_when(
          grepl("30", as.character(Subdivision)) ~ "SD30",
          grepl("31", as.character(Subdivision)) ~ "SD31",
          TRUE ~ NA_character_
        )
      )
  } else {
    stop("No area or ICES rectangle column found to identify subdivisions 30 and 31 in sheet: ", sheet_label)
  }
  
  work %>%
    filter(!is.na(VE_ID), !is.na(Year), !is.na(Area_clean))
}

# Compute counts for SD30, SD31, and Total per Year
calc_counts <- function(df, sheet_name) {
  by_area <- df %>%
    group_by(Year, Area_clean) %>%
    summarise(n = n_distinct(VE_ID), .groups = "drop") %>%
    pivot_wider(names_from = Area_clean, values_from = n, values_fill = 0)
  
  if (!"SD30" %in% names(by_area)) by_area$SD30 <- 0
  if (!"SD31" %in% names(by_area)) by_area$SD31 <- 0
  
  total_area <- df %>%
    group_by(Year) %>%
    summarise(Total = n_distinct(VE_ID), .groups = "drop")
  
  full_join(by_area, total_area, by = "Year") %>%
    dplyr::select(Year, SD30, SD31, Total) %>%
    rename_with(~ paste0(., "_", sheet_name), -Year)
}

data1 <- prepare_trawler_data(df1, sheet1_name)
data2 <- prepare_trawler_data(df2, sheet2_name)

counts_s1 <- calc_counts(data1, sheet1_name)
counts_s2 <- calc_counts(data2, sheet2_name)

comparison <- full_join(counts_s1, counts_s2, by = "Year") %>%
  arrange(Year)

cat("================================================================================\n")
cat(" TRAWLER COUNT COMPARISON (SD30, SD31, AND TOTAL UNIQUE TRAWLERS)\n")
cat(" Gears:", paste(trawler_gears, collapse = ", "), "\n")
cat("================================================================================\n\n")

print(as.data.frame(comparison))





