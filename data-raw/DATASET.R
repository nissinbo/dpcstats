library(tidyverse)
library(rvest)
library(polite)
library(zipangu)
library(fs)
library(here)
library(tidyxl)
library(unpivotr)

# Base URL for MHLW DPC statistics
base_url <- "https://www.mhlw.go.jp/stf/shingi/shingi-chuo_128164.html"

# Extract year from meeting dates table
extract_year_table <- function(html_obj) {
  tryCatch({
    html_obj |>
      html_element("div.m-tableScroll > table.m-tableFlex") |>
      html_table(fill = TRUE) |>
      mutate(year = str_extract(議題等, ".*?年") |> convert_jyear()) |>
      select(year)
  }, error = function(e) {
    warning("Failed to extract meeting years: ", e$message)
    return(NULL)
  })
}

# Extract document links from the page
extract_document_links <- function(html_obj, base_url) {
  tryCatch({
    html_obj |>
      html_elements("td > ul.m-listLink > li > a.m-link") |>
      html_attr("href") |>
      url_absolute(base = base_url)
  }, error = function(e) {
    warning("Failed to extract document links: ", e$message)
    return(NULL)
  })
}

# Process DPC statistics Excel files from a given page
process_disease_excel_links <- function(year, page_url) {
  tryCatch({
    # Establish polite session
    session <- bow(page_url)
    page_content <- scrape(session)

    # Find Excel file links
    excel_files <- page_content |>
      html_nodes("a[data-icon='excel']")

    if (length(excel_files) > 0) {
      # Create dataset of all Excel files
      excel_dataset <- tibble(
        year = year,
        filename = excel_files |> html_text(trim = TRUE),
        file_url = excel_files |> html_attr("href") |> url_absolute(base = page_url)
      )

      # Define target DPC statistics patterns
      target_keywords <- c(
        "（８）疾患別手術別集計_MDC",
        "（９）疾患別手術有無別処置1有無別集計_MDC",
        "（１０）疾患別手術有無別処置2有無別集計_MDC"
      )

      # Filter and clean DPC statistics files
      filtered_links <- excel_dataset |>
        filter(str_detect(filename, str_c(target_keywords, collapse = "|"))) |>
        mutate(
          filename = str_remove_all(filename, "［.*?］"),
          filename = str_remove_all(filename, "（.*?）"),
          filename = str_trim(filename)
        ) |>
        separate(filename,
                 into = c("report_category", "mdc_group"),
                 sep = "_",
                 remove = TRUE) |>
        select(year, report_category, mdc_group, file_url)

      return(filtered_links)
    } else {
      return(tibble(
        year = character(),
        report_category = character(),
        mdc_group = character(),
        file_url = character()
      ))
    }
  }, error = function(e) {
    warning("Failed to process page ", page_url, ": ", e$message)
    return(tibble(
      year = character(),
      report_category = character(),
      mdc_group = character(),
      file_url = character()
    ))
  })
}

# Main data collection function
collect_dpc_data <- function(base_url) {
  tryCatch({
    # Initialize session
    session <- bow(base_url)
    page_content <- scrape(session)

    # Get meeting years
    years_data <- extract_year_table(page_content)
    if (is.null(years_data)) {
      stop("Failed to extract meeting years")
    }

    # Get document links
    doc_links <- extract_document_links(page_content, base_url)
    if (is.null(doc_links)) {
      stop("Failed to extract document links")
    }

    # Combine years and links
    combined_data <- years_data |>
      mutate(page_url = doc_links)

    # Process all pages and remove duplicates
    results <- combined_data |>
      pmap_dfr(process_disease_excel_links) |>
      distinct()

    return(results)

  }, error = function(e) {
    message("Error in data collection: ", e$message)
    return(NULL)
  })
}

# Download DPC statistics files
download_dpc_file <- function(file_url, year, report_category, mdc_group, target_dir) {
  # Polite delay between downloads
  Sys.sleep(1)

  # Generate standardized filename
  output_filename <- path(target_dir,
                          str_c(
                            case_match(report_category,
                                       "疾患別手術別集計" ~ "disease_surgery",
                                       "疾患別手術有無別処置1有無別集計" ~ "disease_surgery_proc1",
                                       "疾患別手術有無別処置2有無別集計" ~ "disease_surgery_proc2"),
                            mdc_group, year, sep = "_"),  ext = "xlsx")

  # Safe download with error handling
  safely_download <- safely(download.file)
  result <- safely_download(file_url, output_filename, mode = "wb")

  if (is.null(result$error)) {
    cat(sprintf("Successfully downloaded: %s\n", output_filename))
  } else {
    cat(sprintf("Download failed for %s: %s\n", output_filename, result$error))
  }

  return(output_filename)
}

# Process and save DPC statistics data
process_dpc_stats <- function(file_name, output_dir = "data") {
  # Define facility columns
  facility_columns <- c("告示番号", "通番", "施設名")

  # Remove file extension for output name
  output_name <- str_remove(file_name, ".xlsx")

  # Extract and transform data
  raw_data <- xlsx_cells(here("data-raw", "mdc_files", file_name)) |>
    behead("up-left", "disease_code") |>
    behead("up", "disease_name") |>
    behead("up", "unit") |>
    behead("up", "category") |>
    mutate(value = coalesce(as.character(numeric), character)) |>
    select(row, disease_code, disease_name, unit, category, value)

  # Process facility information
  facility_info <- raw_data |>
    filter(category %in% facility_columns) |>
    select(category, value, row) |>
    pivot_wider(names_from = category, values_from = value, id_cols = row)

  # Process DPC statistics
  dpc_stats <- raw_data |>
    filter(!category %in% facility_columns) |>
    fill(disease_code, disease_name, unit) |>
    select(disease_code, disease_name, unit, category, value, row)

  # Combine facility info with DPC stats
  combined_stats <- facility_info |>
    inner_join(dpc_stats, "row") |>
    select(施設名, 告示番号, 通番, disease_code, disease_name, unit, category, value)

  # Assign to variable and save
  assign(output_name, combined_stats)
  save_path <- path(output_dir, str_c(output_name, ".rda"))
  save(list = output_name, file = save_path, compress = "xz")
}

# Create directory for downloaded files
dir_create(here("data-raw", "mdc_files"))
dir_create(here("data"))

# Execute main data collection
results <- collect_dpc_data(base_url)

# Download and process all files
downloaded_files <- pmap(
  list(
    results$file_url,
    results$year,
    results$report_category,
    results$mdc_group,
    here("data-raw", "mdc_files")
  ),
  download_dpc_file
)

# Process surgery-related DPC statistics files
dir_ls(here("data-raw", "mdc_files")) |>
  map_chr(~str_remove(basename(.x), ".xlsx")) |>
  str_subset("disease_surgery_MDC") |>
  iwalk(~process_dpc_stats(str_c(.x, ".xlsx")))
