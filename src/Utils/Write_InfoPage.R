# Packages ---------

## --- Ensure required packages are installed ---
required_packages <- c("here", "jsonlite", "fs", "glue", "stringr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Restoring package:", pkg, " from renv lockfile."))
    renv::restore()
  }
}

library(here) # For creating file paths relative to the project root
library(jsonlite) # For reading and parsing JSON metadata files
library(fs) # For file system operations like creating paths and checking for file existence
library(glue) # For easy string interpolation
library(stringr) # For string manipulation, like extracting patterns

# Function to Write Meta Page for Quarto ---------
# This function generates a Quarto (.qmd) file that serves as a detailed information
# page for a given plot. The page includes metadata, a preview of the plot,
# and interactive tables of the underlying datasets.
write_info_page <- function(plot_obj, plot_id, volume, csv_suffix, plot_suffix = NULL, has_legend = TRUE) {
  # Ensure csv_suffix is a vector to handle single or multiple dataset files.
  # This allows the function to process one or more data sources for a single plot.
  if (length(csv_suffix) == 1) {
    csv_suffixes <- csv_suffix
  } else {
    csv_suffixes <- csv_suffix
  }

  # Define a nested function to process a single metadata file associated with a dataset.
  process_metadata <- function(suffix) {
    # --- Construct Dataset and Metadata File Paths ----
    # Build absolute paths to the data (.csv) and metadata (.json) files
    # using the project's directory structure.
    dataset_file <- here(
      "data", "clean",
      glue("Band{volume}"),
      glue("{plot_id}"),
      glue("{plot_id}_{suffix}_Data.csv")
    )

    metadata_file <- glue("{dataset_file}-metadata.json")

    ## --- Create Markdown Link for Path to Dataset ---
    # Generate a relative path for the dataset to use in the .qmd file.
    data_rel_path <- fs::path_rel(dataset_file, start = here("docs", "plots"))
    data_link <- glue("[{fs::path_rel(dataset_file, start = here())}](", data_rel_path, ")")

    ## --- Create Markdown Link for Path to JSON ---
    # Generate a relative path for the metadata file.
    meta_rel_path <- fs::path_rel(metadata_file, start = here("docs", "plots"))
    meta_link <- glue("[{fs::path_rel(metadata_file, start = here())}](", meta_rel_path, ")")

    # --- Extract Metadata from File ----
    # Read the JSON file and extract the schema information.
    meta <- fromJSON(metadata_file)

    # Extract specific metadata fields and create formatted links where applicable.
    fig_id <- meta$`dc:isPartOf`$object_id[[1]]
    fig_link <- glue("{fig_id} ([Research Data Platform](https://forschung.stadtgeschichtebasel.ch/items/{fig_id}.html))")

    publisher_name <- meta$`dc:publisher`$`schema:name`
    publisher_url <- meta$`dc:publisher`$`schema:url`$`@id`
    publisher_link <- sprintf(
      "%s <a href='%s' target='_blank'>![Wikidata](../../assets/img/wikidata_logo.svg){width=16}</a>",
      publisher_name, publisher_url
    )

    # individual authors are not parsed at the moment, listing SGB instead
    creators_str <- publisher_link

    # Process contributors list with ORCID
    contributors <- meta$`dc:contributor`
    if (!is.null(contributors)) {
      contributors_str <- sapply(seq_len(nrow(contributors)), function(i) {
        contributor <- contributors[i, ]
        name <- contributor$`schema:name`

        # extract ORCID for contributor if available
        orcid_id <- contributor$`schema:identifier`$`@id`

        if (!is.null(orcid_id) && !is.na(orcid_id)) {
          sprintf(
            "%s <a href='%s' target='_blank'>![ORCID](../../assets/img/ORCID-iD_icon_vector.svg){width=16}</a>",
            name, orcid_id
          )
        } else {
          name
        }
      }) |> paste(collapse = " / ")
    } else {
      contributors_str <- ""
    }

    # Create a clickable link for the license.
    license <- meta$`dc:license`[[1]]
    license_link <- glue("[{license}]({license})")

    ## --- Read Column Descriptions ---
    # Extract column names and their descriptions from the schema.
    columns_info <- meta$tableSchema$columns[c("name", "dc:description")]

    # Collapse the column info into a bullet point list md string for display.
    columns_str <- paste(apply(columns_info, 1, function(row) {
      paste0("- **", row["name"], ":** ", row["dc:description"])
    }), collapse = "\n")

    ## --- Map Volume Numbers to Open Access DOIs ---
    vol_text <- meta$`dc:isPartOf`$volume[[1]]
    vol_short <- str_extract(vol_text, "Stadt\\.Geschichte\\.Basel\\s*\\d+")
    vol_num <- as.integer(str_extract(vol_short, "\\d+"))

    # Predefined list of DOI suffixes for each volume.
    doi_suffixes <- c(
      "01-406352", "02-404936", "03-345800",
      "04-283636", "05-155353", "06-810743",
      "07-663402", "08-796384", "09-486500"
    )

    # If a valid volume number is found, create a DOI link.
    if (!is.na(vol_num) && vol_num >= 1 && vol_num <= length(doi_suffixes)) {
      vol_link <- glue("[Stadt.Geschichte.Basel {vol_num}](https://doi.org/10.21255/sgb-{doi_suffixes[vol_num]})")
      vol_text_link <- sub(vol_short, vol_link, vol_text, fixed = TRUE)
    }

    ## Parse meta$`dc:modified` and reformat for output
    date_modified <- meta$`dc:modified`[[1]] |>
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%S%z") |>
      format("%Y-%m-%d %H:%M:%S")

    # --- Create a list of metadata fields to appear in the metadata table ---
    fields <- list(
      Figure = fig_link,
      Title = meta$`dc:title`[[1]],
      Description = meta$`dc:description`[[1]],
      Creator = creators_str,
      Contributors = contributors_str,
      Publisher = publisher_link,
      Date = meta$`dc:date`[[1]],
      Coverage = meta$`dc:coverage`[[1]],
      "is Part of" = vol_text_link,
      Dataset = data_link,
      "Source (Dataset)" = meta$`dc:source`[[1]],
      "Metadata (Dataset)" = meta_link,
      "Citation (Dataset)" = meta$`dc:bibliographicCitation`[[1]],
      Rights = meta$`dc:rights`[[1]],
      License = license_link,
      Modified = date_modified
    )

    # Return all processed information for this dataset.
    list(
      fields = fields,
      vol_text = vol_text,
      vol_short = vol_short,
      col_description = columns_str
    )
  }

  ## Process all specified metadata files using the nested function.
  metadata_list <- lapply(csv_suffixes, process_metadata)

  ## Use the metadata from the first dataset for the main document properties (e.g., title).
  main_metadata <- metadata_list[[1]]

  # --- Infer plot object name and locate the corresponding plot script ----
  plot_name <- deparse(substitute(plot_obj)) # Get the variable name of the plot object as a string.
  base_name <- sub("^plot", "", plot_name) # Extract the base ID from the plot name.

  # The script now searches for the R script that generated the plot. This logic is
  # designed to handle different project structures (e.g., one plot per script vs. multiple).
  ## Prefer the explicit subplot script <base_name>_plot.R if it exists.
  candidate_script <- here("src", plot_id, paste0(base_name, "_plot.R"))

  if (fs::file_exists(candidate_script)) {
    plot_script <- candidate_script
  } else {
    # Fallback: try the generic <plot_id>_plot.R for single-script cases.
    generic_script <- here("src", plot_id, paste0(plot_id, "_plot.R"))
    if (fs::file_exists(generic_script)) {
      plot_script <- generic_script
    } else {
      # As a last resort, look for any file ending in '_plot.R' in the directory.
      candidates <- dir_ls(here("src", plot_id), regexp = "_plot\\.R$", type = "file")
      if (length(candidates) == 0) {
        stop("No plot script found in ", here("src", plot_id))
      }
      # Prefer a candidate script whose name contains the base_name.
      names_cand <- fs::path_file(candidates)
      idx <- which(str_detect(names_cand, fixed(base_name)))
      if (length(idx) == 1) {
        plot_script <- candidates[idx]
      } else {
        # If no specific match is found, default to the first candidate.
        plot_script <- candidates[1]
      }
    }
  }

  # --- Extract Plot Generation Code ---
  ## Read the plot script and extract the code needed to generate the plot object.
  ## This relies on a specific comment marker to know where to stop reading.
  script_lines <- readLines(plot_script, warn = FALSE)
  cutoff <- grep("^# Write Info Page", script_lines)
  if (length(cutoff) == 0) {
    stop(
      "No '# Write Info Page' marker found in ", plot_script,
      "\nThis marker is required to separate plot generation from info page writing."
    )
  }
  # Keep only the lines before the marker.
  first_section <- script_lines[seq_len(cutoff - 1)]

  ## --- Build Plot Chunk for Quarto ---
  # This creates the R code chunk that will display the plot in the .qmd file.
  # It includes an option to adjust the legend position.
  if (isTRUE(has_legend)) {
    plot_chunk <- c(
      "```{r plot_object}",
      "#| echo: false",
      "#| message: false",
      "#| warning: false",
      "#| column: page-right",
      "#| fig-cap: \"Preview only. Refer to the Research Data Platform for full metadata and production-ready files.\"",
      glue("{plot_name} + theme(legend.position = \"right\")"),
      "```"
    )
  } else {
    plot_chunk <- c(
      "```{r plot_object}",
      "#| echo: false",
      "#| message: false",
      "#| warning: false",
      "#| fig-cap: \"Preview only. Refer to the Research Data Platform for full metadata and production-ready files.\"",
      glue("{plot_name}"),
      "```"
    )
  }

  # --- Build Data and Metadata Table Chunks for All Datasets ----
  # Loop through each processed dataset to create corresponding datatables and metadata tables.
  table_chunks <- c()
  for (i in seq_along(metadata_list)) {
    fields <- metadata_list[[i]]$fields
    current_suffix <- csv_suffixes[i]

    # Dynamically set titles and chunk names to avoid conflicts if there are multiple datasets.
    data_table_title <- if (length(metadata_list) > 1) {
      glue("Dataset {plot_id}_{current_suffix} (Subset {i}/{length(metadata_list)})")
    } else {
      glue("Dataset {plot_id}_{current_suffix}")
    }
    datatable_chunk_name <- if (length(metadata_list) > 1) glue("datatable{i}") else "datatable"
    metadata_table_title <- glue("Selected Metadata for {data_table_title}")
    metadata_chunk_name <- if (length(metadata_list) > 1) glue("metatable{i}") else "metatable"
    data_var_name <- glue("data{plot_id}_{current_suffix}")

    ## --- Build Datatable Chunk ----
    # This chunk reads the CSV and displays it as an interactive DT::datatable.
    datatable_chunk <- c(
      glue("```{{r {datatable_chunk_name}}}"),
      "#| echo: false",
      "#| message: false",
      "#| warning: false",
      "",
      "library(readr)",
      "library(DT)",
      "",
      glue("# Read CSV file for {plot_id}_{current_suffix}"),
      glue("dataset_file_{i} <- here(\"data\", \"clean\", \"Band{volume}\", \"{plot_id}\", \"{plot_id}_{current_suffix}_Data.csv\")"),
      glue("{data_var_name} <- read_csv(dataset_file_{i})"),
      "",
      "# Wrap datatable in a custom div for styling.",
      "htmltools::div(",
      "  class = 'datatable-frame',",
      "  DT::datatable(",
      glue("    {data_var_name},"),
      "    rownames = FALSE,",
      "    options = list(scrollX = TRUE, scrollCollapse = TRUE, scrollY = '400px', paging = FALSE, dom = 't'),",
      "    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',",
      glue("'{data_table_title}'"),
      "    )",
      "  )",
      ")",
      "```",
      "",
      "::: {#callout-col-description .callout-tip title=\"Column Descriptions\" icon=\"false\" collapse=\"true\"}",
      glue("{metadata_list[[i]]$col_description}"),
      ":::"
    )

    ## --- Build Metadata Table Chunk ----
    # This chunk creates a markdown table of key-value metadata pairs.
    metatable_chunk <- c(
      glue("```{{r {metadata_chunk_name}, results=\"asis\"}}"),
      "#| echo: false",
      "#| message: false",
      "",
      "library(knitr)",
      "",
      "df <- data.frame(",
      "  Key = c(", paste0("    \"", names(fields), "\"", collapse = ",\n"), "  ),",
      "  Value = c(", paste0("    \"", unlist(fields), "\"", collapse = ",\n"), "  ),",
      "  stringsAsFactors = FALSE",
      ")",
      "",
      glue("knitr::kable(df, format = \"markdown\", escape = FALSE, caption = \"{metadata_table_title}\")"),
      "```",
      ""
    )

    # --- Combine Data and Metadata Chunks ----
    # Add horizontal rules between sections if there are multiple datasets.
    if (length(metadata_list) > 1) {
      if (i < length(metadata_list)) {
        table_chunks <- c(table_chunks, datatable_chunk, metatable_chunk, "", "---", "")
      } else {
        table_chunks <- c(table_chunks, datatable_chunk, metatable_chunk)
      }
    } else {
      table_chunks <- c(table_chunks, datatable_chunk, metatable_chunk)
    }
  }

  # --- Build the complete .qmd File Content ----
  # Define the plot ID for use in the YAML header.
  plotid_meta <- if (is.null(plot_suffix)) glue("abb{plot_id}") else glue("abb{plot_id}_{plot_suffix}")

  # Assemble the YAML front matter and the body of the Quarto document.
  qmd_text <- c(
    "---",
    glue("title: \"{main_metadata$fields$Title[[1]]}\""),
    "subtitle: Plot and Data Preview",
    glue("date-modified: {as.Date(main_metadata$fields$Modified[[1]])}"),
    glue("volume: \"{main_metadata$vol_text}\""),
    glue("vol_short: \"{main_metadata$vol_short}\""),
    glue("plotid: \"{plotid_meta}\""),
    "format:",
    "  html:",
    "    fig-width: 8",
    "    title-block-categories: false",
    "fig-cap-location: top",
    "code-links:",
    glue("  - href: 'https://github.com/Stadt-Geschichte-Basel/sgb-figures/tree/main/src/{plot_id}'"),
    glue("    text: abb{plot_id} Source Code"),
    "    icon: github",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "#| echo: false",
    "#| message: false",
    "# Source the extracted plot generation code to create the plot object.",
    paste(first_section, collapse = "\n"),
    "```",
    "",
    plot_chunk,
    "",
    glue("Plot {plotid_meta} was built using the following data:"),
    "",
    table_chunks
  )

  # --- Write the .qmd File to Disk ---
  outdir <- here("docs", "plots")
  dir_create(outdir) # Ensure the output directory exists.

  # Construct the final output file path.
  outfile <- if (is.null(plot_suffix)) {
    path(outdir, glue("{plot_id}.qmd"))
  } else {
    path(outdir, glue("{plot_id}_{plot_suffix}.qmd"))
  }

  # Write the generated content to the file.
  writeLines(qmd_text, outfile)
}
