# ============================================================
# 11_export_to_xlsx.R
# Purpose:
#   Export all result CSV files to one Excel workbook
#   Uses run_config.R so the correct run is exported
# ============================================================

rm(list = ls(all.names = TRUE))

library(openxlsx)

# -----------------------------
# 1. Load run settings
# -----------------------------
source("run_config.R")

diag_dir <- file.path(run_dir, "diagnostic_tables")
res_dir  <- file.path(run_dir, "result_tables")
pub_dir  <- file.path(run_dir, "publication_tables")
sel_dir  <- file.path(run_dir, "selected_conditions")

out_file <- file.path(run_dir, "all_tables_workbook.xlsx")

# -----------------------------
# 2. Collect CSV files safely
# -----------------------------
main_files <- c(
  file.path(run_dir, "results_condition_summary.csv"),
  file.path(run_dir, "results_replication_level.csv")
)

collect_csv <- function(folder) {
  if (!dir.exists(folder)) {
    return(character(0))
  }
  list.files(folder, pattern = "\\.csv$", full.names = TRUE)
}

diag_files <- collect_csv(diag_dir)
res_files  <- collect_csv(res_dir)
pub_files  <- collect_csv(pub_dir)
sel_files  <- collect_csv(sel_dir)

csv_files <- c(main_files, diag_files, res_files, pub_files, sel_files)
csv_files <- csv_files[file.exists(csv_files)]

if (length(csv_files) == 0) {
  stop("No CSV files found to export.")
}

# -----------------------------
# 3. Helper: safe Excel sheet names
# -----------------------------
make_sheet_name <- function(path, used_names = character(0)) {
  
  parent <- basename(dirname(path))
  file   <- tools::file_path_sans_ext(basename(path))
  
  if (parent == basename(run_dir)) {
    prefix <- "main"
  } else if (parent == "diagnostic_tables") {
    prefix <- "diag"
  } else if (parent == "result_tables") {
    prefix <- "res"
  } else if (parent == "publication_tables") {
    prefix <- "pub"
  } else if (parent == "selected_conditions") {
    prefix <- "sel"
  } else {
    prefix <- "sheet"
  }
  
  nm <- paste(prefix, file, sep = "_")
  
  # Excel does not allow these characters in sheet names
  nm <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm)
  
  # Excel sheet names can have max 31 characters
  nm <- substr(nm, 1, 31)
  
  base_nm <- nm
  k <- 1
  
  while (nm %in% used_names) {
    suffix <- paste0("_", k)
    nm <- paste0(substr(base_nm, 1, 31 - nchar(suffix)), suffix)
    k <- k + 1
  }
  
  nm
}

# -----------------------------
# 4. Helper: safe column widths
# -----------------------------
make_safe_widths <- function(dat,
                             max_rows = 1000,
                             min_width = 8,
                             max_width = 30) {
  
  if (ncol(dat) == 0) {
    return(numeric(0))
  }
  
  rows_to_check <- seq_len(min(nrow(dat), max_rows))
  
  widths <- vapply(seq_along(dat), function(j) {
    
    vals <- c(
      names(dat)[j],
      as.character(dat[rows_to_check, j])
    )
    
    vals <- vals[!is.na(vals)]
    
    width <- max(nchar(vals), na.rm = TRUE) + 2
    width <- max(width, min_width)
    width <- min(width, max_width)
    
    width
    
  }, numeric(1))
  
  widths
}

# -----------------------------
# 5. Workbook styles
# -----------------------------
wb <- createWorkbook()

header_style <- createStyle(
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  halign = "center",
  valign = "center",
  border = "bottom"
)

num_style <- createStyle(numFmt = "0.000")
int_style <- createStyle(numFmt = "0")

used_sheet_names <- character(0)

# -----------------------------
# 6. Add each CSV as one sheet
# -----------------------------
for (f in csv_files) {
  
  cat("Adding:", f, "\n")
  
  dat <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  
  sheet_name <- make_sheet_name(f, used_sheet_names)
  used_sheet_names <- c(used_sheet_names, sheet_name)
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = dat,
    withFilter = TRUE
  )
  
  if (ncol(dat) > 0) {
    
    # Header style
    addStyle(
      wb,
      sheet = sheet_name,
      style = header_style,
      rows = 1,
      cols = 1:ncol(dat),
      gridExpand = TRUE
    )
    
    # Freeze first row
    freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    
    # Safe column widths instead of widths = "auto"
    safe_widths <- make_safe_widths(dat)
    
    setColWidths(
      wb,
      sheet = sheet_name,
      cols = 1:ncol(dat),
      widths = safe_widths
    )
    
    # Numeric formatting
    if (nrow(dat) > 0) {
      
      for (j in seq_along(dat)) {
        
        x <- dat[[j]]
        
        if (is.numeric(x)) {
          
          if (all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)) {
            
            addStyle(
              wb,
              sheet = sheet_name,
              style = int_style,
              rows = 2:(nrow(dat) + 1),
              cols = j,
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else {
            
            addStyle(
              wb,
              sheet = sheet_name,
              style = num_style,
              rows = 2:(nrow(dat) + 1),
              cols = j,
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
}

# -----------------------------
# 7. Save workbook
# -----------------------------
saveWorkbook(wb, out_file, overwrite = TRUE)

cat("\nWorkbook saved to:\n")
cat(out_file, "\n\n")

cat("Included sheets:\n")
print(used_sheet_names)

cat("\nExport complete.\n")

