# ============================================================
# 11_export_to_xlsx.R
# Purpose:
#   Export all result CSV files to one Excel workbook
# ============================================================

rm(list = ls(all.names = TRUE))

# install.packages("openxlsx")  # run once if needed
library(openxlsx)

# -----------------------------
# 1. Set run folder
# -----------------------------
run_dir <- file.path("runs", "run_2026-04-26_00-55_nrep400")


diag_dir <- file.path(run_dir, "diagnostic_tables")
res_dir  <- file.path(run_dir, "result_tables")
pub_dir  <- file.path(run_dir, "publication_tables")
sel_dir  <- file.path(run_dir, "selected_conditions")

# -----------------------------
# 2. Output workbook path
# -----------------------------
out_file <- file.path(run_dir, "all_tables_workbook.xlsx")

# -----------------------------
# 3. Collect CSV files
# -----------------------------
main_files <- c(
  file.path(run_dir, "results_condition_summary.csv"),
  file.path(run_dir, "results_replication_level.csv")
)

diag_files <- list.files(diag_dir, pattern = "\\.csv$", full.names = TRUE)
res_files  <- list.files(res_dir,  pattern = "\\.csv$", full.names = TRUE)
pub_files  <- list.files(pub_dir,  pattern = "\\.csv$", full.names = TRUE)
sel_files  <- list.files(sel_dir,  pattern = "\\.csv$", full.names = TRUE)

csv_files <- c(main_files, diag_files, res_files, pub_files, sel_files)
csv_files <- csv_files[file.exists(csv_files)]

if (length(csv_files) == 0) {
  stop("No CSV files found.")
}

# -----------------------------
# 4. Helper: sheet names
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
  nm <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm)
  nm <- substr(nm, 1, 31)
  
  base_nm <- nm
  k <- 1
  while (nm %in% used_names) {
    suffix <- paste0("_", k)
    nm <- substr(base_nm, 1, 31 - nchar(suffix))
    nm <- paste0(nm, suffix)
    k <- k + 1
  }
  
  nm
}

# -----------------------------
# 5. Workbook + styles
# -----------------------------
wb <- createWorkbook()

header_style <- createStyle(
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  halign = "center",
  border = "bottom"
)

num_style <- createStyle(numFmt = "0.000")
int_style <- createStyle(numFmt = "0")

used_sheet_names <- character(0)

# -----------------------------
# 6. Add each CSV as one sheet
# -----------------------------
for (f in csv_files) {
  
  dat <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  
  sheet_name <- make_sheet_name(f, used_sheet_names)
  used_sheet_names <- c(used_sheet_names, sheet_name)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = dat, withFilter = TRUE)
  
  addStyle(
    wb, sheet = sheet_name, style = header_style,
    rows = 1, cols = 1:ncol(dat), gridExpand = TRUE
  )
  
  freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dat), widths = "auto")
  
  if (nrow(dat) > 0) {
    for (j in seq_along(dat)) {
      x <- dat[[j]]
      if (is.numeric(x)) {
        if (all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)) {
          addStyle(
            wb, sheet = sheet_name, style = int_style,
            rows = 2:(nrow(dat) + 1), cols = j,
            gridExpand = TRUE, stack = TRUE
          )
        } else {
          addStyle(
            wb, sheet = sheet_name, style = num_style,
            rows = 2:(nrow(dat) + 1), cols = j,
            gridExpand = TRUE, stack = TRUE
          )
        }
      }
    }
  }
}

# -----------------------------
# 7. Save workbook
# -----------------------------
saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Workbook saved to:\n")
cat(out_file, "\n\n")

cat("Included sheets:\n")
print(used_sheet_names)
