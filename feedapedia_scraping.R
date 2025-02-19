# --- 0) Load libraries and functions ----
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

p_load(rvest, tidyverse, data.table, future, future.apply, progressr, parallel, httr, arrow)

# Function to check if a Feedipedia node exists; returns the URL if exists, NULL otherwise.
check_node_exists <- function(node_number, timeout_seconds = 60) {
  url <- sprintf("https://www.feedipedia.org/node/%d", node_number)
  response <- GET(url, timeout(timeout_seconds))
  if (status_code(response) == 200) return(url)
  return(NULL)
}

# --- 1) Set save directory ---
save_dir <- file.path(getwd(), "feedipedia")
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# --- 2) Set parameters ---
nodes <- sample(3:30000, replace = FALSE)
timeout_seconds <- 60
worker_n <- parallel::detectCores() - 1

# Precompile unwanted titles regex (use escapes where necessary)
unwanted_titles <- c(
  "Project leaders", "Associate organizations", "Beef cattle", "Project governance",
  "Editing and administration", "Scientific authors and data providers",
  "Scientific advisors", "Other contributors", "Explore Feedipedia", "Sponsors",
  "Other supporting organizations", "Get involved with Feedipedia", "Pigs",
  "Poultry", "Fish\\s*", "Pesticides and contaminants", "Spines", 
  "Abstract", "Introduction", "Feed additive strategies", "References", "Résumé",
  "Direct effects of climate change on livestock", "Indirect effects of climate change on livestock",
  "Concept of multiple stressor impacts on livestock", "Impact of climate change on livestock production",
  "Impact of climate change on livestock reproduction", "Impact of climate change on livestock adaptation", 
  "Impact of climate change on livestock diseases", "Conclusion",                                
  "Future perspectives", "Forage", "Seeds", "Enterprise driven", "Collective investment driven",
  "Farm organization", "Personnel management", "Management system", "Production management", "Profit",
  "Dividend mode", "Programme", "Speakers", "2\\.1 Feed Technology Advancements in Tunisia",
  "2\\.2 A case of pellet feed use in Tunisia", "Ease of collecting raw data", "Signs of ergot on plants",
  "Ergot alkaloids", "Ergotism symptoms in animals", "Treatments", "Legislation", "Further reading",
  "Description", "Different categories of organic trace minerals",
  "Process of preparing of amino acid chelates of trace minerals",
  "Benefits of using organic trace minerals for livestock production", "Testing of chelated minerals",
  "Molecular Size determination of chelated minerals",
  "Test for solubility and structural integrity of complexed and chelated trace minerals",
  "Conclusions and applications"
)
unwanted_pattern <- paste0("^(", paste(unwanted_titles, collapse = "|"), ")$")

# --- 3) Define function to process a single node with detailed progress ---
process_node <- function(node, save_dir, timeout_seconds) {
  save_file <- file.path(save_dir, paste0(node, ".csv"))
  
  # If file already exists, load and return its data (if valid)
  if (file.exists(save_file)) {
    dat <- suppressWarnings(fread(save_file))
    if ("status" %in% names(dat)) return(NULL)
    dat[, page_node := node]
    return(dat)
  }
  
  # Create a local progressor for detailed tracking within this node.
  local_progress <- progressr::progressor(steps = 8)
  local_progress(sprintf("Node %d: Starting processing", node))
  
  # Step 1: Check if node exists
  local_progress(sprintf("Node %d: Checking existence", node))
  node_url <- check_node_exists(node, timeout_seconds)
  if (is.null(node_url)) {
    fwrite(data.table(status = "does not exist"), save_file)
    return(NULL)
  }
  local_progress(sprintf("Node %d: Node exists. URL retrieved.", node))
  
  # Step 2: Download HTML content
  local_progress(sprintf("Node %d: Downloading HTML content", node))
  content <- tryCatch({
    read_html(node_url)
  }, error = function(e) {
    fwrite(data.table(status = paste("Error reading HTML:", e$message)), save_file)
    return(NULL)
  })
  if (is.null(content)) return(NULL)
  
  # Step 3: Extract page title
  local_progress(sprintf("Node %d: Extracting page title", node))
  page_title <- content %>% 
    html_node("title") %>% 
    html_text() %>% 
    { trimws(strsplit(., "[|]")[[1]][1]) }
  
  # Step 4: Extract and filter table titles
  local_progress(sprintf("Node %d: Extracting table titles", node))
  titles <- content %>% html_nodes("h3") %>% html_text()
  if (length(titles) < 7) {
    fwrite(data.table(status = paste(page_title, "- no nutritional table")), save_file)
    return(NULL)
  }
  titles <- titles[-(1:6)] %>% gsub("\u00A0", " ", .) %>% .[!grepl("\n\t", .)]
  
  local_progress(sprintf("Node %d: Filtering unwanted table titles", node))
  titles <- titles[!grepl(unwanted_pattern, titles)]
  if (length(titles) == 0) {
    fwrite(data.table(status = paste(page_title, "- no nutritional table")), save_file)
    return(NULL)
  }
  
  # Step 5: Extract Feedipedia node ids from <h3> links
  f_nodes <- content %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") %>% 
    { str_extract(., "[0-9]+$") }
  
  # Step 6: Extract all tables (with fill = TRUE)
  local_progress(sprintf("Node %d: Extracting tables", node))
  tables_list <- content %>% html_table(fill = TRUE)
  
  # Determine if data are from FAO or Bo Gohl (for reference)
  is_fao <- any(sapply(tables_list, function(x) {
    if (nrow(x) > 0) grepl("IMPORTANT INFORMATION: ", x[1, 1]) else FALSE
  }))
  
  # Keep only nutritional composition tables based on first row content
  valid_titles <- c("Main analysis", "Amino acids", "Ruminant nutritive values", "Pig nutritive values")
  keep_tabs <- sapply(tables_list, function(x) {
    if (nrow(x) > 0) x[1, 1] %in% valid_titles else FALSE
  })
  tables_list <- tables_list[keep_tabs]
  
  # Remove empty tables; adjust titles and node ids accordingly
  # FIX: Use unlist() to ensure we get a logical vector.
  non_zero <- unlist(sapply(tables_list, function(x) nrow(x) > 0 && ncol(x) > 0))
  tables_list <- tables_list[non_zero]
  titles <- titles[non_zero]
  f_nodes <- f_nodes[non_zero]
  
  if (length(tables_list) == 0) {
    fwrite(data.table(status = paste(page_title, "- no nutritional table after filtering")), save_file)
    return(NULL)
  }
  
  # Step 7: Process each table with detailed progress updates
  local_progress(sprintf("Node %d: Processing %d table(s)", node, length(tables_list)))
  processed_tables <- lapply(seq_along(tables_list), function(i) {
    local_progress(sprintf("Node %d: Processing table %d/%d", node, i, length(tables_list)))
    tb <- tables_list[[i]]
    setnames(tb, as.character(tb[1, ]))  # Use first row as header
    tb <- tb[-1, , drop = FALSE]
    tb <- data.table(tb)
    
    if ("Unit" %in% names(tb)) tb <- tb[Unit != "" & Unit != "Unit"]
    if (ncol(tb) >= 8) tb <- tb[, -8, with = FALSE]
    
    num_cols <- c("Avg", "SD", "Min", "Max", "Nb")
    for (col in num_cols) {
      if (col %in% names(tb)) tb[, (col) := as.numeric(get(col))]
    }
    
    tb[, `:=`(Diet.Item = titles[i],
              feedipedia_node = f_nodes[i],
              page_title = page_title,
              page_node = node)]
    if (names(tb)[1] != "Variable") setnames(tb, 1, "Variable")
    tb
  })
  
  # Step 8: Finalize processing and save data
  local_progress(sprintf("Node %d: Finalizing and saving data", node))
  dat <- rbindlist(processed_tables, fill = TRUE)
  dat[, is_fao_or_gohl := is_fao]
  fwrite(dat, save_file)
  local_progress(sprintf("Node %d: Completed", node))
  
  dat
}

# --- 4) Process nodes in parallel with detailed progress reporting ---
plan("multisession", workers = worker_n)
handlers("txtprogressbar")

f_tabs <- with_progress({
  # Create the global progressor inside the with_progress block.
  p <- progressr::progressor(along = nodes)
  
  future_lapply(nodes, function(j) {
    p(sprintf("Global: Starting node %d", j))
    process_node(j, save_dir, timeout_seconds)
  })
})

plan("sequential")  # Reset the future plan to sequential after processing

# --- 5) Merge and save downloaded data ---
f_data <- rbindlist(f_tabs, fill = TRUE)
arrow::write_parquet(f_data, file.path(save_dir, "feedipedia.parquet"))

