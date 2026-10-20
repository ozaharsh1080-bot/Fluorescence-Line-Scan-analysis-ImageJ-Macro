# ==========================================================================================
# BIOLOGICAL LINE Intensity Plot Profile ANALYSIS SCRIPT (R) - Colocalization Edition
# ==========================================================================================
# Developed using Vibe coding and tested by Harsh Oza
# This script analyzes fluorescence intensity linescan intensity plot profile from confocal images.
# to measure Profile Colocalization (Pearson's r) between channels, generate 
# raw and normalized intensity plots, and compile an automated Quarto report.
# ==========================================================================================

# 0. AUTOMATIC PACKAGE CHECK & INSTALLATION
required_packages <- c("tidyverse", "openxlsx", "quarto")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  message("The following required packages are missing: ", paste(missing_packages, collapse = ", "))
  install_choice <- readline(prompt = "Would you like to install them now? (y/n): ")
  
  if (tolower(install_choice) == "y") {
    install.packages(missing_packages)
  } else {
    stop("Required packages must be installed to run this script.")
  }
}

library(tcltk)
library(tidyverse)
library(openxlsx)

# Full list of ggplot2 supported formats
image_formats <- c("png", "pdf", "svg", "eps", "tiff", "jpeg", "bmp", "ps")

master_coloc_list <<- list()

# =========================================================================
# 1. GUI SETUP 
# =========================================================================
get_initial_options <- function(csv_file_path, channel_names) {
  first_csv <- read_csv(csv_file_path, show_col_types = FALSE)
  available_columns <- colnames(first_csv)
  
  color_options <- c("black", "red", "green", "blue", "yellow", "purple", "orange", "pink", "cyan", "brown", "gray", "magenta")
  font_families <- c("Helvetica", "Arial", "Times New Roman")
  
  tt <- tktoplevel()
  tkwm.title(tt, "Plot & Colocalization Analysis Options")
  tkwm.geometry(tt, "600x550") # Set max size so scrollbars engage on small screens
  
  # --- Canvas and Scrollbar Setup ---
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...) tkyview(canvas,...))
  canvas <- tkcanvas(tt, yscrollcommand=function(...) tkset(scr,...))
  tkpack(canvas, side="left", fill="both", expand=TRUE)
  tkpack(scr, side="right", fill="y")
  
  parent_frame <- tkframe(canvas)
  tkcreate(canvas, "window", 0, 0, anchor="nw", window=parent_frame)
  
  tkbind(parent_frame, "<Configure>", function() {
    bbox <- tclvalue(tkbbox(canvas, "all"))
    tkconfigure(canvas, scrollregion=bbox)
  })
  
  # Mouse Scroll Support
  tkbind(canvas, "<MouseWheel>", function(D) { tkyview(canvas, "scroll", as.integer(-as.numeric(D)/120), "units") })
  tkbind(canvas, "<Button-4>", function() tkyview(canvas, "scroll", -1, "units"))
  tkbind(canvas, "<Button-5>", function() tkyview(canvas, "scroll", 1, "units"))
  
  vars_color <- list(); vars_width <- list()
  for(i in seq_along(channel_names)) {
    tkgrid(tklabel(parent_frame, text = paste("Channel:", channel_names[i])), row = i-1, column = 0, padx = 5, pady = 2, sticky = "w")
    vars_color[[i]] <- tclVar(color_options[1])
    tkgrid(ttkcombobox(parent_frame, values = color_options, textvariable = vars_color[[i]], state = "readonly", width=10), row = i-1, column = 1, padx=5, pady=2, sticky="w")
    tkgrid(tklabel(parent_frame, text="Line width:"), row=i-1, column=2, padx=5, pady=2, sticky="w")
    vars_width[[i]] <- tclVar("1")
    tkgrid(tkentry(parent_frame, textvariable=vars_width[[i]], width=5), row=i-1, column=3, padx=5, pady=2, sticky="w")
  }
  
  base_row <- length(channel_names)
  tkgrid(tklabel(parent_frame, text="X-axis col:"), row=base_row, column=0, padx=5, pady=2, sticky="w")
  xaxis_var <- tclVar(available_columns[1])
  tkgrid(ttkcombobox(parent_frame, values = available_columns, textvariable = xaxis_var, state = "readonly", width=20), row=base_row, column=1, columnspan=2, padx=5, pady=2, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Y-axis col:"), row=base_row+1, column=0, padx=5, pady=2, sticky="w")
  yaxis_var <- tclVar(available_columns[ifelse(length(available_columns) > 1, 2, 1)])
  tkgrid(ttkcombobox(parent_frame, values = available_columns, textvariable = yaxis_var, state = "readonly", width=20), row=base_row+1, column=1, columnspan=2, padx=5, pady=2, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Legend font:"), row=base_row+2, column=0, padx=5, pady=2, sticky="w")
  legend_font_var <- tclVar("12")
  tkgrid(ttkcombobox(parent_frame, values=c("10", "12", "14", "16", "18"), textvariable=legend_font_var, state="readonly", width=5), row=base_row+2, column=1, padx=5, pady=2, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Font family:"), row=base_row+3, column=0, padx=5, pady=2, sticky="w")
  font_family_var <- tclVar("Helvetica")
  tkgrid(ttkcombobox(parent_frame, values=font_families, textvariable=font_family_var, state="readonly", width=10), row=base_row+3, column=1, padx=5, pady=2, sticky="w")
  
  grid_var <- tclVar(1)
  tkcheckbutton(parent_frame, text="Show grid", variable=grid_var) %>% tkgrid(row=base_row+4, column=0, columnspan=2, padx=5, pady=2, sticky="w")
  normalize_var <- tclVar(0)
  tkcheckbutton(parent_frame, text="Normalize main graph", variable=normalize_var) %>% tkgrid(row=base_row+5, column=0, columnspan=2, padx=5, pady=2, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="--- Global Analysis Settings ---"), row=base_row+6, column=0, columnspan=3, pady=10, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Select Channels for Analysis:"), row=base_row+7, column=0, padx=5, pady=2, sticky="w")
  analysis_lb <- tklistbox(parent_frame, selectmode="multiple", height=length(channel_names), exportselection=FALSE)
  sapply(seq_along(channel_names), function(i) tkinsert(analysis_lb, "end", channel_names[i]))
  for(i in seq_along(channel_names)) tcl(analysis_lb, "selection", "set", i-1) 
  tkgrid(analysis_lb, row=base_row+7, column=1, columnspan=2, padx=5, pady=2, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Select Analyses to Run:"), row=base_row+8, column=0, padx=5, pady=5, sticky="w")
  do_coloc <- tclVar(1)
  tkcheckbutton(parent_frame, text="Profile Colocalization (Pearson's r)", variable=do_coloc) %>% tkgrid(row=base_row+9, column=0, columnspan=2, padx=15, sticky="w")
  
  tkgrid(tklabel(parent_frame, text="Image Export Formats:"), row=base_row+10, column=0, padx=5, pady=5, sticky="w")
  format_lb <- tklistbox(parent_frame, selectmode="multiple", height=length(image_formats), exportselection=FALSE)
  sapply(image_formats, function(i) tkinsert(format_lb, "end", i))
  tcl(format_lb, "selection", "set", 0) 
  tcl(format_lb, "selection", "set", 1) 
  tkgrid(format_lb, row=base_row+10, column=1, columnspan=2, padx=5, pady=5, sticky="w")
  
  do_master <- tclVar(1)
  tkcheckbutton(parent_frame, text="Generate Master Summary CSVs", variable=do_master) %>% tkgrid(row=base_row+11, column=0, columnspan=3, padx=5, pady=10, sticky="w")
  
  result_var <- tclVar("")
  selected_analysis_channels <- character(0)
  selected_formats <- character(0)
  final_ch_suffix <- character(0)
  
  on_ok <- function() {
    selected_analysis_channels <<- channel_names[as.integer(tkcurselection(analysis_lb)) + 1]
    selected_formats <<- image_formats[as.integer(tkcurselection(format_lb)) + 1]
    if (length(selected_formats) == 0) selected_formats <<- c("png", "pdf") 
    
    # Generate the safe channel suffix based on selected channels
    raw_suffix <- paste(selected_analysis_channels, collapse="_vs_")
    final_ch_suffix <<- gsub("[^A-Za-z0-9_-]", "_", raw_suffix) # Sanitizes spaces to underscores
    
    tclvalue(result_var) <- "ok"
    tkdestroy(tt)
  }
  
  tkbutton(parent_frame, text="OK", command=on_ok) %>% tkgrid(row=base_row+12, column=0, columnspan=4, pady=10)
  tkfocus(tt); tkwait.variable(result_var)
  
  list(
    colors = setNames(sapply(vars_color, tclvalue), channel_names), 
    widths = setNames(as.numeric(sapply(vars_width, tclvalue)), channel_names),
    xaxis_col = tclvalue(xaxis_var), 
    yaxis_col = tclvalue(yaxis_var),
    xlab = tclvalue(xaxis_var), 
    ylab = tclvalue(yaxis_var),
    legend_font_size = as.numeric(tclvalue(legend_font_var)), 
    font_family = tclvalue(font_family_var),
    show_grid = as.logical(as.integer(tclvalue(grid_var))), 
    normalize = as.logical(as.integer(tclvalue(normalize_var))),
    analysis_channels = selected_analysis_channels,
    ch_suffix = final_ch_suffix, 
    do_coloc = as.logical(as.integer(tclvalue(do_coloc))),
    do_master = as.logical(as.integer(tclvalue(do_master))),
    export_formats = selected_formats
  )
}

smooth_channel_data <- function(df, x_col, y_col, n_points = 500) {
  x_seq <- seq(min(df[[x_col]]), max(df[[x_col]]), length.out = n_points)
  y_smooth <- spline(df[[x_col]], df[[y_col]], xout = x_seq)$y
  result <- tibble(x = x_seq, y = y_smooth, Channel = df$Channel[1])
  colnames(result) <- c(x_col, y_col, "Channel")
  result
}

# =========================================================================
# 2. MAIN PROCESSING FUNCTION
# =========================================================================
process_folder <- function(folder_path, params = NULL, first_in_subtree = FALSE) {
  cat("\nProcessing folder:", folder_path, "\n")
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    for(sf in list.dirs(folder_path, recursive = FALSE)) { 
      res <- process_folder(sf, params, first_in_subtree)
      params <- res$params 
    }
    return(list(params = params, first_in_subtree = first_in_subtree))
  }
  
  channel_names <- tools::file_path_sans_ext(basename(csv_files))
  if (is.null(params) && !first_in_subtree) { 
    params <- get_initial_options(csv_files[1], channel_names)
    first_in_subtree <- TRUE 
    master_coloc_list <<- list()
  }
  
  distance_df <- read_csv(csv_files[1], show_col_types = FALSE) %>% select(all_of(params$xaxis_col))
  gray_values <- lapply(csv_files, function(f) {
    d <- read_csv(f, show_col_types = FALSE) %>% select(all_of(params$yaxis_col))
    colnames(d) <- tools::file_path_sans_ext(basename(f)); d
  })
  merged_data <- bind_cols(distance_df, bind_cols(gray_values))
  
  raw_wide <- merged_data
  colnames(raw_wide)[1] <- "Distance"
  colnames(raw_wide)[-1] <- paste0("Gray value (", channel_names, ")")
  
  # --- MAIN GRAPH & COLOCALIZATION ---
  
  # ALWAYS Export Combined Raw Dataset CSV
  write.csv(raw_wide, file.path(folder_path, paste0("Combined_Raw_Data_", params$ch_suffix, ".csv")), row.names = FALSE)
  
  # 1. ALWAYS generate the Raw Main Plot
  plot_data_raw <- merged_data %>% pivot_longer(cols = -all_of(params$xaxis_col), names_to = "Channel", values_to = "Gray_Value")
  plot_data_smooth_raw <- plot_data_raw %>% group_by(Channel) %>% group_modify(~smooth_channel_data(.x, params$xaxis_col, "Gray_Value")) %>% ungroup()
  
  p_main_raw <- ggplot(plot_data_smooth_raw, aes(x = .data[[params$xaxis_col]], y = Gray_Value, color = Channel)) +
    geom_line(aes(size = Channel), lineend = "round") + 
    scale_color_manual(values = params$colors) + 
    scale_size_manual(values = params$widths) +
    labs(title = "Line Scan (Original Raw)", x = params$xlab, y = params$ylab, color = "Channel", size = "Channel") +
    theme_minimal(base_family = params$font_family) + 
    theme(
      legend.title = element_text(size = params$legend_font_size), 
      legend.text = element_text(size = params$legend_font_size), 
      panel.grid.major = if(params$show_grid) element_line() else element_blank(), 
      axis.line = element_line(color = "black")
    )
  
  coloc_text <- ""
  if (params$do_coloc && length(params$analysis_channels) > 1) {
    pairs <- combn(params$analysis_channels, 2)
    coloc_text <- "Profile Colocalization (r):\n"
    local_coloc_df <- data.frame() 
    
    for(p in 1:ncol(pairs)) {
      c1 <- pairs[1, p]; c2 <- pairs[2, p]
      r_val <- cor(raw_wide[[paste0("Gray value (", c1, ")")]], raw_wide[[paste0("Gray value (", c2, ")")]], method="pearson")
      coloc_text <- paste0(coloc_text, c1, " vs ", c2, " = ", round(r_val, 3), "\n")
      
      master_coloc_list[[length(master_coloc_list)+1]] <<- data.frame(
        ROI_Folder = basename(folder_path), 
        Channel_1 = c1, 
        Channel_2 = c2, 
        Pearson_r = r_val
      )
      local_coloc_df <- rbind(local_coloc_df, data.frame(Channel_1 = c1, Channel_2 = c2, Pearson_r = r_val)) 
    }
    write.csv(local_coloc_df, file.path(folder_path, paste0("Colocalization_Results_", params$ch_suffix, ".csv")), row.names = FALSE) 
  }
  
  # Apply coloc text to Raw Graph if available
  if (coloc_text != "") {
    x_pos_raw <- min(plot_data_smooth_raw[[params$xaxis_col]], na.rm=TRUE)
    y_pos_raw <- max(plot_data_smooth_raw$Gray_Value, na.rm=TRUE)
    p_main_raw <- p_main_raw + annotate("text", x = x_pos_raw, y = y_pos_raw, label = coloc_text, hjust = 0, vjust = 1, size = 4, fontface="bold")
  }
  
  # Save Raw Plot
  for(fmt in params$export_formats) {
    ggsave(file.path(folder_path, paste0("LineScanPlot_Main_Raw_", params$ch_suffix, ".", fmt)), plot = p_main_raw, device = fmt, width = 8, height = 6, dpi = 300)
  }
  
  # 2. Add Normalized Plot if explicitly requested by GUI toggle
  if (params$normalize) {
    # Smooth first, then normalize so the plotted curve strictly spans 0.0 to 1.0
    plot_data_smooth_norm <- plot_data_raw %>% 
      group_by(Channel) %>% 
      group_modify(~smooth_channel_data(.x, params$xaxis_col, "Gray_Value")) %>% 
      mutate(Gray_Value = if (max(Gray_Value, na.rm = TRUE) > min(Gray_Value, na.rm = TRUE)) {
        (Gray_Value - min(Gray_Value, na.rm = TRUE)) / (max(Gray_Value, na.rm = TRUE) - min(Gray_Value, na.rm = TRUE))
      } else {
        0
      }) %>% 
      ungroup()
    
    p_main_norm <- ggplot(plot_data_smooth_norm, aes(x = .data[[params$xaxis_col]], y = Gray_Value, color = Channel)) +
      geom_line(aes(size = Channel), lineend = "round") + 
      scale_color_manual(values = params$colors) + 
      scale_size_manual(values = params$widths) +
      labs(title = "Line Scan (Normalized)", x = params$xlab, y = "Normalized Intensity", color = "Channel", size = "Channel") +
      theme_minimal(base_family = params$font_family) + 
      theme(
        legend.title = element_text(size = params$legend_font_size), 
        legend.text = element_text(size = params$legend_font_size), 
        panel.grid.major = if(params$show_grid) element_line() else element_blank(), 
        axis.line = element_line(color = "black")
      )
    
    if (coloc_text != "") {
      x_pos_norm <- min(plot_data_smooth_norm[[params$xaxis_col]], na.rm=TRUE)
      y_pos_norm <- max(plot_data_smooth_norm$Gray_Value, na.rm=TRUE)
      p_main_norm <- p_main_norm + annotate("text", x = x_pos_norm, y = y_pos_norm, label = coloc_text, hjust = 0, vjust = 1, size = 4, fontface="bold")
    }
    
    # Save Normalized Plot
    for(fmt in params$export_formats) {
      ggsave(file.path(folder_path, paste0("LineScanPlot_Main_Normalized_", params$ch_suffix, ".", fmt)), plot = p_main_norm, device = fmt, width = 8, height = 6, dpi = 300)
    }
  }
  
  for(sf in list.dirs(folder_path, recursive = FALSE)) process_folder(sf, params, first_in_subtree)
  return(list(params = params, first_in_subtree = first_in_subtree))
}

# =========================================================================
# 3. EXECUTION & MASTER SUMMARY GENERATION
# =========================================================================
main_folder <- tk_choose.dir(caption = "Select main folder with subfolders")
if (!is.na(main_folder)) {
  res <- process_folder(main_folder, params = NULL, first_in_subtree = FALSE)
  
  stats_dir <- file.path(main_folder, "Master Stats")
  if (!dir.exists(stats_dir)) { dir.create(stats_dir) }
  
  if (res$params$do_master && res$params$do_coloc && length(master_coloc_list) > 0) {
    coloc_df <- bind_rows(master_coloc_list)
    write.csv(coloc_df, file.path(stats_dir, paste0("Master_Colocalization_Results_", res$params$ch_suffix, ".csv")), row.names = FALSE)
    
    avg_coloc_df <- coloc_df %>%
      group_by(Channel_1, Channel_2) %>%
      summarise(
        Pearson_r_Mean = mean(Pearson_r, na.rm = TRUE), 
        Pearson_r_SD = sd(Pearson_r, na.rm = TRUE), 
        .groups = "drop"
      )
    write.csv(avg_coloc_df, file.path(stats_dir, paste0("Average_Colocalization_Results_", res$params$ch_suffix, ".csv")), row.names = FALSE)
  }
  
  # =========================================================================
  # 4. AUTOMATED QUARTO REPORT GENERATION
  # =========================================================================
  cat("\nGenerating Quarto Summary Report... This may take a moment.\n")
  
  qmd_file <- file.path(main_folder, paste0("Analysis_Report_", res$params$ch_suffix, ".qmd"))
  
  # --- A. Initialize Base Document ---
  qmd_content <- c(
    "---",
    "title: 'Fluorescence Line Scan Analysis Report'",
    paste0("subtitle: 'Channels Analyzed: ", paste(res$params$analysis_channels, collapse = " vs "), "'"),
    "date: today",
    "format:",
    "  html:",
    "    theme: flatly",
    "    toc: true",
    "    toc-depth: 4",
    "    embed-resources: true", 
    "execute:",
    "  echo: false",
    "  warning: false",
    "---",
    "",
    "## 1. Analysis Overview",
    paste0("This automated report summarizes the quantitative fluorescence line scan colocalization analyses performed on the dataset located in: **", basename(main_folder), "**."),
    "The primary goal of this analysis is to evaluate spatial intensity distributions and measure the degree of colocalization between target proteins along a defined spatial path.",
    "",
    "---",
    "",
    "## 2. Interpretation Guide",
    "This section provides general guidelines for interpreting the metrics when comparing fluorescent signal profiles between channels (e.g., Protein A vs. Protein B):",
    ""
  )
  
  # --- B. Append Generic Colocalization Interpretation ---
  if (res$params$do_coloc) {
    qmd_content <- c(
      qmd_content, 
      "* **Profile Colocalization (Pearson's r):** Evaluates the linear relationship between two intensity profiles across space.",
      "  * **High Positive $r$ ($r \\approx +1$):** Indicates that Protein A and Protein B localize together, sharing identical peaks and troughs along the scan path.",
      "  * **Near Zero $r$ ($r \\approx 0$):** Indicates independent spatial distributions with no linear correlation.",
      "  * **Negative $r$ ($r \\approx -1$):** Indicates mutually exclusive localization, where one protein's peak coincides with the other protein's trough (e.g., distinct compartmentalization)."
    )
  }
  
  # --- C. Append Mathematical Definitions ---
  qmd_content <- c(
    qmd_content, 
    "", 
    "---", 
    "", 
    "## 3. Mathematical Definitions & Methods", 
    "This section details the formal biostatistical equations used to process the raw pixel intensity data.", 
    ""
  )
  
  if (res$params$normalize) {
    qmd_content <- c(
      qmd_content, 
      "### Data Normalization", 
      "To facilitate visual comparison between channels with different dynamic ranges or expression levels, the raw intensity data can be min-max normalized. This scales each channel's profile independently from 0.0 (background minimum) to 1.0 (peak maximum):", 
      "$$ I_{norm} = \\frac{I_{raw} - \\min(I_{raw})}{\\max(I_{raw}) - \\min(I_{raw})} $$", 
      "\n*Where:* $I_{raw}$ is the original raw pixel intensity, and $I_{norm}$ is the scaled intensity.", 
      ""
    )
  }
  
  if (res$params$do_coloc) {
    qmd_content <- c(
      qmd_content, 
      "### Profile Colocalization (Pearson's $r$)", 
      "Pearson's correlation coefficient ($r$) measures the strength and direction of association between the intensity values of two channels across all sample points along the line scan:", 
      "$$ r = \\frac{\\sum (C_{1i} - \\bar{C}_1)(C_{2i} - \\bar{C}_2)}{\\sqrt{\\sum (C_{1i} - \\bar{C}_1)^2 \\sum (C_{2i} - \\bar{C}_2)^2}} $$", 
      "\n*Where:* $C_{1i}$ and $C_{2i}$ represent individual pixel intensities for Channel 1 and Channel 2, and $\\bar{C}_1$ and $\\bar{C}_2$ represent the mean intensity of each respective channel.", 
      ""
    )
  }
  
  # --- D. Append Image Generation Block ---
  qmd_content <- c(
    qmd_content,
    "---",
    "",
    "## 4. Visual Results by ROI",
    "Below are the plots generated from this analysis run, organized by Region of Interest (ROI). **Use the Table of Contents on the left to navigate directly to a specific ROI.**",
    "",
    "```{r results='asis', fig.align='center', out.width='90%'}",
    paste0("plot_files <- list.files(path = '.', pattern = '.*", res$params$ch_suffix, "\\\\.png$', full.names = TRUE, recursive = TRUE)"),
    "if(length(plot_files) > 0) {",
    "  dirs <- dirname(plot_files)",
    "  unique_dirs <- unique(dirs)",
    "  for (d in unique_dirs) {",
    "    roi_name <- basename(d)",
    "    if (roi_name == '.') roi_name <- 'Main Folder'",
    "    cat(paste0('\\n\\n### ROI: ', roi_name, '\\n\\n'))",
    "    files_in_dir <- plot_files[dirs == d]",
    "    for (f in files_in_dir) {",
    "      cat(paste0('![](', f, ')\\n\\n'))",
    "    }",
    "    cat('\\n---\\n\\n')",
    "  }",
    "} else {",
    "  cat('No PNG plots matching this analysis run were found.')",
    "}",
    "```",
    ""
  )
  
  # --- E. Write and Render ---
  writeLines(qmd_content, qmd_file)
  if (requireNamespace("quarto", quietly = TRUE)) {
    tryCatch({
      quarto::quarto_render(input = qmd_file, quiet = TRUE)
      cat("-> Report successfully compiled to HTML!\n")
    }, error = function(e) {
      cat("-> Could not render Quarto document. Please ensure Quarto CLI is installed on your OS.\n Error details:", e$message, "\n")
    })
  } else {
    cat("-> 'quarto' R package not found. Report text generated, but HTML compilation skipped.\n")
  }
  # =========================================================================
  
  cat("\nAnalysis Complete! All requested files and stats generated successfully in:", stats_dir, "\n")
} else { 
  cat("No folder selected. Exiting.\n") 
}
