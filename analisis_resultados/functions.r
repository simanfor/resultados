
accumulated_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                              title, subtitle, x, y, save, output_path){
  # Function to graph the accumulated value of a desired variable for each scenario 
  # internal function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # projection_time: time to be used for the projection (5 years by default)
  # scnr_name: column name for the scenario groups (folder_name by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language to be used (es, en)
  # title: title for the graph
  # subtitle: subtitle for the graph
  # x: x axis label
  # y: y axis label
  # save: save the graph (no by default) (yes/si, no)
  # output_path: path to save the graph (getwd() by default)
  
  df <- get_accumulated_variable(df, y_axis_var = y_axis_var)
  
  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var)
  
  df <- skip_duplicated_harvests(df, grouped_x_axis_var = grouped_x_axis_var, file_groups = "file_name")
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var, lang)
  avg_df_all <- get_avg_df(df, "accumulated", scnr_name, grouped_x_axis_var, lang)
  avg_df_all <- avg_df_all %>% rename(accumulated = y_axis_var)
  
  action_name <- lang_switcher(lang, 'action_name')
  avg_df <- merge(avg_df, avg_df_all, by = c(`scnr_name`, `grouped_x_axis_var`, `action_name`))
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  
  # reorder the levels of the factor
  action_order <- lang_switcher(lang, "action_order")
  action_order <- strsplit(action_order, "-")[[1]]
  avg_df$action <- factor(avg_df[[action_name]], levels = action_order)
  avg_df[[action_name]] <- NULL
  avg_df <- avg_df %>%
    arrange(scnr_name, grouped_x_axis_var, action)
  action_name <- lang_switcher(lang, 'action_name_legend')
  shape_values <- setNames(c(16, 17, 4), action_order)
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = accumulated,  
                          group = scnr_name, colour = scnr_name)) +
    geom_point(aes(shape = action), size = 2.5) +
    geom_line(linetype = "solid") +
    labs(title = title, subtitle = subtitle, x = x, y = y, shape = action_name) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) +
    scale_shape_manual(values = shape_values)   
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_accumulated")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
  
  return(g)
}



accumulated_standing_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                                       title, subtitle, x, y, save, output_path){
  # Function to graph the accumulated + standing value of a desired variable for each scenario 
  # internal function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # projection_time: time to be used for the projection (5 years by default)
  # scnr_name: column name for the scenario groups (folder_name by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language to be used (es, en)
  # title: title for the graph
  # subtitle: subtitle for the graph
  # x: x axis label
  # y: y axis label
  # save: save the graph (no by default) (yes/si, no)
  # output_path: path to save the graph (getwd() by default)
  
  df <- get_accumulated_variable(df, y_axis_var = y_axis_var)
  
  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var)
  
  df <- skip_duplicated_harvests(df, grouped_x_axis_var = grouped_x_axis_var, file_groups = "file_name")
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var, lang)
  avg_df_all <- get_avg_df(df, "accumulated", scnr_name, grouped_x_axis_var, lang)
  avg_df_all <- avg_df_all %>% rename(accumulated = y_axis_var)
  
  action_name <- lang_switcher(lang, 'action_name')
  avg_df <- merge(avg_df, avg_df_all, by = c(`scnr_name`, `grouped_x_axis_var`, `action_name`))
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  caption <- lang_switcher(lang, "caption")
  
  # reorder the levels of the factor
  action_order <- lang_switcher(lang, "action_order")
  action_order <- strsplit(action_order, "-")[[1]]
  avg_df$action <- factor(avg_df[[action_name]], levels = action_order)
  avg_df[[action_name]] <- NULL
  avg_df <- avg_df %>%
    arrange(scnr_name, grouped_x_axis_var, action)
  action_name <- lang_switcher(lang, 'action_name_legend')
  shape_values <- setNames(c(16, 17, 4), action_order)
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = accumulated,  
                          group = scnr_name, colour = scnr_name)) +
    geom_point(aes(shape = action), size = 2.5) +
    geom_line(linetype = "solid") +
    geom_line(aes(y = y_axis_var), linetype = "dashed") +
    geom_point(aes(y = y_axis_var, shape = action), size = 2.5) +
    labs(title = title, subtitle = subtitle, x = x, y = y, caption = caption, shape = action_name) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) +
    scale_shape_manual(values = shape_values)   
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_standing_accumulated")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
  
  return(g)
}



get_variable_summary <- function(df, y_axis_var, projection_time = 5, scnr_name = "folder_name",
                                 x_axis_var = "T", grouped_x_axis_var = "T_grouped", lang = "es", 
                                 save = "yes", output_path = getwd()){
  # Function to select the type of data processing to be performed
  # user function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # projection_time: time to be used for the projection (5 years by default)
  # scnr_name: column name for the scenario groups (folder_name by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language to be used (es, en)
  
  if(!is.numeric(df[[x_axis_var]])) stop(lang_switcher(lang, "x_axis_error"))
  if(!is.numeric(df[[y_axis_var]])) stop(lang_switcher(lang, "y_axis_error"))
  
  df <- get_accumulated_variable(df, y_axis_var, scnr_name)
  df <- homogenize_age(df, projection_time, x_axis_var, grouped_x_axis_var)
  df_proc <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var, lang)
  
  df_total <- get_avg_df(df, "accumulated", scnr_name, grouped_x_axis_var, lang)
  df_total <- df_total %>% rename(accumulated = y_axis_var)
  
  action_name <- lang_switcher(lang, 'action_name')
  df_proc <- merge(df_proc, df_total, by = c(scnr_name, grouped_x_axis_var, action_name))
  original_label <- paste(y_axis_var, "_original", sep = "")
  df_proc[[original_label]] <- df_proc$y_axis_var
  df_proc$y_axis_var <- NULL
  
  accumulated_label <- lang_switcher(lang, "output_accumulated")
  accumulated_label <- paste(y_axis_var, "_", accumulated_label, sep = "")
  df_proc[[accumulated_label]] <- df_proc$accumulated
  df_proc$accumulated <- NULL
  
  extracted_name <- lang_switcher(lang, "extracted_name")
  extracted_label <- paste(y_axis_var, "_", extracted_name, sep = "")
  df_proc <- get_extracted_variable(df_proc, original_label, grouped_x_axis_var, scnr_name, lang)
  df_proc[[extracted_label]] <- df_proc$extracted
  df_proc$extracted <- NULL
  df_proc$diff <- NULL
  
  if(save %in% c("si", "yes")){
    output_data <- lang_switcher(lang, "output_data")
    write.csv(df_proc, file = paste(output_path, "/", output_data, "-", y_axis_var, ".csv", sep = ""), 
              row.names = FALSE)
  }
  
  return(df_proc)
}



get_extracted_variable <- function(df, y_axis_var, grouped_x_axis_var, scnr_name = "folder_name", lang = 'es'){
  # Function to get the accumulated value of a desired variable for each plot
  # internal function
  # Arguments:
  # df: data frame to be processed
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # y_axis_var: column name for the variable that will be displayed on y axis
  # scnr_name: column name for the groups (folder_name by default)
  # lang: language for the output (es by default)
  
  # reorder the levels of the factor
  action_order <- lang_switcher(lang, "action_order")
  action_order <- strsplit(action_order, "-")[[1]]
  action_name <- lang_switcher(lang, 'action_name')
  df <- df %>%
    arrange(df[[scnr_name]], df[[grouped_x_axis_var]], factor(df[[action_name]], levels = action_order))
  
  # create a new column with the difference between the values on each simulation step
  df <- df %>%
    group_by(across(all_of(c(scnr_name)))) %>%
    mutate(diff = .data[[y_axis_var]] - lag(.data[[y_axis_var]]))
  
  # create a new column with the accumulated variable
  new_df <- tibble()
  
  for(scnr in unique(df[[scnr_name]])){
    
    scnr <- df[df[[scnr_name]] == scnr, ]
    acc_variable <- 0
    
    for(row in 1:nrow(scnr)){
      
      new_row <- scnr[row, ]
      
      if(row == 1){
        
        new_row$extracted <- acc_variable
      } else {
        
        if(new_row$diff > 0){
          acc_variable <- acc_variable
        } else {
          acc_variable <- acc_variable + abs(new_row$diff)
        }
        
        new_row$extracted <- acc_variable
      }
      
      new_df <- rbind(new_df, new_row)
      
    }
  }
  
  return(new_df)
}



get_accumulated_variable <- function(df, y_axis_var, 
                                     scnr_name = "folder_name", file_groups = "file_name"){
  # Function to get the accumulated value of a desired variable for each plot
  # internal function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # scnr_name: column name for the groups (folder_name by default)
  # file_groups: column name for the groups (file_name by default)
  
  # create a new column with the difference between the values on each simulation step
  df <- df %>%
    group_by(across(all_of(c(scnr_name, file_groups)))) %>%
    mutate(diff = .data[[y_axis_var]] - lag(.data[[y_axis_var]]))
  
  # create a new column with the accumulated variable
  new_df <- tibble()
  
  for(scnr in unique(df[[scnr_name]])){
    
    scnr <- df[df[[scnr_name]] == scnr, ]
    
    for(plot in unique(scnr[[file_groups]])){
      
      plot <- scnr[scnr[[file_groups]] == plot, ]
      acc_variable <- 0
      
      for(row in 1:nrow(plot)){
        
        new_row <- plot[row, ]
        
        if(row == 1){
          # get the first value
          acc_variable <- new_row[[y_axis_var]]
          new_row$accumulated <- acc_variable
        } else {
          # estimate increment except on harvests, that must be excluded
          # acc_variable <- acc_variable + abs(new_row$diff)
          if(new_row$diff > 0){
            acc_variable <- acc_variable + new_row$diff
          } else {
            acc_variable <- acc_variable
          }
          new_row$accumulated <- acc_variable
        }
        
        new_df <- rbind(new_df, new_row)
        
      }
    }
  }
  return(new_df)
}



get_avg_df <- function(df, y_axis_var, scnr_name = "folder_name", grouped_x_axis_var = "T_grouped", lang = "es"){
  # Function to get the average values for each group (scenario) and stand age (grouped age)
  # internal function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: variable to be averaged
  # scnr_name: column name for the scenario groups (folder_name by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language for the output (es by default)
  
  # get average values for each group and stand age
  action_name <- lang_switcher(lang, 'action_name')
  avg_df <- df %>%
    group_by(across(all_of(c(scnr_name, grouped_x_axis_var, action_name)))) %>%
    summarise(y_axis_var = mean(.data[[y_axis_var]], na.rm = TRUE), .groups = "drop")
  
  return(avg_df)
}



graph_selector <- function(df, y_axis_var, graph_type, projection_time = 5, scnr_name = "folder_name",
                           x_axis_var = "T", grouped_x_axis_var = "T_grouped", lang = "es",
                           title = NULL, subtitle = NULL, x = NULL, y = NULL, 
                           save = "no", output_path = getwd()){
  # Function to select the type of graph to be generated
  # user function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # graph_type: type of graph to be generated (accumulated, standing + accumulated, standing)
  # projection_time: time to be used for the projection (5 years by default)
  # scnr_name: column name for the scenario groups (folder_name by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language to be used (es, en)
  # title: title for the graph
  # subtitle: subtitle for the graph
  # x: x axis label
  # y: y axis label
  # save: save the graph (no by default) (yes/si, no)
  # output_path: path to save the graph (getwd() by default)
  
  if(!is.numeric(df[[x_axis_var]])){
    stop(lang_switcher(lang, "x_axis_error"))
  }
  if(!is.numeric(df[[y_axis_var]])){
    stop(lang_switcher(lang, "y_axis_error"))
  }
  
  if(graph_type %in% c("en pie", "standing")){
    return(standing_graph(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                          title, subtitle, x, y, save, output_path))
  } else if(graph_type %in% c("acumulado", "accumulated")){
    return(accumulated_graph(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                             title, subtitle, x, y, save, output_path))
  } else if(graph_type %in% c("en pie + acumulado", "standing + accumulated")){
    accumulated_standing_graph(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                               title, subtitle, x, y, save, output_path)
  } else {
    stop(lang_switcher(lang, "graph_type_error"))
  }
}



homogenize_age <- function(df, projection_time = 5, x_axis_var = "T", grouped_x_axis_var = "T_grouped"){
  # Function to group all the plots by average age according to the projection time
  # internal function
  # Arguments:
  # df: data frame to be processed
  # projection_time: time to be used for the projection (5 years by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  
  df[[grouped_x_axis_var]] <- projection_time * round(df[[x_axis_var]]/projection_time)
  return(df)
}



install_and_load <- function(packages) {
  # Function to check if the required packages are installed and load them
  # user function
  # Arguments:
  # packages: list of packages to be installed and loaded
  
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
}



lang_switcher <- function(lang, text){
  # Function to select the correct strings based on the language
  # internal function
  # Arguments:
  # lang: language to be used (es, en)
  # text: text to be translated
  
  translations <- list(
    sheet_name = c(es = "Parcelas", en = "Plots"),
    action_name = c(es = "Accion", en = "Action"),
    action_name_legend = c(es = "Acción", en = "Action"),
    action_order = c(es = "Inicialización-Ejecución-Corta", en = "Initialization-Execution-Thinning"), 
    extracted_name = c(es = "extraido", en = "extracted"),
    harvest_name = c(es = "Corta", en = "Thinning"),
    scnr_name = c(es = "Nombre_archivo_escenario", en = "Scenario_file_name"),
    scnr_legend = c(es = "Escenario", en = "Scenario"),
    caption = c(
      es = "La línea sólida representa la producción total (en pie + extraído); la línea discontinua representa el valor en pie",
      en = "The solid line represents the total production (standing + extracted); the dashed line represents the standing value"
    ),
    x_axis_error = c(es = "El eje X debe ser numérico", en = "The X axis must be numeric"),
    y_axis_error = c(es = "El eje Y debe ser numérico", en = "The Y axis must be numeric"),
    graph_type_error = c(es = "Tipo de gráfico no válido", en = "Invalid graph type"),
    output_standing = c(es = "en_pie", en = "standing"),
    output_standing_accumulated = c(es = "en_pie_y_acumulado", en = "standing_and_accumulated"),
    output_accumulated = c(es = "acumulado", en = "accumulated"),
    output_data = c(es = "resumen_datos", en = "data_summary")
  )
  
  if (text %in% names(translations)) {
    return(translations[[text]][[lang]])
  } else {
    return("Error")
  }
  
  return(string)
}



load_plot_data <- function(simulations_path, lang = "es"){
  # Function to load the data from the simulations
  # user function
  # Arguments:
  # simulations_path: path to the simulations output
  # lang: language to be used (es, en)
  
  sheet_name <- lang_switcher(lang, "sheet_name")
  action_name <- lang_switcher(lang, "action_name")
  scnr_name <- lang_switcher(lang, "scnr_name")
  
  plots <- data.frame()
  directory <- list.dirs(path = simulations_path)  
  
  for(folder in directory){
    
    # avoid // errors on path
    folder <- gsub("//", "/", folder)
    
    files_list <- list.files(path = folder, pattern="xlsx")
    
    for(doc in files_list) {
      
      plot_data <- read_excel(path = paste(folder, "/", doc, sep = ""),
                              sheet = sheet_name)  
      
      plot_data$file_name <- doc
      plot_data$file_path <- folder
      plot_data$folder_name <- basename(folder)
      plot_data$scenario_name <- plot_data[[scnr_name]]
      plot_data$scenario_name <- gsub(".json", "", plot_data$scenario_name)
      
      ifelse(length(plots) == 0, plots <- rbind(plot_data), plots <- rbind(plots, plot_data))
    }
  }
  
  plots <- plots[!plots[[action_name]] == "Carga Inicial", ] 
  
  rm(directory, folder, files_list, doc, plot_data, sheet_name)
  
  return(plots)
}



skip_duplicated_harvests <- function(df, grouped_x_axis_var = 'T_grouped', file_groups = "file_name"){
  # Function to skip duplicated harvests when they are programmed at the same age
  # internal function
  # Arguments:
  # df: data frame to be processed
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # file_groups: column name for the groups (file_name by default)
  
  # get average values for each group and stand age
  action_name <- lang_switcher(lang, 'action_name')
  
  df <- df %>%
    mutate(row_id = row_number()) %>%  # preserve original order
    group_by(across(all_of(c(action_name, grouped_x_axis_var, file_groups)))) %>%
    mutate(n = n(),
           orden = row_number()) %>%
    ungroup() %>%
    filter(!(n > 1 & orden == 1)) %>%  # delete just the first of the duplicated harvests
    select(-row_id, -n, -orden)
  
  return(df)
}



standing_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                           title, subtitle, x, y, save, output_path){
  # Function to graph the standing value of a desired variable for each scenario 
  # internal function
  # Arguments:
  # df: data frame to be processed
  # y_axis_var: column name for the variable that will be displayed on y axis
  # projection_time: time to be used for the projection (5 years by default)
  # scnr_name: column name for the scenario groups (folder_name by default)
  # x_axis_var: column name for the variable that will be displayed on x axis (T by default)
  # grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
  # lang: language to be used (es, en)
  # title: title for the graph
  # subtitle: subtitle for the graph
  # x: x axis label
  # y: y axis label
  # save: save the graph (no by default) (yes/si, no)
  # output_path: path to save the graph (getwd() by default)
  
  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var) 
  
  df <- skip_duplicated_harvests(df, grouped_x_axis_var = grouped_x_axis_var, file_groups = "file_name")
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var, lang)
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  
  # reorder the levels of the factor
  action_order <- lang_switcher(lang, "action_order")
  action_order <- strsplit(action_order, "-")[[1]]
  action_name <- lang_switcher(lang, 'action_name')
  avg_df$action <- factor(avg_df[[action_name]], levels = action_order)
  avg_df[[action_name]] <- NULL
  avg_df <- avg_df %>%
    arrange(scnr_name, grouped_x_axis_var, action)
  action_name <- lang_switcher(lang, 'action_name_legend')
  shape_values <- setNames(c(16, 17, 4), action_order)
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = y_axis_var,  
                          group = scnr_name, colour = scnr_name)) +
    geom_point(aes(shape = action), size = 2.5) +
    geom_line(linetype = "solid") +
    labs(title = title, subtitle = subtitle, x = x, y = y, shape = action_name) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) +
    scale_shape_manual(values = shape_values)   
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_standing")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
  
  return(g)
}
