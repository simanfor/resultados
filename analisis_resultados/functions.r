
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
accumulated_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                              title, subtitle, x, y, save, output_path){
  
  df <- get_accumulated_variable(df, y_axis_var = y_axis_var)
  
  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var)
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var)
  avg_df_all <- get_avg_df(df, "accumulated", scnr_name, grouped_x_axis_var)
  avg_df_all <- avg_df_all %>% rename(accumulated = y_axis_var)
  avg_df <- merge(avg_df, avg_df_all, by = c(`scnr_name`, `grouped_x_axis_var`))
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = accumulated,  
                          group = scnr_name, colour = scnr_name)) +
    geom_line(linetype = "solid") +
    labs(title = title, subtitle = subtitle, x = x, y = y) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) 
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_accumulated")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
  
  return(g)
}


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
accumulated_standing_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                                       title, subtitle, x, y, save, output_path){
  
  df <- get_accumulated_variable(df, y_axis_var = y_axis_var)
  
  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var)
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var)
  avg_df_all <- get_avg_df(df, "accumulated", scnr_name, grouped_x_axis_var)
  avg_df_all <- avg_df_all %>% rename(accumulated = y_axis_var)
  avg_df <- merge(avg_df, avg_df_all, by = c(`scnr_name`, `grouped_x_axis_var`))
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  caption <- lang_switcher(lang, "caption")
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = accumulated,  
                          group = scnr_name, colour = scnr_name)) +
    geom_line(linetype = "solid") +
    geom_line(aes(y = y_axis_var), linetype = "dashed") +
    labs(title = title, subtitle = subtitle, x = x, y = y, caption = caption) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) 
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_standing_accumulated")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
  
  return(g)
}


# Function to get the accumulated value of a desired variable for each plot
# internal function
# Arguments:
# df: data frame to be processed
# y_axis_var: column name for the variable that will be displayed on y axis
# scnr_name: column name for the groups (folder_name by default)
# file_groups: column name for the groups (file_name by default)
get_accumulated_variable <- function(df, y_axis_var, 
                                     scnr_name = "folder_name", file_groups = "file_name"){
  
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
          # estimate increment
          acc_variable <- acc_variable + abs(new_row$diff)
          new_row$accumulated <- acc_variable
        }
        
        new_df <- rbind(new_df, new_row)
        
      }
    }
  }
  return(new_df)
}


# Function to get the average values for each group (scenario) and stand age (grouped age)
# internal function
# Arguments:
# df: data frame to be processed
# y_axis_var: variable to be averaged
# scnr_name: column name for the scenario groups (folder_name by default)
# grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
get_avg_df <- function(df, y_axis_var, scnr_name = "folder_name", grouped_x_axis_var = "T_grouped"){
  
  # get average values for each group and stand age
  avg_df <- df %>%
    group_by(across(all_of(c(scnr_name, grouped_x_axis_var)))) %>%
    summarise(y_axis_var = mean(.data[[y_axis_var]], na.rm = TRUE), .groups = "drop")
  
  return(avg_df)
}


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
graph_selector <- function(df, y_axis_var, graph_type, projection_time = 5, scnr_name = "folder_name",
                           x_axis_var = "T", grouped_x_axis_var = "T_grouped", lang = "es",
                           title = NULL, subtitle = NULL, x = NULL, y = NULL, 
                           save = "no", output_path = getwd()){
  
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


# Function to group all the plots by average age according to the projection time
# internal function
# Arguments:
# df: data frame to be processed
# projection_time: time to be used for the projection (5 years by default)
# x_axis_var: column name for the variable that will be displayed on x axis (T by default)
# grouped_x_axis_var: column name for the variable that will be displayed on x axis grouped (T_grouped by default)
homogenize_age <- function(df, projection_time = 5, x_axis_var = "T", grouped_x_axis_var = "T_grouped"){

    df[[grouped_x_axis_var]] <- projection_time * round(df[[x_axis_var]]/projection_time)
  return(df)
}


# Function to check if the required packages are installed and load them
# user function
# Arguments:
# packages: list of packages to be installed and loaded
install_and_load <- function(packages) {

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
}


# Function to select the correct strings based on the language
# internal function
# Arguments:
# lang: language to be used (es, en)
# text: text to be translated
lang_switcher <- function(lang, text){
  
  translations <- list(
    sheet_name = c(es = "Parcelas", en = "Plots"),
    action_name = c(es = "Accion", en = "Action"),
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
    output_accumulated = c(es = "acumulado", en = "accumulated")
  )
  
  if (text %in% names(translations)) {
    return(translations[[text]][[lang]])
  } else {
    return("Error")
  }
  
  return(string)
}


# Function to load the data from the simulations
# user function
# Arguments:
# simulations_path: path to the simulations output
# lang: language to be used (es, en)
load_plot_data <- function(simulations_path, lang = "es"){

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
standing_graph <- function(df, y_axis_var, projection_time, scnr_name, x_axis_var, grouped_x_axis_var, lang, 
                           title, subtitle, x, y, save, output_path){

  df <- homogenize_age(df, projection_time = projection_time, 
                       x_axis_var = x_axis_var, grouped_x_axis_var = grouped_x_axis_var) 
  
  avg_df <- get_avg_df(df, y_axis_var, scnr_name, grouped_x_axis_var)
  
  # custom names
  avg_df$scnr_name <- avg_df[[scnr_name]]
  avg_df$grouped_x_axis_var <- avg_df[[grouped_x_axis_var]]
  
  scnr <- lang_switcher(lang, "scnr_legend")
  
  g <- ggplot(avg_df, aes(x = grouped_x_axis_var, y = y_axis_var,  
              group = scnr_name, colour = scnr_name)) +
    geom_line(linetype = "solid") +
    labs(title = title, subtitle = subtitle, x = x, y = y) +
    theme_minimal() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
          axis.title = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_color_discrete(scnr, type = viridis::viridis(length(unique(avg_df$scnr_name)))) 
  
  if(save %in% c("si", "yes")){
    output_string <- lang_switcher(lang, "output_standing")
    ggsave(paste(output_path, "/", y_axis_var, "_vs_", x_axis_var, "-", output_string, ".png", sep = ""), 
           plot = g, width = 17, height = 10)
  }
    
  return(g)
}
