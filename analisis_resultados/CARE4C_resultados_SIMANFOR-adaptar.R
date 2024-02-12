#------------------------------------------------------------------------------------------#
####                       Groupe SIMANFOR results on a single df                       ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/03/2022                              #
#                              Last modification: 23/07/2023                               #
#------------------------------------------------------------------------------------------#



#### Summary ####

# Extended explanation here: 
# https://github.com/simanfor/resultados/blob/main/analisis_resultados/analisis_resultados_SIMANFOR.R


#### Basic steps ####

# libraries
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# set directory
general_dir <- "/media/aitor/Elements/aitor/iuFOR_trabajo/Repositorios/LINUX/simanfor-dask/simulator/output/CARE4C/PsylFsyl/"
setwd(general_dir)


#### Read SIMANFOR outputs (just plot information) ####

plots <- tibble()  # will contain plot data
directory <- list.dirs(path = ".")  # will contain folder names

# for each subfolder...
for (folder in directory){ 
        
  # each subfolder is stablished as main one
  specific_dir <- paste(general_dir, "substract", folder, sep = "")
  specific_dir <- gsub("substract.", "", specific_dir)
  setwd(specific_dir)
  
  # extract .xlsx files names
  files_list <- list.files(specific_dir, pattern="xlsx")
  
  # for each file...
  for (doc in files_list){
          
    # read plot data
    plot_data <- read_excel(doc, sheet = "Plots")
  
    # create a new column with its name                
    plot_data$File_name <- doc  
  
    # add information to plot df
    ifelse(length(plots) == 0, plots <- rbind(plot_data), plots <- rbind(plots, plot_data))
  }
}


#### Data management - stand evolution ####

# make a copy of the data
df <- plots  

# function to round on ages on 5 years step
redondeo <- function(x, base){  
        base * round(x/base)
}                                 

# remove initial load
df <- df[!df$Action == "Initial load", ]

# round ages
df$T <- redondeo(df$T, 5) 

# get scenario code
df$n_scnr <- substr(df$Scenario_file_name, 17, 19)

# delete empty rows
df <- df[!is.na(df$n_scnr), ]


#### Mean stand evolution ####

# mean values by scenario and year
mean_evo_by_scnr <- ddply(df, c('n_scnr', 'T'), summarise, 

                          # general variables                         
                          N = mean(N, na.rm = TRUE),                  
                          dg = mean(dg, na.rm = TRUE),
                          Ho = mean(Ho, na.rm = TRUE),  
                          V = mean(V, na.rm = TRUE), 
                          WT = mean(WT, na.rm = TRUE), 
                          G = mean(G, na.rm = TRUE),
                          
                          # biomass classification
                          WSW = mean(WSW, na.rm = TRUE),
                          WTHICKB = mean(WTHICKB, na.rm = TRUE),
                          WB2_7 = mean(WB2_7, na.rm = TRUE),
                          WTHINB = mean(WTHINB, na.rm = TRUE),
                          WTBL = mean(WTBL, na.rm = TRUE),
                          WR = mean(WR, na.rm = TRUE)
                          
)

# clean data
rm(plot_data, directory, doc, files_list, folder, general_dir, specific_dir)


#### Accumulated stand wood (thinninged included) ####

# reuse initial df
df_2 <- plots 

# remove initial load
df_2 <- df_2[!df_2$Action == "Initial load", ]

# calculate differences per scenario step on the desired variables
# that is the first step to record losses and gains due to thinning
df_2 <- df_2 %>%
  group_by(File_name, Scenario_file_name) %>%
  mutate(V_diff = V - lag(V),
         WSW_diff= WSW - lag(WSW),
         WTHICKB_diff= WTHICKB - lag(WTHICKB),
         WB2_7_diff= WB2_7 - lag(WB2_7),
         WTHINB_diff= WTHINB - lag(WTHINB),
         WTBL_diff= WTBL - lag(WTBL),
         WR_diff= WR - lag(WR),
         WT_diff= WT - lag(WT),
)

# create a new df with accumulated values
new_df_2 <- tibble()

# for each scenario...
for(scnr in unique(df_2$Scenario_file_name)){
  
  # get data
  scnr <- df_2[df_2$Scenario_file_name == scnr, ]
  
  # for each plot in the scenario...
  for(plot in unique(scnr$File_name)){
    
    # get data
    plot <- scnr[scnr$File_name == plot, ]
    
    # stablish initial values for accumulated variables as 0
    all_V <- all_WSW <- all_WTHICKB <- all_WB2_7 <- all_WTHINB <- all_WTBL <- all_WR <- all_WT <- 0
    
    # for each row...
    for(row in 1:nrow(plot)){
      
      # select data
      new_row <- plot[row, ]
      
      # if it is row 1, then initial values must be taken
      if(row == 1){
        
        # get initial value
        all_V <- new_row$V
        all_WSW <- new_row$WSW
        all_WTHICKB <- new_row$WTHICKB
        all_WB2_7 <- new_row$WB2_7
        all_WTHINB <- new_row$WTHINB
        all_WTBL <- new_row$WTBL
        all_WR <- new_row$WR
        all_WT <- new_row$WT
        
        # add value to the row
        new_row$V_all <- all_V
        new_row$WSW_all <- all_WSW
        new_row$WTHICKB_all <- all_WTHICKB
        new_row$WB2_7_all <- all_WB2_7
        new_row$WTHINB_all <- all_WTHINB
        new_row$WTBL_all <- all_WTBL
        new_row$WR_all <- all_WR
        new_row$WT_all <- all_WT
        
      # if it is another row, then difference between rows is added in abs()
      } else {
        
        # add increment to the previous value
        all_V <- all_V + abs(new_row$V_diff)
        all_WSW <- all_WSW + abs(new_row$WSW_diff)
        all_WTHICKB <- all_WTHICKB + abs(new_row$WTHICKB_diff)
        all_WB2_7 <- all_WB2_7 + abs(new_row$WB2_7_diff)
        all_WTHINB <- all_WTHINB + abs(new_row$WTHINB_diff)
        all_WTBL <- all_WTBL + abs(new_row$WTBL_diff)
        all_WR <- all_WR + abs(new_row$WR_diff)
        all_WT <- all_WT + abs(new_row$WT_diff)
        
        # add value to the row
        new_row$V_all <- all_V
        new_row$WSW_all <- all_WSW
        new_row$WTHICKB_all <- all_WTHICKB
        new_row$WB2_7_all <- all_WB2_7
        new_row$WTHINB_all <- all_WTHINB
        new_row$WTBL_all <- all_WTBL
        new_row$WR_all <- all_WR
        new_row$WT_all <- all_WT
      }
      
      # add new row to a new df
      new_df_2 <- rbind(new_df_2, new_row)
      
    } # row
  } # plot
} # scenario

# round ages
new_df_2$T <- redondeo(new_df_2$T, 5) 

# get scenario code
new_df_2$n_scnr <- substr(new_df_2$Scenario_file_name, 17, 19)

# delete empty rows
new_df_2 <- new_df_2[!is.na(new_df_2$n_scnr), ]


#### Mean accumulated stand evolution ####

# mean values by scenario and year
stand_evolution <- ddply(new_df_2, c('n_scnr', 'T'), summarise, 
                          
                          # general variables                         
                          N = mean(N, na.rm = TRUE),                  
                          dg = mean(dg, na.rm = TRUE),
                          Ho = mean(Ho, na.rm = TRUE),  
                          V = mean(V_all, na.rm = TRUE), 
                          WT = mean(WT_all, na.rm = TRUE), 
                          G = mean(G, na.rm = TRUE),
                          
                          # biomass classification - stand variables
                          WSW = mean(WSW, na.rm = TRUE),
                          WTHICKB = mean(WTHICKB, na.rm = TRUE),
                          WB2_7 = mean(WB2_7, na.rm = TRUE),
                          WTHINB = mean(WTHINB, na.rm = TRUE),
                          WTBL = mean(WTBL, na.rm = TRUE),
                          WR = mean(WR, na.rm = TRUE),
                          
                          # all accumulated stand variables
                          WSW_all = mean(WSW_all, na.rm = TRUE),
                          WTHICKB_all = mean(WTHICKB_all, na.rm = TRUE),
                          WB2_7_all = mean(WB2_7_all, na.rm = TRUE),
                          WTHINB_all = mean(WTHINB_all, na.rm = TRUE),
                          WTBL_all = mean(WTBL_all, na.rm = TRUE),
                          WR_all = mean(WR_all, na.rm = TRUE),
                          WT_all = mean(WT_all, na.rm = TRUE),
                          V_all = mean(V_all, na.rm = TRUE)
)

