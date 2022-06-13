# ------------- Packages ------------- 
library(DT)
library(readxl); library(here)
library(akima); library(metR); library(RColorBrewer)
library(tidyverse)

# ------------- Load Data ------------- 

file1 = "Nutlin + Radiation, JHUEM2, Hec108, Hec1B, 5-14-21.xlsx"#Name of the file to be read
sheet_name1 = c("JHUEM2", "Hec108", "Hec1B") #Name of the sheets within the file that is associated with each experiment
variable_names1 = c("jhuem2", "hec108", "hec1b")#What each table should be referred to as in the script.

file2 = "AMG + Radiation, JHUEM2, Hec108, Hec1B, 5-7-21.xlsx"
sheet_name2 = c("JHUEM2", "Hec108", "Hec1B")
variable_names2 = c("jhuem2", "hec108", "hec1b")

file3 = "JHUEM2 WT, variant + Nutlin, Radiation, 4-28-21.xlsx"
sheet_name3 = c("NTC + mCherry", "NTC + R273C", "KO + R273C")
variable_names3 = c("ntc_mch", "ntc_r", "ko_r")


# Read in the files
data1 = list() #Create an empty list to fill with the data
#Loop through each of the sheets within the excel file and store the table in an element within the list
for (i in 1:length(sheet_name1)){
  data1[[i]] = read_xlsx(here("Data", file1), sheet = sheet_name1[i])
}
names(data1) = variable_names1 #Label each of the sheets' data with the appropriate experiment name
xlimit1= 10
ylimit1= 4


data2 = list()
for (i in 1:length(sheet_name2)){
  data2[[i]] = read_xlsx(here("Data", file2), sheet = sheet_name2[i])
}
names(data2) = variable_names2
xlimit2= 1
ylimit2= 4


data3 = list()
for (i in 1:length(sheet_name3)){
  data3[[i]] = read_xlsx(here("Data", file3), sheet = sheet_name3[i])
}
names(data3) = variable_names3
xlimit3= 10
ylimit3= 2


# ------------- Server functions ------------- 
function(input, output, session){
  
#Interdependent dropdown options
  observeEvent(c(input$experiment),
               {
                if(input$experiment == "data1"){
                  selected_data=data1
                  selected_labels = sheet_name1
                } else if (input$experiment == "data2"){
                  selected_data=data2
                  selected_labels = sheet_name2
                } else if (input$experiment == "data3"){
                  selected_data=data3
                  selected_labels = sheet_name3
                }
                 cond_options = names(selected_data)
                 
                 updateSelectInput(session,
                                   "condition",
                                   choices = setNames(cond_options, selected_labels))
                   
               })
  
#output the selected data frame
  output$table = renderDT({
    if(input$experiment == "data1"){
      selected_data=data1
    } else if (input$experiment == "data2"){
      selected_data=data2
    } else if (input$experiment == "data3"){
      selected_data=data3
    }
    
    selected_data[[input$condition]] %>%
      datatable(rownames=FALSE)
  })
  
  
#plot an isobologram of the selected data frame
  output$plot = renderPlot({
    #Select the correct data frame
    if(input$experiment == "data1"){
      selected_data=data1
      xlimit=xlimit1
      ylimit=ylimit1
      exp="Nutlin + Radiation"
    } else if (input$experiment == "data2"){
      selected_data=data2
      xlimit=xlimit2
      ylimit=ylimit2
      exp="AMG + Radiation"
    } else if (input$experiment == "data3"){
      selected_data=data3
      xlimit=xlimit3
      ylimit=ylimit3
      exp="JHUEM2 WT, variant + Nutlin, Radiation"
    }
    
    plot_data = as.data.frame(selected_data[[input$condition]])
    
    #Clean the data
    drug_doses = as.vector(colnames(plot_data))
    drug_doses = drug_doses[-1] #remove the first value, because it's not a concentration ("...1")
    
    #Extract the radiation doses using the first column of the table. Use one data frame, all three here are the same anyway.
    rad_doses = as.matrix(plot_data[,1])
    rad_doses = as.vector(rad_doses)
    
    #Change to long format
    plot_data <- plot_data %>% 
        pivot_longer(drug_doses, names_to = "Drug", values_to = "Cell Death")
    #rename the columns so they make sense
    colnames(plot_data) = c("Radiation Dose", "Drug Dose", "Cell Death")
    #make the drug/rad columns numeric
    plot_data["Drug Dose"] = as.numeric(unlist(plot_data["Drug Dose"]))
    plot_data["Radiation Dose"] = as.numeric(unlist(plot_data["Radiation Dose"]))
    
    
    #Convert values from survival to death
    plot_data$`Cell Death` = 1 - (plot_data$`Cell Death` / max(plot_data$`Cell Death`))
    
    #Using the Akima package, we will interpolate the data.
    interpolated_raw = interp(x=(plot_data$`Drug Dose`), 
                              y=(plot_data$`Radiation Dose`), 
                              z=(plot_data$`Cell Death`), nx=500)
    interpolated_to_plot = data.frame(x = rep(interpolated_raw$x, ncol(interpolated_raw$z)),
                                      y = rep(interpolated_raw$y, each = nrow(interpolated_raw$z)),
                                      z = as.numeric(interpolated_raw$z))
    
    #Plot the interpolated data
    output$plot = renderPlot(ggplot(data = interpolated_to_plot, #select the data to plot and define the axes
                         aes(x = x, y = y, z = z)) +
      
      geom_tile(aes(fill = z)) + #Plot the heatmap using the z values (Cell Death)
      geom_contour(colour="white") + #Add contour lines
      geom_text_contour(aes(z = z), stroke = 0.1, skip=0) + #Label the contour lines with associated z values
      
      scale_x_continuous(breaks=seq(from=0, to=xlimit, by=0.5), limits=c(0, xlimit)) + #set how many tick marks appear on each axis; set axis limits
      #for AMG, set by=0.1; for nutlin, set by=0.5
      scale_y_continuous(breaks=seq(0, ylimit, 0.5), limits=c(0, ylimit)) + #AMG and new nutlin: by=0.5; old nutlin: by=0.25
      
      scale_fill_gradient(high="red",low="blue") + #Set heatmap color scheme
      
      guides(fill = guide_colorbar(title = "Cell Death")) + #Define plot labels
      ggtitle(paste0(exp, " - ", input$condition, "Isobologram")) + 
      xlab("Drug (uM)") +
      ylab("Radiation (Gy)"))
    
    
  })
  
}