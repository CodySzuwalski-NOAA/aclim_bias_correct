# Set the working directory to the folder containing the .csv files
folder_path <- paste(getwd(),"/data/kelly_csv/",sep='')

# List all files in the folder that contain both 'annual' and 'Hindcast' in the filename
# List all files in the folder that contain both 'annual' and 'Hindcast' in the filename
files <- list.files(path = folder_path, pattern = "annual.*Hindcast.*\\.csv$", full.names = TRUE)


# Initialize an empty list to store data frames
df_list <- list()

# Loop over each file and process
for (file in files) {
  # Read the CSV file
  df <- read.csv(file)
  
  # Extract the signifier (e.g., 'bc1' from 'annual_bc1_Hindcast.csv')
  signifier <- sub(".*annual_(.*?)_Hindcast.*\\.csv$", "\\1", basename(file))
  
  # Add a new column to the data frame with the signifier
  df$signifier <- signifier
  
  # Append the data frame to the list
  df_list[[length(df_list) + 1]] <- df
}

# Combine all data frames into one
final_df <- do.call(rbind, df_list)

# Print the final data frame
print(final_df)

ggplot(final_df)+
  geom_line(aes(x=as.Date(Time),y=aice,col=signifier))

#==quick comparison for just historical
my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}
hind_rel_plot<-ggpairs(final_df[,which(!is.na(match(colnames(final_df),use_ind)))],lower = list(continuous = my_fn),upper=list(continuous = my_fn)) +theme_bw() 
png("ggpairs_hindcast.png",width=6,height=6,units='in',res=300)
print(hind_rel_plot)
dev.off()