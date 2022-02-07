#Tidy Data: Combining and Transforming Data in R
#MMID Coding Workshop: February 9, 2022
#Author: Molly Pratt

#Open the Data Wrangling Cheatsheet from RStudio in your browser here: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

install.packages("tidyverse") #only run this line if you do not already have tidyverse installed.

# Remember you can always bring up the help page for any function by typing "?" before the function name, for example:
# ?read_csv
#==========================================================================================================================================================#

# For this exercise we will be working with a Galaxy line list (tool output, .csv format) 
# and GISAID metadata (downloaded from GISAID, .xls format) to create a combined metadata table 
# containing all the information we want in a "tidy" format.

### 1. Load your datasets into new R objects with readr and readxl: =======================================================================================#
  #Note: library(tidyverse) will load all of the packages we need in one call. 
  #They are loaded individually in this script to give you an idea of which package each function comes from, and to help with troubleshooting. 
  #Once you are comfortable with these packages and functions, library(tidyverse) is all you need.

library(readr)

  # Assign the dataset to a new R object using the arrow "<-" operator:
line_list_raw <- read_csv("workshop_materials_Feb9/data/line_list_galaxy.csv", #Depending on the location of your workshop_materials folder, you may need to modify this file path
                          skip = 2, show_col_types = FALSE) #skip the reference sequence rows, don't show column types in the console.

View(line_list_raw) #this will open the dataset in an interactive viewer in RStudio as a new tab. 
#Note: It is good practice to avoid overwriting your `..._raw` dataset, in case you need to come back to it later.

library(readxl)
gisaid_metadata_raw <- read_excel("workshop_materials_Feb9/data/gisaid_metadata.xls") #Depending on the location of your workshop_materials folder, you may need to modify this file path

View(gisaid_metadata_raw)

### 2. Remove unwanted columns/variables and prepare to combine our two datasets: ========================================================================#

####2.1 ==================================================================================================================================================#
#The line list output contains nucleotide substitution data that we aren't interested in keeping for today (we only want the metadata). 
  #The variables we need are the Clade assignments, total number of substitutions, and the percent identity with the reference sequence.
  #From the raw line list, keep only relevant columns using select(), and rename some columns using rename(). 
  #Save in a new dataframe called `line_list_keep`:

library(dplyr)

line_list_keep <- line_list_raw %>% #when scripting on multiple lines, make sure your pipe (%>%) is at the end of a line, not the beginning.
  select("Sequence Name", 
         "Clade", 
         "Number of Amino Acid Substitutions in Antigenic Sites", 
         "% Identity of Antigenic Site Residues") %>% 
  rename(Isolate_Name = "Sequence Name", #rename to be consistent with the GISAID data
         Num.AA.Sub = "Number of Amino Acid Substitutions in Antigenic Sites", 
         percent.id = "% Identity of Antigenic Site Residues")
#Refer to the Syntax section of your data wrangling cheatsheet for a refresher on `%>%` AKA pipes.
#When renaming columns, always use the format new.col.name = old.col.name (as above)

#Alternatively, you can also select columns by their position, though for complex datasets with many columns this can be challenging.
line_list_keep_alt <- line_list_raw %>% select(1, 3, 136, 137) %>% #Use the viewer to confirm the column positions by hovering over the column header with your mouse.
  rename(Isolate_Name = `Sequence Name`, 
         Num.AA.Sub = `Number of Amino Acid Substitutions in Antigenic Sites`, 
         percent.id = `% Identity of Antigenic Site Residues`)

View(line_list_keep) 
View(line_list_keep_alt) # confirm the datasets are identical. We will only use one of these moving forward.

####2.2 ====================================================================================================================================================#
#The GISAID metadata also contains several columns we aren't interested in, such as the segment IDs.
  #Select columns from the raw GSAID metadata using column positions.
  #You can change the order of columns within select(). Let's put Isolate_Name (12) before Isolate_Id (1):

metadata_keep <- gisaid_metadata_raw %>% select(12, 1, 13, 16, 17, 19, 25, 28, 31:43) 

View(metadata_keep)

####2.3 ====================================================================================================================================================#
#Prepare to combine our two datasets:
  #First, compare the Isolate_Name columns to ensure they match. 
  #This is important because we are going to use this column as a "key" to join our datasets in the next step.

a <- line_list_keep$Isolate_Name %>% sort() #sort all isolate names alphabetically and store them in a variable called `a`
b <- metadata_keep$Isolate_Name %>% sort()
summary(a == b) #some column names do not match! 

# If we look at the Isolate_Name column in our dataframes (you can do this by manually looking at the data viewer or by printing `a` and `b` in the console),
# we can see that the names in `metadata_keep` contain spaces " ", whereas the `line_list_keep` names use underscores "_" instead of spaces.
# We can fix this using str_replace() on the Isolate_Name column specifically:

library(stringr)
metadata_keep$Isolate_Name <- metadata_keep$Isolate_Name %>% str_replace_all(" ", "_") #replace space characters (first argument) with underscores (second argument).

#Now, if we re-run the code chunk above (lines 80-82) to compare the columns, we see that all of the columns match, "TRUE"

####2.4 =======================================================================================================================================================#
#Join the two datasets on Isolate_Name, which is now the first column in each dataset. Since we are using the Isolate Names to join our tables, this will be our "key" variable.
  #Since we have the same # of rows in each dataset corresponding to the same samples, you can use any of left_, right_, inner_, or full_join() here.
  #Refer to "Mutating Joins" in the Combine Data Sets section of your cheatsheet for more info.

metadata_full <- full_join(metadata_keep, line_list_keep, by = "Isolate_Name") #"Isolate_Name" is our key variable that is used to link the two datasets.

View(metadata_full) #This combined dataset should have 41 rows and 24 columns (21 from metadata + 4 from line list) - 1(key) = 24.

### 3. Clean up our combined metadata so that it is easier to visualize: ======================================================================================#
#Create new columns for Collection_Year and Host_Age_Y, replace inconsistent values in Host_Gender column, 
  #and separate the Location column into 3 new columns Continent, Country, and Region.

library(tidyr)
metadata_clean <- metadata_full %>%
  mutate(Collection_Year = substr(Collection_Date, 1, 4), # Keep the first 4 characters (containing the year) from the Collection_Date column
         Host_Gender = if_else(Host_Gender %in% c("Male", "M"), "Male", 
                                 if_else(Host_Gender %in% c("Female", "F"), "Female", 
                                 if_else(is.na(Host_Gender) == FALSE, "Other", "NA"))),
         Host_Age_Y = if_else(Host_Age_Unit == "M", Host_Age / 12, Host_Age),
         .keep = "unused") %>% # setting .keep = "unused" deletes the `Collection_Date` column that we used to make the new column.
  separate(col = Location, c("Continent", "Country", "Region"), sep = "/")# Separates `Location` into 3 new columns, missing info is recorded as `NA`

View(metadata_clean)

###------------This section is optional: not included in video tutorial###-----------------------------------------------------------------------------------###

#If you want to further subset your data, I recommend saving the subset as a new dataset and preserving the original whole clean dataset:
  #For example, subset rows using filter() to include only samples from Swine or Human hosts:
metadata_mammal_only <- metadata_clean %>% filter(Host %in% c("Human", "Swine")) 
  # filters out 1 sample where Host = "Mallard" and saves the remaining rows in a new dataframe:`metadata_mammal_only`
  
#Filter out samples collected before a certain date: 
metadata_2019 <- metadata_clean %>% filter(!Collection_Year < 2019) #Do not (denoted by "!") keep samples from before 2019. 
                                #This is the same as filter(Collection_Year >= 2019), try it! 

#Keep 10 rows that have the most % identity with the reference sequence:
metadata_ref_id <- metadata_clean %>% top_n(10, percent.id)

#Check out the cheatsheet for more information on subsetting observations.

#---------------End of optional section-------------------------------------------------------------------------------------------------------------------###

### 4. Save our final dataset as a .csv file on our computer: ==================================================================================================#
#Reorder columns using select() to put all the isolate and host information together:

metadata_final <- metadata_clean %>% select(1:3, 21, 23, 7, 20, 25, 4:6, 24, 8:19)

View(metadata_final) #If you need one final check

write_csv(metadata_final, "YOUR-PATH/metadata-tidy.csv") #please replace YOUR-PATH with where you would like the file to be saved.
