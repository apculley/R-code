library(readxl)

# assigns the excel file path to a variable name path (not necessary, just saves typing it repeatedly)
path <-'/filepath/Labor Force Data by County All Years.xlsx'

#creates list of sheet names in the excel file
sheet_list<-excel_sheets(path)
print(sheet_list)

#reads in an individual sheet from an excel file, specifying by sheet number or name
temp1<-read_excel(path,sheet=9)
temp2<-read_excel(path,sheet="laucnty09")

#readxl is in tidyverse, so it reads in excel files as tibbles.  To convert to dataframe:
temp3<-as.data.frame(temp2)

#Read all sheets in as a list of tibbles
df_list<-lapply(sheet_list, read_excel, path = path)

#Read some in 
df_list<-lapply(sheet_list[1:10], read_excel, path = path)    

#after reading in as a list of tibbles, row bind a subset of the tibbles into a single tibble
scc<-do.call("rbind", df_list_2[4:19])

#multiple files:

# creates a list of the names of all the .xls files in the specified folder
xls_list<-dir(path=county_health_dir,pattern = "*.xls") 

# creates a list of the names of all the files whose name contain "health" in the specified folder
xls_list<-dir(path=county_health_dir,pattern = "*health") 

#iterates through the list of file names to read all the excel files into a list of tibbles
county_health_df_list_2<-lapply(xls_list, read_excel, sheet="Data", skip=1)

