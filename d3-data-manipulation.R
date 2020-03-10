# Load the libraries
library(tidyverse)
library(readxl)

excel_data <- read_excel("DHIS2_DATA_2018_Analysis.xlsx", sheet = 1)

# Load EPI Data 2017 - 2019
epi <- read.csv("so-dhis-epi-2017-9.csv", stringsAsFactors = F)

# Examine the data
names(epi) # review column names

dim(epi) # Data dimensions i.e. number of rows and columns

str(epi) # Check the data structure in base R 

glimpse(epi) # Check the data structure using tidyverse - glimpse

head(epi, 10) # Check the first few rows

tail(epi) # check the bottom few rows

# Part 1 - Data Indexing and Subsetting

# Extract a single column using the dollar sign 

district <- epi$District
View(district)

# Selecting a column by index number 
district2 <- epi[,1]
region2 <- epi[,2]

# Selecting rows by index number
row1 <- epi[1,]

# Select Multiple Columns
col1n2 <- epi[,1:2]

# Select Multiple Columns - Example 2
col1n3 <- epi[, c(1,3,5)]

# Select multiple rows sequence
multirows_seq <- epi[200:250,]

# Select Multiple Rows Using C()
multi_rows <- epi[c(1,10,30, 40, 50, 1001, 2000),]


# Example multiple rows and columns
first200 <- epi[1:200, 3:5] 
first201 <- epi[1:200, c(3,4,5)]


# Comparison and Logical Operators


# > Greater than Symbol
# < Less than Symbol
# == Equal to 
# >= Equal to or Greater than
# <= Less than or Equal
# != Not equal to
# %in% Value Matching
# | OR
# & AND

# Examples Greater Than
2 > 3
5 > 1



# Example of Less Tnan

14 < 10
-4 < -8

# Equaity Operator

4 == 4

-5 == 5

"Ali" == "Ali"

TRUE == FALSE


14 >= 10

14 <= 10

14 != 10

TRUE | FALSE | TRUE


TRUE & FALSE

c("A", "B", "C", "D") %in% "X"


# Lets Use the Comparison Operators to Select and Extract Data
# ----------------------------
bcg <- epi[epi$Vaccine == "BCG", ]


bcg_2018 <- epi[epi$Vaccine == "BCG" & epi$Year == 2018,]

# Visualize Using a Bar Chart
ggplot(data=bcg_2018, aes(x=reorder(Region,Dose), y=Dose)) + 
  geom_col() + coord_flip() + 
  ggtitle("BCG 2018 by Region", 
          subtitle = "Banadir is the largest consumer of BCG vaccines.") + 
  xlab("") + 
  labs(caption = "Source: EPI Data FGSMoH")

# Visualize Many Years Using Faceting
ggplot(data=bcg, aes(x=reorder(Region,Dose), y=Dose)) + 
  geom_col() + coord_flip() + 
  facet_wrap(~Year) +
  ggtitle("BCG 2018 by Region", 
          subtitle = "Banadir is the largest consumer of BCG vaccines.") + 
  xlab("") + 
  labs(caption = "Source: EPI Data FGSMoH")

# Extract A section of the data using Value Matching
opv <- epi[epi$Vaccine %in% c("OPV_1", "OPV_2", "OPV_3"),]


# Visualization Example 2
ggplot(data=opv, aes(x=reorder(Region,Dose), y=Dose)) + 
  geom_col() + coord_flip() + 
  facet_wrap(Vaccine~Year) +
  ggtitle("OPV by Region, 2018-2019", 
          subtitle = "Banadir is the largest consumer of BCG vaccines.") + 
  xlab("") + 
  labs(caption = "Source: EPI Data FGSMoH") + theme_minimal()

# Visualization Example 3
ggplot(data=opv[opv$Year==2018 & opv$Region == "Banadir",], aes(x=Vaccine, y=Dose, fill=Vaccine)) + 
  geom_col() +
  facet_wrap(~District, scales = "free_y") +
  ggtitle("OPV by Region, 2018-2019", 
          subtitle = "Banadir is the largest consumer of BCG vaccines.") + 
  xlab("") + 
  labs(caption = "Source: EPI Data FGSMoH") + 
  theme_minimal() + 
  theme(legend.position = "top")








