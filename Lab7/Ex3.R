#working with xl files
library(readxl) #importing the library
d=read_excel('/home/ibab/Downloads/pone.0148733.s001.xlsx',1) #loading the dataset
#Print the column names and dimensions of the data in the data data frame
print(names(d))
print(dim(d))
