# Install and load the necessary packages
install.packages("systemfonts")
install.packages("flextable")
install.packages("officer")
library(dplyr)
library(systemfonts)
library(flextable)
library(officer)

# Create the data frame
data <- data.frame(
  Group = c("Total (n = 79)", "With PCS (n = 49)", "Without PCS (n = 30)"),
  Mean_Age = c(48.52, 50.29, 45.63),
  Age_Range = c("22-78", "22-78", "22-77"),
  Female_n = c(48, 32, 16),
  Male_n = c(31, 17, 14),
  Mean_Education = c(15.27, 15.04, 15.63),
  Education_Range = c("9-24", "9-23", "10-24"),
  Diverse_n = c(0, 0, 0)
)

# Create and format the flextable
ft <- flextable(data) %>%
  set_header_labels(
    Group = "Group",
    Mean_Age = "Mean Age (years)",
    Age_Range = "Age Range",
    Female_n = "Female (n)",
    Male_n = "Male (n)",
    Mean_Education = "Mean Education (years)",
    Education_Range = "Education Range",
    Diverse_n = "Diverse (n)"
  ) %>%
  theme_vanilla() %>%
  autofit()

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(value = ft)

# Save the document as a Word file
print(doc, target = "table_in_word.docx")
