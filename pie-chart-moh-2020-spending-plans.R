# Load the required packages
library(tidyverse)

url_data <- "https://raw.githubusercontent.com/mabdimalik/data/master/somalia-budget/somalia-fgs-budget-2020.csv"

# Load the fgs 2020 budget
budget <- read_csv(url(url_data))

# Subset budget to extract only adopted budget for MOH in 2020
moh_budget <- budget %>% filter(mda == "Ministry of Health", year == 2020)

# Let's find out total budget for ministry of health in 2020
total_budget <- sum(moh_budget$amount)

# Group moh_budget data into the main expenditure categories

# Let's collect together the main expense categories
employee_cost <- c("Allowances in cash", "Other employee costs", 
                   "Wages and salaries in cash", "Training expenses")

office_travel <- c("Office materials and other consumables", 
                     "Fuel and lubricants", "Repairs and maintenance",
                     "Rent","Utilities", "Travel expenses")
fixed_assets_others <- c("Bank commissions" , "Other fixed assets" , "Special operational services", "Other General Expenses")

consulting_fees <- "Consulting and professional fees"

# Pull it together - add new expense categories.
moh_expenses <- moh_budget %>% group_by(item) %>% 
    summarise(total_allocated=sum(amount)) %>% 
    mutate(main_expenses=case_when(item %in% office_travel ~ "Office & Travel",
                                    item %in% employee_cost ~ "Employee Cost", 
                                    item == consulting_fees ~ "Consulting Fees",
                                    item %in% fixed_assets_others ~ "Fixed Assets & Others")) %>% 
    group_by(main_expenses) %>% summarise(total_allocated=sum(total_allocated))

# Visualize the summary data using a pie chart
ggplot(data=moh_expenses, aes(x="", y=total_allocated, fill=main_expenses)) + 
    geom_bar(stat="identity", width = 1) +
    coord_polar("y", start = 0) + 
    geom_text(aes(label=paste0(round((total_allocated/total_budget)*100), "%")), 
              position = position_stack(vjust = 0.5), fontface="bold") +
    scale_fill_manual(values=c("#ed5e5f", "#5e97c6", "#80c87d", "#ff7f00"), name="Major Spendings:") +
    theme_void() + theme(legend.position = "bottom", 
                         plot.title = element_text(face="bold"), 
                         plot.subtitle = element_text(margin=margin(5,0,10,0))) +
    labs(x="", y="", 
         title="Proposed Spending Plan for MoH's US$13M,  2020", 
         subtitle = "Out of the allocated $ 13M, aroun, 71% (or 9.4M) is paid by the FGS.\nThe rest is donor funded through special projects.", 
         caption = "Source: FGS Budget 2020, MoF")

# Save the plot using the desired dimensions.
ggsave("moh-spending-plans-2020.png", width = 20, height = 20, units = "cm")
