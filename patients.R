patient_label <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_2235.csv")

patient_raw <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_2022-06-10_2235.csv")

patient_label <- patient_label %>% filter (!is.na(`II.  Household or participant name`))

variable.names(patient_label)
patient_label %>% group_by()

# table(patient_label$`12.  Which of these diseases are you affected by?`)
# table(patient_label$`13. Which of these diseases are your charges affected by?`)
# 
# glimpse(patient_label)
# 
# 
# table(patient_label$ `I.  Patient Hospital ID`)
