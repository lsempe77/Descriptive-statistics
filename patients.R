patient_label <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_2235.csv")

patient_raw <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_2022-06-10_2235.csv")

patient_label <- patient_label %>% filter (!is.na(`II.  Household or participant name`))
