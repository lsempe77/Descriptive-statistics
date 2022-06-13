library(tidyverse)
library(readxl)
library(forcats)
library(plotly)

# Page 1 = Health Facility general information -------------------

# Sys.Date()
# [1] "2022-06-10"

## Check duplicates -------

hfs <- read.csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_1841.csv") # downloaded page 1

hfs$health_facility<- gsub('[[:digit:]]+', '',hfs$X1..Facility.Information...Facility.name)

hfs$health_facility<- gsub('\t', '',hfs$health_facility)

hfs$District<- gsub('[[:digit:]]+', '',hfs$Health.district)

hfs$District<- str_trim(hfs$District)

hfs$District<- str_remove(hfs$District," District")

hfs$County<- gsub('[[:digit:]]+', '',hfs$County)

hfs$County<- str_remove(hfs$County,".")

hfs$County<- str_trim(hfs$County)

hfs %>% distinct(health_facility) # 56 distinct but # 67 observations

hfs <- hfs %>% rowwise() %>%
  mutate (OIC...Total = sum(c(OIC...Male,OIC...Female),na.rm = T),
          Second.Screener...Total = sum(c(Second.Screener....Male,Second.Screener....Female),na.rm = T),
          Lab.Tech...Total = sum(c(Lab.Tech...Male,Lab.Tech....Female),na.rm = T),
          CHSS...Total = sum(c(CHSS...Male,CHSS...Female),na.rm = T),
          CHA...Total = sum(c(CHA...Male,CHA...Female),na.rm = T),
          CHV...Total = sum(c(CHV...Male,CHV...Female),na.rm = T),
          Other...Total = sum(c(Other...Male,Other...Female),na.rm = T),
          Total = sum(c(OIC...Total,Second.Screener...Total,
                        Lab.Tech...Total,CHSS...Total,
                        CHA...Total,CHV...Total,Other...Total), na.rm = T))
  
repeated_hfs <- hfs %>% group_by(health_facility) %>% 
  tally () %>% 
  filter (n > 1) %>% pull (health_facility) # 22 observations, this mean 11 repeated. 

# 67 - 11 = should be 56!

hfs.unique<- hfs %>% distinct_at(vars(-Record.ID)) # 56 health facilities!! no more duplicated

## Data wrangling -------

# check if there are changes when data from Voinjama comes in


hfs.unique <- hfs.unique %>% 
  mutate (health_facility = case_when(health_facility == "Other" ~ "Kanneh community clinic",
                                      T ~ health_facility))

hfs.unique <- hfs.unique %>% 
  mutate (District = case_when(health_facility == "Trokon Glay Medical Clinic #" ~ "Mambah-Kaba",
                                      T ~ District))

hfs.unique <- hfs.unique %>% 
  mutate (County = case_when(District == "Mambah-Kaba" ~ "Margibi",
                             District == "Cavalla" ~ "Grand Gedeh",
                                      T ~ County))


hfs.unique<- hfs.unique %>% mutate (Type.of.facility..clinic..health.centre..hospital..etc..=
                                      str_squish(tolower(Type.of.facility..clinic..health.centre..hospital..etc..)
                                      )) 


hfs.unique <- hfs.unique %>% 
  mutate (Type.of.facility..clinic..health.centre..hospital..etc.. = case_when(health_facility == "Trokon Glay Medical Clinic #" ~ "clinic",
                                      T ~ Type.of.facility..clinic..health.centre..hospital..etc..))


#hfs.unique %>% group_by(District,County) %>% tally ()



## number of patients in ledgers

hfs.unique %>% ungroup () %>% 
  summarise (t=sum(X3...Number.of.NTD.patients.,na.rm = T)) # 46 patients over the period

hfs.unique %>% group_by(County,health_facility) %>% 
  summarise (t=sum(X3...Number.of.NTD.patients.,na.rm = T)) %>% 
  filter (t>0) %>% 
  arrange (-t) # 46 patients in 13 health facilities  - 43 health facilities do not have any ntd case in the last year


#still need to go to pictures and contrast


## Comparison to HMIS2 ------

Sampling_frame_draft_13_12_2021_From_Colleen <- read_excel("C:/Users/lucas/Desktop/Desktop/QMU/REDRESS/Sampling frame draft_13.12.2021_From Colleen.xlsx")

dd<-hfs.unique %>% 
  select(health_facility,X3...Number.of.NTD.patients.,Health.district) %>% 
  mutate(health_facility = case_when(health_facility=="Trokon Glay Medical Clinic #"  ~ 
                                       "Trokon Glay Medical Clinic #1",
                                     T ~ health_facility)) %>%
  left_join(Sampling_frame_draft_13_12_2021_From_Colleen,by=c("health_facility"="Facility name")) %>%
  select(County,health_facility,District,X3...Number.of.NTD.patients.,`SSSD cases 2020 (total)`, `SSSD cases 2019 (total)`) %>%
  rename (Baseline_REDRESS=X3...Number.of.NTD.patients.) %>%
  mutate(Baseline_REDRESS = ifelse(is.na(Baseline_REDRESS), 0, Baseline_REDRESS),
         hmis20_19 = (`SSSD cases 2020 (total)` + `SSSD cases 2019 (total)`))


dd <- dd %>% 
  mutate (District = case_when(health_facility=="Kanneh community clinic"  ~ "Tchien",
                              T ~ District),
          County = case_when(is.na(County) ~ "Grand Gedeh",
                             T ~ County) ) %>%
 rename (HF=health_facility) %>%  relocate(District, .before = HF)

dd %>%    arrange(Baseline_REDRESS) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(HF=factor(HF, levels=HF)
         ) %>%
    ggplot() + 
  geom_point(aes (HF,hmis20_19,colour="HMIS 2019+2020"),
             position=position_jitter(width = 0.1))+
  geom_point(aes (HF,Baseline_REDRESS,colour="Baseline_REDRESS")) + theme_light() + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~County, scales = "free_x") + ylab ("") + xlab ("") + 
  labs(colour="Data") + 
  theme (legend.position = "bottom")


## hw

# still need to compare number of health workers reported in page 1 and information from HMIS and mails from colleagues





## Descriptive analysis -----

hfs.unique %>% ungroup() %>%
  summarise (phq=sum(X4...Number.of.patients.with.NTD.who.have.PHQ.9.score.in.last.month,na.rm = T),
             gad=sum(X5...Number.of.patients.with.NTD.who.have.GAD7.score.in.last.month,na.rm = T),
        psych_sup=sum (X6...Number.of.people.with.NTD.who.received.psychosocial.support.from.mid.level.health.workers.in.the.last.month, na.rm = T),
       mhgap = sum(X7...Number.of.people.with.NTD.who.were.referred.to.MHGAP.clinician.in.the.last.month, na.rm = T),                    
       reffered_next_level = sum(X8...Number.of.people.with.NTD.referred.to.the.next.level.of.health.facility.in.the.last.month, na.rm = T))

table(hfs.unique$X2..Is.the.facility.providing.surgical.procedure.for.hydrocele.)

table (hfs.unique$Type.of.facility..clinic..health.centre..hospital..etc..)

###

hfs.unique %>% group_by(County,District) %>% 
  summarise (OIC= sum(OIC...Total, na.rm = T),
             Second_Screener= sum(Second.Screener...Total, na.rm = T),
             Lab_tech= sum(Lab.Tech...Total, na.rm = T),
             CHSS= sum(CHSS...Total, na.rm = T),
             CHV= sum(CHV...Total, na.rm = T),
             Other=sum(Other...Total, na.rm = T),
             Total = sum (Total),
             NTD_cases = sum (X3...Number.of.NTD.patients., na.rm = T)
  ) 

hws<-hfs.unique %>% group_by(County,District) %>% 
  summarise (OIC= sum(OIC...Total, na.rm = T),
             Second_Screener= sum(Second.Screener...Total, na.rm = T),
             Lab_tech= sum(Lab.Tech...Total, na.rm = T),
             CHSS= sum(CHSS...Total, na.rm = T),
             CHV= sum(CHV...Total, na.rm = T),
             Other=sum(Other...Total, na.rm = T),
             Total = sum (Total),
             NTD_cases = sum (X3...Number.of.NTD.patients., na.rm = T)
  ) 

cor.test(hws$Total,hws$NTD_cases)

hfs.unique %>% group_by(health_facility) %>% 
  summarise (OIC= sum(OIC...Total, na.rm = T),
             Second_Screener= sum(Second.Screener...Total, na.rm = T),
             Lab_tech= sum(Lab.Tech...Total, na.rm = T),
             CHSS= sum(CHSS...Total, na.rm = T),
             CHV= sum(CHV...Total, na.rm = T),
             Other=sum(Other...Total, na.rm = T),
             Total = sum (Total),
             NTD_cases = sum (X3...Number.of.NTD.patients., na.rm = T)
  ) %>% 
  arrange (-NTD_cases) %>% 
  print (n= 56)

# Page 2 - Patient data from health facility ledgers -----

### there is still a problem with health workers data - chris is on it

page2 <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_2045.csv")

page2$health_facility<- gsub('[[:digit:]]+', '',page2$`1.  Facility name`)

page2$health_facility<- gsub('\t', '',page2$health_facility)

page2$District<- gsub('[[:digit:]]+', '',page2$`5.  Health district`)

page2$District<- str_remove(page2$District,".")

page2$District<- str_trim(page2$District)

page2$District<- str_remove(page2$District," District")

districts<-hfs %>% distinct (County,District) 

districts<- districts%>% filter (County!="")

page2<-page2 %>% left_join(districts)

page2 %>% distinct(health_facility) # 41 health facilities

variable.names(page2)[c(1,62,8,20:25,63,64)]

p2<-page2 %>% select(c(1,62,8,20:25,63,64)) %>% # filtering by empty sex
  pivot_longer(cols = 4:9) %>% 
  mutate (
    value1 = case_when(value == "Checked" ~ "yes",
                       T ~ NA_character_)) %>% 
  group_by(`3.  Sex`,name,value1,health_facility,County,
           District) %>% 
  tally () %>% 
  filter (!is.na(value1)) 

p2<-p2 %>% mutate (District = case_when(health_facility=="Tuzon Clinic (Cavalla)" ~ "Cavalla",
                                        health_facility=="Tellewoyan Memorial Hospital" ~ "Voinjama",
                                    T ~ District),
               County = case_when(health_facility=="Tuzon Clinic (Cavalla)" ~ "Grand Gedeh",
                                  health_facility=="Tellewoyan Memorial Hospital" ~ "Lofa",
                                    T ~ County))

tofind <- paste(c("Leprosy","Buruli","Yaws","Onchocerciasis","Hydrocele"), collapse="|")

p2<-p2 %>% mutate (Disease=  str_extract(name, tofind),
                   ) %>% rename (Sex=`3.  Sex`)

 
  



page2 %>% select(c(1,62,8,20:25,63,64)) %>% # filtering by empty sex
  pivot_longer(cols = 4:9) %>% 
  mutate (
    value1 = case_when(value == "Checked" ~ "yes",
                       T ~ NA_character_)) %>% 
  filter (!is.na(value1)) %>%
  ungroup() %>% 
  tally () 




# 42 cases


# page 3 is about lab

page3 <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_2158.csv") # page 3

variable.names (page3)[1:12]

# there is no data!!!

page3 <- page3 %>% filter (`26. Did this patient have specimen(s) collected{Only show rest of survey if this is YES}` == "A. Yes")

page3 %>% filter (!is.na(`26.1  Date first specimen(s) taken`))
