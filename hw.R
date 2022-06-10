REDRESS_DATA <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_2022-06-10_2231.csv")

hw_tool <- read_csv("C:/Users/lucas/Desktop/RedCap/REDRESS_DATA_LABELS_2022-06-10_2231.csv")

variable.names(hw_tool)

variable.names(REDRESS_DATA)


# still need to disaggregate on topics from 1 to 37


##

supervisor <- REDRESS_DATA[37:50] %>% 
  pivot_longer(cols = ends_with("_sc"),names_to = "percent_scale",values_to = "percent_value")

supervisor <-   supervisor %>% pivot_longer(-c(percent_scale,percent_value),names_to = "original_scale",
                                            values_to = "original_value")

supervisor <- supervisor %>%
  filter(str_detect(percent_scale, original_scale )) %>% 
  mutate(quest=str_extract(original_scale, "[0-9]{2}")) %>%
  filter (original_value<80)%>% filter(percent_value<=100)


supervisor %>% 
  ggplot() + 
  geom_histogram(aes(percent_value,fill="percent"),alpha=.5) + 
  facet_wrap(~quest) + geom_histogram(aes(original_value*20,fill="original"),alpha=.5)+
  scale_x_continuous(sec.axis = ~ 1+./25) + theme_light() + 
  scale_fill_discrete(name = "Measurement of questions")+
  ggtitle("Questions about supervision")


cor.test(supervisor$original_value, supervisor$percent_value, method = "kendall")

cor.test(supervisor$original_value, supervisor$percent_value, method = "spearman",
         exact = F)


# care and training?

REDRESS_DATA[51:58]


##

stigma <- REDRESS_DATA[59:72] %>%  mutate_if(is.character, as.numeric, na.rm = TRUE) %>%
  pivot_longer(cols = ends_with("_sc"),names_to = "percent_scale",values_to = "percent_value")

stigma <-   stigma %>% pivot_longer(-c(percent_scale,percent_value),names_to = "original_scale",
                                    values_to = "original_value")  %>%
  filter(str_detect(percent_scale, original_scale ))


stigma <- stigma %>% filter (original_value<80)%>%filter(percent_value<=100)%>%
  mutate(ov = case_when(original_value ==1 ~ 3,
                        original_value == 2~ 2,
                        original_value == 3~ 1,
                        original_value ==4~0))

table(stigma$original_value)

table(stigma$percent_value)


stigma %>% filter(percent_value<=100)%>%
  ggplot() + 
  geom_histogram(aes(percent_value,fill="percent"),alpha=.5) + 
  facet_wrap(~original_scale,scales = "free_y") + geom_histogram(aes(ov*33,fill="original"),alpha=.5)+
  theme_light() +   scale_x_continuous(sec.axis = ~ 1+./33) +
  scale_fill_discrete(name = "Measurement of questions")+
  ggtitle("Questions about stigma")


cor.test(stigma$ov, stigma$percent_value, method = "spearman",exact = F)

cor.test(stigma$ov, stigma$percent_value, method = "kendall")



##

social_distance <- REDRESS_DATA[73:94] %>%  mutate_if(is.character, as.numeric, na.rm = TRUE) %>%
  pivot_longer(cols = ends_with("_sc"),names_to = "percent_scale",values_to = "percent_value")

social_distance <-   social_distance %>% pivot_longer(-c(percent_scale,percent_value),names_to = "original_scale",
                                    values_to = "original_value")  %>%
  filter(str_detect(percent_scale, original_scale ))


social_distance <- social_distance %>% filter (original_value<80)%>%filter(percent_value<=100)%>%
  mutate(ov = case_when(original_value ==1 ~ 3,
                        original_value == 2~ 2,
                        original_value == 3~ 1,
                        original_value ==4~0))

table(social_distance$original_value)

table(social_distance$percent_value)


social_distance %>% filter(percent_value<=100)%>%
  ggplot() + 
  geom_histogram(aes(percent_value,fill="percent"),alpha=.5) + 
  facet_wrap(~original_scale,scales = "free_y") + geom_histogram(aes(ov*33,fill="original"),alpha=.5)+
  theme_light() +   scale_x_continuous(sec.axis = ~ 1+./33) +
  scale_fill_discrete(name = "Measurement of questions")+
  ggtitle("Questions about social_distance")


cor.test(social_distance$ov, social_distance$percent_value, method = "spearman",exact = F)

cor.test(social_distance$ov, social_distance$percent_value, method = "kendall")


# costs

REDRESS_DATA[95:105]



# 3 different scales here: satisfaction, orgcom, workcon

REDRESS_DATA[106:117]


