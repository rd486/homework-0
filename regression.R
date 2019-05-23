library(tidyverse)
library(dslabs)

data(admissions)
admissions %>% group_by(gender) %>%
  summarize(percentage =
              round(sum(admitted*applicants)/sum(applicants),1))
#round(x,1) means to keep 1 decimal

admissions %>% group_by(major) %>%
  summarize(major_select = sum(admitted*applicants)/sum(applicants),
            #(gender == "women") returns 1 if gender is women otherwise returns 0
            percent_women = sum(applicants*(gender == "women")/sum(applicants))*100) %>%
  ggplot(aes(major_select, percent_women, label = major)) +
  geom_text()
#major E and F are hard and women are more likely to apply to these two

admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

#major is the confounder, therefore we can condidtion or stratify by major
admissions %>%
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()