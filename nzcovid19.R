library(readxl)
library(tidyverse)
library(plyr)
library(reshape2)
library(dplyr)

april8 <- read_excel("~/Desktop/NZ Covid19 data/covid-casedetails-9april2020.xlsx")
data <- april8 %>% mutate(Date_of_report=as.Date(Date_of_report), Sex = factor(Sex), Age_group = factor(Age_group))

#calculate confirm cases and probably cases each day
number_confirm <- ddply(data, "Date_of_report", summarise, confirmed=sum(Confirmed), probable=sum(Probable), confirmedandprobable=sum(Confirmed)+sum(Probable))
select <- dplyr::select

#use reshape to make category
new_confirm_table <- number_confirm %>% select(-confirmedandprobable) %>% melt(id.vars = "Date_of_report", variable.name = "confirmedorprobable", value.name = "cases_number")

write.table(number_confirm, file = "~/Desktop/NZ Covid19 data/number_confirm.txt")


#barchart of each day cases
number_confirm %>% ggplot(aes(x=Date_of_report, y=confirmedandprobable, fill=confirmed)) + geom_bar(stat = "identity") + geom_text(aes(label=confirmedandprobable, vjust=-0.5)) + scale_x_date(date_breaks = "1 week")
p1 <- new_confirm_table %>% ggplot(aes(x=Date_of_report, y=cases_number, fill=confirmedorprobable)) + geom_bar(stat = "identity") + scale_x_date(date_breaks = "1 week") + guides(fill=guide_legend(reverse=TRUE)) 
p1 + ggtitle("New confirmed and probable cases") + theme(plot.title = element_text(size=24, vjust = -6))

#barchart of age and sex
data %>% select(Sex, Age_group) %>% mutate(Sex = reorder(Sex, Age_group)) %>% ggplot(aes(Age_group, color=Sex)) + geom_bar()



#total cases by DHB
data %>% ggplot(aes(DHB, fill=DHB)) + geom_bar()
data %>% ggplot(aes(DHB, y=..count.., fill=DHB)) + geom_density(alpha=0.2)

#positive rate
positive_rate_2 <- read_excel("~/Desktop/NZ Covid19 data/positive rate_2.xlsx")
positive_rate_table <- positive_rate_2 %>% mutate(Date=as.Date(Date), Tests_perday = `Tests per Day`, positive_rate = `Positive rate`)
p2 <- positive_rate_table %>% ggplot(aes(x=Date, y=Tests_perday)) + geom_line(stat = "identity") + geom_point(size=2, color="red") + geom_text(aes(label=Tests_perday, vjust=-1.5))
p2 + scale_x_date(date_breaks = "3 day") + ggtitle("Nucleic acid tests per day") + theme(plot.title = element_text(size=24, vjust = -6))

p3 <- positive_rate_table %>% ggplot(aes(x=Date, y=positive_rate * 100)) + geom_line(stat = "identity") + geom_point(size=2, color="blue") 
p3 + ylim(0, 8) + scale_x_date(date_breaks = "3 day") + ylab("Positive rate (%)") + ggtitle("Positive rate per day") + theme(plot.title = element_text(size=24, vjust = -6))
