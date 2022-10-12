library(jsonlite)
library(tidyverse)
library(udpipe)

###DATA INGESTION###

#Read the CSV file into a data frame
subjects <- read.csv("subjects.csv")

#Read the LCSH column as a JSON body in order to create an accessible list
subjects$LCSH <- lapply(subjects$LCSH, fromJSON)


###DATA CLEANUP###

#Filter the data frame to remove all rows that contain NULL; remove brackets and question marks from LCC column
subjects <- subjects %>% filter(LCC != "NULL", LCSH != "NULL")
subjects$LCC <- gsub("\\[|\\]|\\?", "", as.character(subjects$LCC))

#Separate LCC into indicators, call number, and cutter, and remove rows with invalid first character (numbers and lowercase letters)
subjects <- subjects %>% 
  separate("LCC", c("IND", "LCC"), sep = "\\$a") %>%   
  separate("LCC", c("LCC", "CUTTER"), sep = "\\$b") %>%
  filter(grepl("^[A-Z]", LCC))

#Create a column with the LC class
subjects <- subjects %>% mutate(CLASS = str_extract(LCC, regex("^[[:alpha:]]+")))

###DATA ORGANIZATION###

#Create a new data table with a column with the count of headings in the list in LCSH
subjects_group <- subjects %>% rowwise() %>% mutate(Headings = length(LCSH))

#Create a new data table to show unlisted LCSH; break headings into individual columns for sorting
subjects_ind <- unnest(subjects_group, LCSH)
subjects_ind <- subjects_ind %>% mutate(Subs = str_count(subjects_ind$LCSH, "\\$"))
subjects_ind <- subjects_ind %>% 
  separate("LCSH", c("Old", "Primary", "Sub1", "Sub2", "Sub3"), sep = "\\$") %>% filter(grepl("^a[A-Z]", Primary)) 

#Remove periods from all LCSH columns
sub_cols <- c("Primary", "Sub1", "Sub2", "Sub3")
subjects_ind[sub_cols] <- lapply(subjects_ind[sub_cols], gsub, pattern = "\\.$", replacement = "")

#Create a table showing the occurrences of individual main subject headings; display the top results
unique_table <- table(subjects_ind$Primary)
primary_unique <- as.data.frame(unique_table)
primary_unique <- primary_unique %>% arrange(desc(primary_unique$Freq))

#Create a table showing the occurrences of individual first subject subdivisions
unique_table2 <- table(subjects_ind$Sub1)
sub_unique <- as.data.frame(unique_table2) 
sub_unique <- sub_unique %>% filter(grepl("^x", Var1))
sub_unique <- sub_unique %>% arrange(desc(sub_unique$Freq))

#Calculate the average number of subject headings assigned by LC Class and count the number of bibs assigned to each LC Class 
average <- subjects_group %>% group_by(Class = substring(subjects_group$CLASS, 1, 1)) %>% 
  summarize(mean_head = mean(Headings))
count <- subjects_group %>% group_by(Class = substring(subjects_group$CLASS, 1, 1)) %>% 
  summarize(bib_count = n())
average_count_combine <- average %>%
  full_join(count, by = "Class", suffix = c("", "_count"))


###DATA ANALYSIS###

#Import list of identified LCSH for various people groups
lcsh <- read.csv("LCSH.csv", fileEncoding = "UTF-8-BOM")

#Identify records containing LCSH for people groups for comparison
selectAfAm <- subjects_ind %>% filter(Primary %in% lcsh$AfAm)
selectIndg <- subjects_ind %>% filter(Primary %in% lcsh$Indg)
selectLGBTQ <- subjects_ind %>% filter(Primary %in% lcsh$LGBTQ)
selectWomen <- subjects_ind %>% filter(Primary %in% lcsh$Women)

#Calculate the average number of subject headings assigned to records containing identified LCSH and compile into a single table for comparison
average_AfAm <- selectAfAm %>% group_by(BIB_ID) %>% group_by(Class = substring(selectAfAm$CLASS, 1, 1)) %>% summarize(mean_head = mean(Headings))
average_Indg <- selectIndg %>% group_by(BIB_ID) %>% group_by(Class = substring(selectIndg$CLASS, 1, 1)) %>% summarize(mean_head = mean(Headings))
average_LGBTQ <- selectLGBTQ %>% group_by(Class = substring(selectLGBTQ$CLASS, 1, 1)) %>% summarize(mean_head = mean(Headings))
average_Women <- selectWomen %>% group_by(Class = substring(selectWomen$CLASS, 1, 1)) %>% summarize(mean_head = mean(Headings))
h_avg <- average_count_combine %>%
  full_join(average_AfAm, by = "Class", suffix = c("", "_AfAm")) %>% 
  full_join(average_Indg, by = "Class", suffix = c("", "_Indg")) %>%
  full_join(average_LGBTQ, by = "Class", suffix = c("", "_LGBTQ")) %>% 
  full_join(average_Women, by = "Class", suffix = c("", "_Women"))
h_avg <- pivot_longer(h_avg, cols = 2:7, names_to = "LCSH", values_to = "Average")
h_avg$LCSH <- factor(h_avg$LCSH, levels=c("mean_head", "mean_head_AfAm", "mean_head_Indg", "mean_head_LGBTQ", "mean_head_Women"), labels=c("Average", "AfAm", "Indg", "LGBTQ", "Women"))

#Create graph of results
ggplot(na.omit(h_avg), aes(x = Class, y= Average, group = LCSH, color = LCSH)) +
  geom_line(size=1) +
  geom_point(size=1) +
  scale_color_manual(labels = c("Average", "African \nAmerican", "Indigenous", "LGBTQIA+", "Women"), values = c("Average" = "black", "AfAm" = "blue", "Indg" = "darkgreen", "LGBTQ" = "purple", "Women" = "red")) +
  theme_classic() +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5), legend.text=element_text(size=12, face="bold"), legend.position = "bottom", legend.title=element_blank(), axis.title=element_text(size=12,face="bold"), axis.text=element_text(size=10,face="bold")) +
  xlab("LC Class") +
  ylab("Average LCSH Assigned") +
  ggtitle("Avg. Number of LCSH per Bib by Class")


###CO-OCCURANCES OF SELECTED LCSH###

#Count individual occurrences of selected subject headings
countAfAm <- selectAfAm %>% group_by(Primary) %>% count(Primary)
head(countAfAm %>% arrange(desc(n)))
countIndg <- selectIndg %>% group_by(Primary) %>% count(Primary)
head(countIndg %>% arrange(desc(n)))
countLGBTQ <- selectLGBTQ %>% group_by(Primary) %>% count(Primary)
head(countLGBTQ %>% arrange(desc(n)))
countWomen <- selectWomen %>% group_by(Primary) %>% count(Primary)
head(countLGBTQ %>% arrange(desc(n)))

#Find co-occurrences of additional subject headings across bibliographic records
bibAfAm <- subset(subjects_ind, (BIB_ID %in% selectAfAm$BIB_ID))
coocAfAm <- cooccurrence(bibAfAm$Primary, group = "BIB_ID")
coocAfAm <- subset(coocAfAm, (as.character(term1) %in% lcsh$AfAm))
coocAfAm <- subset(coocAfAm, !(as.character(term2) %in% lcsh$AfAm))
bibIndg <- subset(subjects_ind, (BIB_ID %in% selectIndg$BIB_ID))
coocIndg <- cooccurrence(bibIndg$Primary, group = "BIB_ID")
coocIndg <- subset(coocIndg, (as.character(term1) %in% lcsh$Indg))
coocIndg <- subset(coocIndg, !(as.character(term2) %in% lcsh$Indg))
bibLGBTQ <- subset(subjects_ind, (BIB_ID %in% selectLGBTQ$BIB_ID))
coocLGBTQ <- cooccurrence(bibLGBTQ$Primary, group = "BIB_ID")
coocLGBTQ <- subset(coocLGBTQ, (as.character(term1) %in% lcsh$LGBTQ))
coocLGBTQ <- subset(coocLGBTQ, !(as.character(term2) %in% lcsh$LGBTQ))
bibWomen <- subset(subjects_ind, (BIB_ID %in% selectWomen$BIB_ID))
coocWomen <- cooccurrence(bibWomen$Primary, group = "BIB_ID")
coocWomen <- subset(coocWomen, (as.character(term1) %in% lcsh$Women))
coocWomen <- subset(coocWomen, !(as.character(term2) %in% lcsh$Women))


###GRAPHS###

#Plot to show distribution of the number of headings assigned to a bib record
ggplot((subjects_group %>% count(Headings)), aes(Headings, n)) +
  ggtitle("Plot of distribution of number of headings per bib record") +
  geom_bar(stat = 'identity')

#Plot to show distribution of the number of bibs by class
ggplot((subjects_group %>% group_by(Class = substring(subjects$CLASS, 1, 1)) %>% count(Class)), aes(Class, n)) +
  ggtitle("Plot of distribution of number bibs by class") +
  geom_bar(stat = 'identity')

#Line plot to show distribution of the number of bibs by class
ggplot(average, aes(x = Class, y= mean_head, group = 1)) +
  geom_line(stat = 'identity')
