library(tidyverse)
library(dplyr)
#################### Clean Data ##########################
#load csvs
library(readr)
w1 <- readr::read_csv("Week1.csv")
w2 <- readr::read_csv("Week2.csv")
w3 <- readr::read_csv("Week3.csv")
w4 <- readr::read_csv("Week4.csv")
View(w1)
#merge csvs into master
master <- merge(w1,w2,by="participant_id")
master <- merge(master,w4,by="participant_id")
master <- merge(master,w3,by="participant_id")
 
  
#select ethnicity columns
demo <- select(master,1,7:11,18:22,31:35)



#combine columns into one using paste function, trimming leading whitespace
demo <- demo %>%
  mutate(across(everything(),~replace_na(.,""))) %>%
  mutate(E_1 = trimws(paste(Ethnicity_1,Ethnicity_1.x,Ethnicity_1.y)),"l") %>%
  mutate(E_2 = trimws(paste(Ethnicity_2,Ethnicity_2.x,Ethnicity_2.y)),"l") %>%
  mutate(E_3 = trimws(paste(Ethnicity_3,Ethnicity_3.x)),"l") %>%
  mutate(E_4 = trimws(paste(Ethnicity_4,Ethnicity_4.x,Ethnicity_4.y)),"l") %>%
  mutate(E_5 = trimws(paste(Ethnicity_5,Ethnicity_5.y)),"l") %>%
  mutate(Ethnicity = trimws(paste(E_1,E_2,E_3,E_4,E_5)),"l") %>%
  select(participant_id,Ethnicity)

#Create vectors for region
West <- c("WA","OR","CA","MT","ID","WY","NV","UT","CO","AZ","NM")
Midwest <- c("IL", "IN", "OH", "MI", "WI", "MN", "ND", "SD", "NE", "KS", "MO", "IA")
South <- c("DC", "DE", "MD", "WV", "VA", "KY", "TN", "NC", "SC", "GA", "FL", "AL", "MS", "LA", "AR", "OK", "TX")
Northeast <- c("PA", "NJ", "NY", "CT", "MA", "VT", "RI", "NH", "ME")

#Change states to region

demo2 <- select(master,1,Region.y,Region.x,Region) %>%
  mutate(R1 = case_when(
    (Region %in% West) ~ "West",
    (Region %in% Midwest) ~ "Midwest",
    (Region %in% South) ~ "South",
    (Region %in% Northeast) ~ "Northeast")) %>%
  mutate(R2 = str_split(Region.y, " \\(", simplify = TRUE)[,1]) %>%
  mutate(across(c(R1,R2,Region.x),~replace_na(.,""))) %>%
  mutate(Region = trimws(paste(R1,R2,Region.x),"b")) %>%
  select(participant_id,Region)

#Adjust age groups
today = as.Date("2020-03-31", format="%Y-%m-%d")
library(lubridate)
demo3 <- select(master,1,Age.x,Age.y,29) %>%
  mutate(dob = as.Date(master$`Date of Birth`, format = "%Y-%m-%d")) %>%
  mutate(a1 = as.integer(time_length(difftime(today, dob), "years"))) %>%
  mutate(across(c(Age.y,a1),~replace_na(.,""))) %>%
  mutate(a2 = as.integer(paste(a1,Age.y))) %>%
  mutate(Age = case_when(
    (a2 < 18) ~ "Under 18",
    (a2 > 18 & a2 < 30) ~ "18-29",
    (a2 > 29 & a2 < 40) ~ "30-39",
    (a2 > 39 & a2 < 50) ~ "40-49",
    (a2 > 49 & a2 < 60) ~ "50-59",
    (a2 > 59 & a2 < 70) ~ "60-69",
    (a2 > 69) ~ "70+"
  )) %>%
  mutate(across(c(Age,Age.x),~replace_na(.,"")))  %>%
  mutate(Age = trimws(paste(Age,Age.x),"b")) %>%
  select(participant_id,Age)

#Combine gender columns
demo4 <- select(master,participant_id,Gender,Gender.x,Gender.y) %>%
  mutate(across(everything(),~replace_na(.,""))) %>%
  mutate(Gender = trimws(paste(Gender,Gender.x,Gender.y),"b")) %>%
  select(participant_id, Gender)

#Combine all demographics
demographics <- merge(demo,demo2)
demographics <- merge(demographics,demo3)
demographics <- merge(demographics,demo4)

#Select master without demographic so that we can add our combined demographics
questions <- select(master,!c(5:12,16:23,29:36))

positivemoods <- c("Relaxed","Peaceful","Optimistic","Happy","Excited","Confident")
negativemoods <- c("Stressed/Worried","Anxious","Pessimistic","Overwhelmed","Bored")
master2 <- merge(questions,demographics)

master2 <- master2 %>%
  mutate(across(c(Q1_Mood_Week1,Q1_Mood_Week2,Q1_Mood_Week3,Q1_Mood_Week4),~str_split(., " ", simplify = TRUE)[,1])) %>%
  mutate(Q1_Mood_Week1 = gsub("Stressed", "Stressed/Worried",Q1_Mood_Week1)) %>%
  dplyr::rename(House_PrePurchase = Q8_Pre_Covid_Purchase_1) %>%
  dplyr::rename(Car_PrePurchase = Q8_Pre_Covid_Purchase_2) %>%
  dplyr::rename(Empty_PrePurchase = Q8_Pre_Covid_Purchase_3) %>%
  dplyr::rename(RecV_PrePurchase = Q8_Pre_Covid_Purchase_4) %>%
  dplyr::rename(TV_PrePurchase = Q8_Pre_Covid_Purchase_5) %>%
  dplyr::rename(PC_PrePurchase = Q8_Pre_Covid_Purchase_6) %>%
  dplyr::rename(Software_PrePurchase = Q8_Pre_Covid_Purchase_7) %>%
  dplyr::rename(Cell_PrePurchase = Q8_Pre_Covid_Purchase_8) %>%
  dplyr::rename(Audio_PrePurchase = Q8_Pre_Covid_Purchase_9) %>%
  dplyr::rename(Gaming_PrePurchase = Q8_Pre_Covid_Purchase_10) %>%
  dplyr::rename(Appliances_PrePurchase = Q8_Pre_Covid_Purchase_11) %>%
  dplyr::rename(Cosmetics_PrePurchase = Q8_Pre_Covid_Purchase_12) %>%
  dplyr::rename(Furniture_PrePurchase = Q8_Pre_Covid_Purchase_13) %>%
  dplyr::rename(Jewelry_PrePurchase = Q8_Pre_Covid_Purchase_14) %>%
  dplyr::rename(Pets_PrePurchase = Q8_Pre_Covid_Purchase_15) %>%
  dplyr::rename(Apparel_PrePurchase = Q8_Pre_Covid_Purchase_16) %>%
  dplyr::rename(Handbag_PrePurchase = Q8_Pre_Covid_Purchase_17) %>%
  dplyr::rename(Reno_PrePurchase = Q8_Pre_Covid_Purchase_18) %>%
  dplyr::rename(Fitness_PrePurchase = Q8_Pre_Covid_Purchase_19) %>%
  dplyr::rename(Other_PrePurchase = Q8_Pre_Covid_Purchase_20) %>%
  dplyr::rename(None_PrePurchase = Q8_Pre_Covid_Purchase_21) %>%
  dplyr::rename(Trip_PrePurchase = Q8_Pre_Covid_Purchase_22) %>%
  dplyr::rename(House_PostPurchase = Q9_Postpone_Purchase_1) %>%
  dplyr::rename(Car_PostPurchase = Q9_Postpone_Purchase_2) %>%
  dplyr::rename(Empty_PostPurchase = Q9_Postpone_Purchase_3) %>%
  dplyr::rename(RecV_PostPurchase = Q9_Postpone_Purchase_4) %>%
  dplyr::rename(TV_PostPurchase = Q9_Postpone_Purchase_5) %>%
  dplyr::rename(PC_PostPurchase = Q9_Postpone_Purchase_6) %>%
  dplyr::rename(Software_PostPurchase = Q9_Postpone_Purchase_7) %>%
  dplyr::rename(Cell_PostPurchase = Q9_Postpone_Purchase_8) %>%
  dplyr::rename(Audio_PostPurchase = Q9_Postpone_Purchase_9) %>%
  dplyr::rename(Gaming_PostPurchase = Q9_Postpone_Purchase_10) %>%
  dplyr::rename(Appliances_PostPurchase = Q9_Postpone_Purchase_11) %>%
  dplyr::rename(Cosmetics_PostPurchase = Q9_Postpone_Purchase_12) %>%
  dplyr::rename(Furniture_PostPurchase = Q9_Postpone_Purchase_13) %>%
  dplyr::rename(Jewelry_PostPurchase = Q9_Postpone_Purchase_14) %>%
  dplyr::rename(Pets_PostPurchase = Q9_Postpone_Purchase_15) %>%
  dplyr::rename(Apparel_PostPurchase = Q9_Postpone_Purchase_16) %>%
  dplyr::rename(Handbag_PostPurchase = Q9_Postpone_Purchase_17) %>%
  dplyr::rename(Reno_PostPurchase = Q9_Postpone_Purchase_18) %>%
  dplyr::rename(Fitness_PostPurchase = Q9_Postpone_Purchase_19) %>%
  dplyr::rename(Other_PostPurchase = Q9_Postpone_Purchase_20) %>%
  dplyr::rename(None_PostPurchase = Q9_Postpone_Purchase_21) %>%
  dplyr::rename(Trip_PostPurchase = Q9_Postpone_Purchase_22) %>%
  mutate(across(everything(),~replace_na(.,""))) %>%
  mutate(Mood_Week1 = case_when(
    (Q1_Mood_Week1 %in% positivemoods)~ "Positive 😃",
    (Q1_Mood_Week1 %in% negativemoods)~ "Negative 😓"
  )) %>%
  mutate(Mood_Week2 = case_when(
    (Q1_Mood_Week2 %in% positivemoods)~ "Positive 😃",
    (Q1_Mood_Week2 %in% negativemoods)~ "Negative 😓"
  )) %>%
  mutate(Mood_Week3 = case_when(
    (Q1_Mood_Week3 %in% positivemoods)~ "Positive 😃",
    (Q1_Mood_Week3 %in% negativemoods)~ "Negative 😓"
  )) %>%
  mutate(Mood_Week4 = case_when(
    (Q1_Mood_Week4 %in% positivemoods)~ "Positive 😃",
    (Q1_Mood_Week4 %in% negativemoods)~ "Negative 😓"
  )) %>%
  mutate(Q1_Mood_Week1 = case_when(
    (Q1_Mood_Week1 == "Anxious")~ "Anxious 😰",
    (Q1_Mood_Week1 == "Stressed/Worried")~ "Stressed/Worried 😓",
    (Q1_Mood_Week1 == "Bored")~ "Bored 🥱",
    (Q1_Mood_Week1 == "Happy")~ "Happy 😃",
    (Q1_Mood_Week1 == "Optimistic")~ "Optimistic 👍",
    (Q1_Mood_Week1 == "Relaxed")~ "Relaxed 💆‍♀‍",
    (Q1_Mood_Week1 == "Confident")~ "Confident 😎",
    (Q1_Mood_Week1 == "Peaceful")~ "Peaceful 😊",
    (Q1_Mood_Week1 == "Pessimistic")~ "Pessimistic 😕",
    (Q1_Mood_Week1 == "Excited")~ "Excited 😄",
    (Q1_Mood_Week1 == "Overwhelmed")~ "Overwhelmed 😫"
  )) %>%
  mutate(Q1_Mood_Week2 = case_when(
    (Q1_Mood_Week2 == "Anxious")~ "Anxious 😰",
    (Q1_Mood_Week2 == "Stressed/Worried")~ "Stressed/Worried 😓",
    (Q1_Mood_Week2 == "Bored")~ "Bored 🥱",
    (Q1_Mood_Week2 == "Happy")~ "Happy 😃",
    (Q1_Mood_Week2 == "Optimistic")~ "Optimistic 👍",
    (Q1_Mood_Week2 == "Relaxed")~ "Relaxed 💆‍♀‍",
    (Q1_Mood_Week2 == "Confident")~ "Confident 😎",
    (Q1_Mood_Week2 == "Peaceful")~ "Peaceful 😊",
    (Q1_Mood_Week2 == "Pessimistic")~ "Pessimistic 😕",
    (Q1_Mood_Week2 == "Excited")~ "Excited 😄",
    (Q1_Mood_Week2 == "Overwhelmed")~ "Overwhelmed 😫"
  )) %>%
  mutate(Q1_Mood_Week3 = case_when(
    (Q1_Mood_Week3 == "Anxious")~ "Anxious 😰",
    (Q1_Mood_Week3 == "Stressed/Worried")~ "Stressed/Worried 😓",
    (Q1_Mood_Week3 == "Bored")~ "Bored 🥱",
    (Q1_Mood_Week3 == "Happy")~ "Happy 😃",
    (Q1_Mood_Week3 == "Optimistic")~ "Optimistic 👍",
    (Q1_Mood_Week3 == "Relaxed")~ "Relaxed 💆‍♀‍",
    (Q1_Mood_Week3 == "Confident")~ "Confident 😎",
    (Q1_Mood_Week3 == "Peaceful")~ "Peaceful 😊",
    (Q1_Mood_Week3 == "Pessimistic")~ "Pessimistic 😕",
    (Q1_Mood_Week3 == "Excited")~ "Excited 😄",
    (Q1_Mood_Week3 == "Overwhelmed")~ "Overwhelmed 😫"
  )) %>%
  mutate(Q1_Mood_Week4 = case_when(
    (Q1_Mood_Week4 == "Anxious")~ "Anxious 😰",
    (Q1_Mood_Week4 == "Stressed/Worried")~ "Stressed/Worried 😓",
    (Q1_Mood_Week4 == "Bored")~ "Bored 🥱",
    (Q1_Mood_Week4 == "Happy")~ "Happy 😃",
    (Q1_Mood_Week4 == "Optimistic")~ "Optimistic 👍",
    (Q1_Mood_Week4 == "Relaxed")~ "Relaxed 💆‍♀‍",
    (Q1_Mood_Week4 == "Confident")~ "Confident 😎",
    (Q1_Mood_Week4 == "Peaceful")~ "Peaceful 😊",
    (Q1_Mood_Week4 == "Pessimistic")~ "Pessimistic 😕",
    (Q1_Mood_Week4 == "Excited")~ "Excited 😄",
    (Q1_Mood_Week4 == "Overwhelmed")~ "Overwhelmed 😫"
  )) %>%
  select(1,61,60,59,58,2,5,13,8,62:65,3,4,6,7,9:12,35,57,14,36,15,37,17,39,18,40,19,41,20,42,21,43,22,44,23,45,24,46,25,47,26,48,27,49,28,50,29,51,30,52,31,53,32,54,33,55,34,56)
colnames(master2)
  

########################### END CLEAN #####################################


##################################

write_csv(master2,"RivalCOVID.csv")


