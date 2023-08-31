#########################################
# MSc Main project examining trajectories of change for PTSD patients
# during trauma focused CBT 
#
# Author: 220225638
# Date: 2023-08-13
# Description: Data management file for raw data import, cleaning and export
#########################################

#########################################
# Setup and import raw data for manipulation
#########################################

source(here::here("script", "config.R"))

#Reading in the data 
raw_data = read_sav(raw_data_path)
processed_data = raw_data # creating a reference of the raw data 
# Adding some variables for time:
processed_data$TIME <- processed_data$`Session_number_@Step` #Creating a column called TIME with session @ step
processed_data$TIME_log <- log(processed_data$TIME) #Creating a log of time column 
processed_data$TIME <- NULL #Deleting the session @ step TIME
processed_data$TIME <- processed_data$`Session_number_@Step`-1 #creating TIME as session -1 to root time at 0

write_sav(processed_data, processed_data_path)

# Moving the columns over to inspect them.

wide_dataset <- processed_data[, -grep("RCSI", names(processed_data))] #deleting the old RCSI variables 

wide_column_names <- names(wide_dataset)
wide_phq_position <- which(wide_column_names== "PHQ9")
wide_phq_chg_position <- which(wide_column_names== "PHQ9_Chg")
if(wide_phq_chg_position < wide_phq_position){
  wide_phq_position <- wide_phq_position - 1
}
wide_before_phq <- wide_column_names[1:wide_phq_position]
wide_after_phq <- wide_column_names[(wide_phq_position + 1): length(wide_column_names)]

wide_dataset <- wide_dataset[, c(seq_len(wide_phq_position), wide_phq_chg_position, 
                                 (wide_phq_position + 1): (wide_phq_chg_position - 1), (wide_phq_chg_position + 1): ncol(wide_dataset))]

# Same again for GAD7.

wide_column_names <- names(wide_dataset)
wide_phq_position <- which(wide_column_names== "GAD7")
wide_phq_chg_position <- which(wide_column_names== "GAD7_Chg")
if(wide_phq_chg_position < wide_phq_position){
  wide_phq_position <- wide_phq_position - 1
}
wide_before_phq <- wide_column_names[1:wide_phq_position]
wide_after_phq <- wide_column_names[(wide_phq_position + 1): length(wide_column_names)]

wide_dataset <- wide_dataset[, c(seq_len(wide_phq_position), wide_phq_chg_position, 
                                 (wide_phq_position + 1): (wide_phq_chg_position - 1), (wide_phq_chg_position + 1): ncol(wide_dataset))]

# Creating GAD7_RI_Session from GAD7_Chg
GAD7_RI_session_data <- case_when(wide_dataset$GAD7_Chg <= -4 ~ 2, wide_dataset$GAD7_Chg >= -3 & wide_dataset$GAD7_Chg <= 3 ~ 0,
                                          wide_dataset$GAD7_Chg >= 4 ~ 1)

wide_gad_position <- which(names(wide_dataset)=="GAD7_Chg")
wide_dataset <- cbind(wide_dataset[, 1:wide_gad_position], GAD7_RI_session= GAD7_RI_session_data, wide_dataset[, (wide_gad_position + 1):
                                                                                                                ncol(wide_dataset)])

wide_dataset <- wide_dataset %>% arrange(CaseID, `Session_number_@Step`) # Arranging to ascending order 

wide_dataset <- wide_dataset %>% arrange(CaseID) %>% group_by(CaseID) %>% mutate(`GAD7_@Last_session` = last(GAD7)) # Making GAD7 at last session
#THIS NEEDS TO BE MOVED ^.

wide_dataset <- wide_dataset %>% mutate(`GAD7_Chg@Last_session` = `GAD7_@S1` - `GAD7_@Last_session`) #making gad7chnage at last session.

# Creating RCSI:
wide_dataset <- wide_dataset %>% mutate(`GAD7_RCSI_@Last_session` = ifelse(`GAD7_@S1` >= 8, 0, NA)) #na for the people who didnt reach clinical levels at first session.

wide_dataset <- wide_dataset %>% mutate(`GAD7_RCSI_@Last_session` = ifelse(`GAD7_@S1`>= 8 & `GAD7_Chg@Last_session` >= 4 &
                                                                             `GAD7_@Last_session` < 8,
                                                                           1, `GAD7_RCSI_@Last_session`))

#(Doing something with aggregate)
# Group by CaseID and get the last Session_number for each group
wide_dataset <- wide_dataset %>%
  arrange(CaseID) %>%  # Ensure data is sorted by CaseID
  group_by(CaseID) %>%
  mutate(`Total_sessions_@Step` = last(`Session_number_@Step`))

wide_session4_dataset <- wide_dataset %>% filter(`Total_sessions_@Step` >4 & `Session_number_@Step` == 4)

#Notes I made in SPSS:
#instead of GAD7 say GAD7@S4. 
#Renamed PHQ9_Chg to PHQ9_Chg_@S4
#and same for GAD7.
#Also chnaged the name of GAD7_RI_@Session to GAD7_RI_@S4.

wide_session4_dataset <- wide_session4_dataset %>% rename("GAD7_@S4" = "GAD7", "PHQ9_Chg_@S4" = "PHQ9_Chg", "GAD7_Chg_@S4" = "GAD7_Chg",
                                                          "GAD7_RI_@S4" = "GAD7_RI_session", "PHQ9_@S4" = "PHQ9")





