{
  library(dplyr)
  library(gtsummary)
  library(tidyverse)
}


# WHO standard labels -----------------------------------------------------


# Variable Name                                               , label
#                                                             , 
# Facility ID                                                 , fac_id
# Registry Case ID                                            , reg_id
# Date of Birth                                               , dob
# Age                                                         , age
# IF Unknown --> Age Category                                 , age_cat
# Gender                                                      , gender
# Injury Geographic Location                                  , inj_loc
# Patient Residence                                           , pt_res
# Patient Occupation                                          , pt_oc
# Number of Prior Facilities                                  , fac_number
# Referring Facility Level                                    , fac_level
# Arrival Date                                                , date_arr
# Arrival Time                                                , time_arr
# Emergency Unit/Facility Arrival Mode                        , arr_mode
# Signs of Life                                               , life
# First Vital Sign Assessment Time                            , life_time
# Initial Heart Rate                                          , hr_arred
# Initial SBP                                                 , sbp_arred
# Initial Spontaneous Respiratory Rate                        , rr_arred
# Initial Oxygen Saturation                                   , pox_arred
# Supplemental Oxygen Administration                          , oxygen
# Initial GCS - Total                                         , gcs_total
# Initial GCS - Eye                                           , gcs_eye
# Initial GCS - Verbal                                        , gcs_verbal
# Initial GCS - Motor                                         , gcs_motor
# GCS Qualifier                                               , gcs_qual
# Initial AVPU                                                , avpu
# Number of pupils reactive to light                          , pupils
# Time of First Provider Assessment                           , prov_time
# Number of Major Medical Comorbidities                       , comorb
# Is patient pregnant?                                        , pregnant
# Mass Casualty Event                                         , mass_event
# Injury Event Date                                           , inj_date
# Injury Event Time                                           , inj_time
# Setting                                                     , setting
# Activity                                                    , activity
# Mechanism                                                   , moi
# IF Drowning or Submersion --> Intent of immersion           , int_drown
# IF Poisoning or Exposure --> Mode                           , int_pois
# IF RTI --> Patient Mode of Transport                        , rti_transp
# --> Road User                                               , road_user
# Counterpart to patient's crash                              , crash_counter
# Intent                                                      , int
# IF Intentional Assault --> Assault Relationship             , assault_rel
# Protective Devices                                          , prot_dev
# Alcohol Use                                                 , alcohol
# Drug Use                                                    , drug
# Scene or First Transport Care                               , first_care
# Procedures Performed at the Scene or During First Transport , first_care_proc
# Facility Where First Care Sought - Arrival Date             , first_fac_date
# Facility Where First Care Sought - Arrival Time             , first_fac_time
# Scene to First Care Transport Mode                          , first_care_transp
# Prior Facility Interventions                                , other_fac_int
# Condition                                                   , condition
# Injury Anatomic Location                                    , inj_anatomic
# Number of Provider-Defined Serious Injuries                 , serious_inj
# Emergency Unit Interventions                                , ed_int
# Emergency Unit Departure Date                               , ed_leave_date
# Emergency Unit Departure Time                               , ed_leave_time
# Emergency Unit Disposition                                  , ed_dispo
# IF Morgue or Died --> Official Cause of Death               , ed_death_cause
# Standardised Diagnosis                                      , ed_std_diag
# Inpatient Intervention Date                                 , int_date
# Inpatient Interventions                                     , interventions
# Total Number of Ventilator Days                             , vent_days
# Number of Operating Theatre Encounters                      , or_encounters
# Date patient first arrived to Operating Theatre             , or_date
# Time patient first arrived to Operating Theatre             , or_time
# Complications                                               , complications
# Facility Discharge Date                                     , in_fac_dc_date
# Facility Disposition                                        , in_fac_dispo
# IF Morgue or Died --> Official Cause of Death               , in_death_cause
# Standardised Diagnosis                                      , in_std_diag
# Functional Status at Disposition                            , in_func

# ToC ---------------------------------------------------------------------
toc <- read.csv("/media/newhd/joao-souza/projects/toc_orthopedics/ToCTraumaRegistry_DATA_2021-02-12_1342.csv",
                na.strings = c("", "."))

toc_recoded <- toc %>% 
  mutate(fac_id = "kcmc",
         reg_id = paste(part_id, "toc", sep="_"),
         dob = NA,
         age = age,
         age_cat = NA, 
         gender = recode(female, "0"="Male", "1"="Female"),
         inj_loc = NA,
         pt_res = NA,
         pt_oc = recode(employ, "0"="Student", "1"="Unemployed", "2"="Professional", "3"="Skilled employment",
                        "4"="Self-employed", "5"="Farmer", "6"="Other"),
         fac_number = ifelse(firsthosp != 0, 1, 0),
         # I don't know the levels of each facility just now
         fac_level = NA,
         date_arr = as.Date.character(datearrivekcmc, format="%Y-%m-%d"),
         time_arr = timearrivekcmc,
         arr_mode = recode(moa, "0" = "Ambulance", "1" = "Private car", "2" = "Bijaji", "3" = "Police car", 
                           "4" = "Private motorcycle", "5" = "Boda Boda", "6" = "Taxi", "89" = "Other", "99" = "Unknown"),
         # AVPU?
         life = ifelse(avpu_arred !=3, "Yes", "No"),
         # time arrival at ED or at kcmc
         life_time = ifelse(!is.na(time_arred), time_arred, timearrivekcmc),
         hr_arred = hr_arred,
         sbp_arred = sbp_arred,
         rr_arred = rr_arred,
         pox_arred = pox_arred,
         oxygen = NA,
         # Only GOS
         gcs_total = rowSums(select(., c(gcs_eye, gcs_verbal, gcs_motor)), na.rm=TRUE),
         gcs_eye = gcs_eye,
         gcs_verbal = gcs_verbal,
         gcs_motor = gcs_motor,
         gcs_qual = NA,
         avpu = avpu_arred,
         pupils = case_when(pupils_l %in% c(0, 1) & pupils_r %in% c(0, 1) ~ 2,
                            pupils_l %in% c(0, 1) | pupils_r %in% c(0, 1) ~ 1,
                            TRUE ~ 0),
         prov_time = ifelse(!is.na(time_arred), time_arred, timearrivekcmc),
         comorb = case_when(dm == 1 & htn == 1  ~ 2,
                            dm == 1 | htn == 1  ~ 1,
                            TRUE ~ 0),
         pregnant = NA,
         mass_event = NA, 
         inj_date = as.Date.character(dateinjury, format="%Y-%m-%d"),
         inj_time = timeinjury,
         setting = NA,
         activity = NA,
         moi = recode(moi, "0" = "Road Traffic Injury", "1" = "Assault", "2" = "Drowning", "3" = "Fall", 
                      "4" = "Burn", "89" = "Other", "99" = "Unknown"),
         int_drown = case_when(moi == "Drowning" & intent == 1 ~ "Intentional",
                               moi == "Drowning" & intent == 0 ~ "Not intentional",
                               TRUE ~ NA_character_),
         int_pois = NA,
         rti_transp = recode(moi_vehic, "0" = "Motorcycle", "1" = "Car", "2" = "Bijaji", "3" = "Truck", "4" = "Dala Dala", 
                             "5" = "Bus"),
         road_user = recode(moi_rti, "0" = "Driver", "1" = "Passenger", "2" = "Pedestrian", "3" = "Bicycle"),
         crash_counter = NA,
         int = recode(intent, "0" = "Not intentional", "1" = "Intentional", "99" = "Unknown"),
         assault_rel = NA,
         prot_dev = NA,
         alcohol = recode(etoh, "0" = "Negative", "1" = "Positive"),
         drug = NA, 
         first_care = NA,
         first_care_proc = NA,
         first_fac_date = as.Date.character(o_hospdate, format="%Y-%m-%d"),
         first_fac_time = o_hosptime,
         first_care_transp = NA,
         other_fac_int = NA,
         condition = NA,
         # We don't  collect that information
         inj_anatomic = NA,
         serious_inj = NA,
         ed_int = NA,
         ed_leave_date = as.Date.character(date_leaved, format="%Y-%m-%d"),
         ed_leave_time = time_leaved,
         ed_dispo = recode(dispo_loc, "0" = "OR", "1" = "ICU", "2" = "Surgical",
                           "3" = "Surgical", "89" = "Other", "4" = "Other"),
         ed_death_cause = NA,
         ed_std_diag = NA,
         int_date = NA,
         # solve issue with interventions later
         interventions = NA,
         # interventions = list(unite(toc, surg_1proc, surg_2proc, surg_3proc, remove=FALSE, na.rm=TRUE, sep=" | ")),
         vent_days = NA,
         or_encounters = case_when(!is.na(surg_1date) & !is.na(surg_2date) & !is.na(surg_3date) ~ "3",
                                   !is.na(surg_1date) & !is.na(surg_2date) ~ "2",
                                   !is.na(surg_1date) ~ "1",
                                   TRUE ~ "0"),
         or_date = as.Date.character(surg_1date, format="%Y-%m-%d"),
         or_time = surg_1time,
         complications = NA,
         in_fac_dc_date = as.Date.character(dc_date, format="%Y-%m-%d"),
         in_fac_dispo = recode(discharge_dest, "0" = "Morgue",  "1" = "Home", "89" = "Other"),
         in_death_cause = NA,
         in_std_diag = NA,
         in_func = rowSums(select(., fimcare:fimmemory), na.rm=TRUE)) %>% 
  select(fac_id, reg_id, dob, age, age_cat, gender, inj_loc, pt_res, pt_oc, fac_number, 
         fac_level, date_arr, time_arr, arr_mode, life, life_time, hr_arred, sbp_arred, 
         rr_arred, pox_arred, oxygen, gcs_total, gcs_eye, gcs_verbal, gcs_motor, gcs_qual, 
         avpu, pupils, prov_time, comorb, pregnant, mass_event, inj_date, inj_time, setting, 
         activity, moi, int_drown, int_pois, rti_transp, road_user, crash_counter, int, 
         assault_rel, prot_dev, alcohol, drug, first_care, first_care_proc, first_fac_date, 
         first_fac_time, first_care_transp, other_fac_int, condition, inj_anatomic, serious_inj, 
         ed_int, ed_leave_date, ed_leave_time, ed_dispo, ed_death_cause, ed_std_diag, int_date, 
         interventions, vent_days, or_encounters, or_date, or_time, complications, in_fac_dc_date, 
         in_fac_dispo, in_death_cause, in_std_diag, in_func)




# TBI ---------------------------------------------------------------------
tbi <- read.csv("/media/newhd/joao-souza/projects/PRESTO/tz_TBIregistry_data.csv",
                na.strings = c("", "."))

tbi_recoded <- tbi %>% 
  # Creating some variables first so I am able to select the variables of interest easily in order later
  mutate(hosp_1 = case_when(grepl("hos|Hos|center|Center", transport_start_name_1) & 
                              !grepl("same|Sam|SAME|kcm|Kcm|KCM", transport_start_name_1) ~ 1,
                            TRUE ~ 0),
         # variables to count how many hospital the patient has been previous to kcmc
         hosp_2 = case_when(grepl("hos|Hos|center|Center", transport_start_name_2) & 
                              !grepl("same|Sam|SAME|kcm|Kcm|KCM", transport_start_name_2) ~ 1,
                            TRUE ~ 0),
         hosp_3 = case_when(grepl("hos|Hos|center|Center", transport_start_name_3) & 
                              !grepl("same|Sam|SAME|kcm|Kcm|KCM", transport_start_name_3) ~ 1,
                            TRUE ~ 0),
         hosp_4 = case_when(grepl("hos|Hos|center|Center", transport_start_name_4) & 
                              !grepl("same|Sam|SAME|kcm|Kcm|KCM", transport_start_name_4) ~ 1,
                            TRUE ~ 0),
         date_tbisurg = as.Date.character(date_tbisurg, format="%m/%d/%y"),
         date_othersurg = as.Date.character(date_othersurg, format="%m/%d/%y")) %>% 
  mutate(fac_id = "kcmc",
         reg_id = paste(study_id, "tbi", sep="_"),
         dob = NA,
         age = age,
         age_cat = NA,
         gender = recode(male, "1"="Male", "0"="Female"),
         # first district
         inj_loc = recode(transport_start_1, "1" = "Moshi Urban", "2" = "Moshi Rural", "3" = "Hai", "4" = "Rombo", 
                          "5" = "Mwanga", "6" = "Same", "7" = "Siha", "8" = "Other"),
         pt_res = NA,
         pt_oc = NA,
         fac_number = rowSums(select(., hosp_1:hosp_4), na.rm=TRUE),
         # I don't know the levels of each facility just now
         fac_level = NA,
         date_arr = as.Date.character(date_arrival, format="%m/%d/%y"),
         time_arr = date_arrival,
         # Used only the first variable
         arr_mode = recode(transport_mode_1, "0" = "None", "1" = "Private car", "2" = "Taxi", "3" = "Bicycle", "4" = "Boda Boda", 
                           "5" = "Dala Dala", "6" = "Police car", "7" = "Ambulance", "8" = "Bus", "9" = "Other", 
                           "91" = "Piki-piki", "99" = "Unknown"),
         # AVPU?
         life = ifelse(avpu !=3, "Yes", "No"),
         # time arrival at ED or at kcmc
         life_time = time_arrival,
         hr_arred = heart_rate,
         sbp_arred = sys_bp,
         rr_arred = resp_rate,
         pox_arred = pulse_ox,
         oxygen = ifelse(airway_mgmt!=0, "Yes", "No"),
         gcs_total = rowSums(select(., c(gcs_eye, gcs_verbal, gcs_motor)), na.rm=TRUE),
         gcs_eye = gcs_eye,
         gcs_verbal = gcs_verbal,
         gcs_motor = gcs_motor,
         gcs_qual = NA,
         avpu = avpu,
         pupils = case_when(pupil_left_r == 1 & pupil_right_r == 1 ~ 2,
                            pupil_left_r == 1 | pupil_right_r == 1 ~ 1,
                            TRUE ~ 0),
         prov_time = time_arrival,
         comorb = NA,
         pregnant = NA,
         mass_event = NA,
         inj_date = as.Date.character(inj_date, format="%m/%d/%y"),
         inj_time = inj_time,
         setting = NA,
         activity = NA,
         moi = recode(moi, "0" = "Road Traffic Injury", "1" = "Assault", "2" = "Drowning", "3" = "Fall",
                      "4" = "Other"),
         int_drown = NA,
         int_pois = NA,
         rti_transp = recode(rti, "1" = "Motorcycle", "2" = "Car"),
         road_user = recode(rti, "1" = "Driver", "2" = "Driver", "0" = "Pedestrian"),
         # text field? Try it out?
         crash_counter = NA,
         int = recode(intent, "0" = "Not intentional", "1" = "Intentional",  "2" = "Intentional", "3" = "Unknown"),
         assault_rel = NA,
         prot_dev = NA,
         alcohol = recode(alcohol, "0" = "No", "1" = "Yes", "2" = "Unknown"),
         drug = NA,
         first_care = NA,
         first_care_proc = NA,
         first_fac_date = NA,
         first_fac_time = NA,
         first_care_transp = recode(transport_mode_1, "0" = "None", "1" = "Private car", "2" = "Taxi", "3" = "Bicycle", "4" = "Boda Boda", 
                                    "5" = "Dala Dala", "6" = "Police car", "7" = "Ambulance", "8" = "Bus", "9" = "Other", 
                                    "91" = "Piki-piki", "99" = "Unknown"),
         other_fac_int = NA,
         condition = NA,
         # We don't  collect that information
         inj_anatomic = NA,
         serious_inj = NA,
         ed_int = NA,
         ed_leave_date = NA,
         ed_leave_time = NA,
         ed_dispo = recode(cd_dispo, "2" = "OR", "0" = "ICU", "1" = "Surgical", "4" = "Home", "3" = "Death, Morgue"),
         ed_death_cause = NA,
         # All patients have tbi?
         ed_std_diag = NA,
         int_date = NA,
         # solve issue with interventions later
         interventions = NA,
         # interventions = list(unite(toc, surg_1proc, surg_2proc, surg_3proc, remove=FALSE, na.rm=TRUE, sep=" | ")),
         vent_days = NA,
         or_encounters = case_when(tbi_surgery == 1 & other_surgery == 1 ~ "2",
                                   tbi_surgery == 1 | other_surgery == 1 ~ "1",
                                   TRUE ~ "0"),
         or_date = case_when(is.na(date_tbisurg) & !is.na(date_othersurg) ~ date_othersurg,
                             !is.na(date_tbisurg) & is.na(date_othersurg) ~ date_tbisurg,
                             (!is.na(date_tbisurg) & !is.na(date_othersurg)) & (date_tbisurg >= date_othersurg) ~ date_othersurg,
                             (!is.na(date_tbisurg) & !is.na(date_othersurg)) & (date_othersurg >= date_tbisurg) ~ date_tbisurg,
                             TRUE ~ as.Date("2020-01-01", format="%Y-%m-%d")),
           # ifelse(date_tbisurg >= date_othersurg, date_othersurg, date_tbisurg),
         or_time = ifelse(date_tbisurg >= date_othersurg, time_othersurg, time_tbisurg),
         complications = NA,
         in_fac_dc_date = as.Date.character(date_dc_home, format="%m/%d/%y"),
         in_fac_dispo = NA,
         in_death_cause = NA,
         in_std_diag = NA,
         in_func = NA
         ) %>%
  select(fac_id, reg_id, dob, age, age_cat, gender, inj_loc, pt_res, pt_oc, fac_number, 
         fac_level, date_arr, time_arr, arr_mode, life, life_time, hr_arred, sbp_arred, 
         rr_arred, pox_arred, oxygen, gcs_total, gcs_eye, gcs_verbal, gcs_motor, gcs_qual, 
         avpu, pupils, prov_time, comorb, pregnant, mass_event, inj_date, inj_time, setting, 
         activity, moi, int_drown, int_pois, rti_transp, road_user, crash_counter, int, 
         assault_rel, prot_dev, alcohol, drug, first_care, first_care_proc, first_fac_date, 
         first_fac_time, first_care_transp, other_fac_int, condition, inj_anatomic, serious_inj, 
         ed_int, ed_leave_date, ed_leave_time, ed_dispo, ed_death_cause, ed_std_diag, int_date, 
         interventions, vent_days, or_encounters, or_date, or_time, complications, in_fac_dc_date, 
         in_fac_dispo, in_death_cause, in_std_diag, in_func)


# Registry ----------------------------------------------------------------
# code to load registry rdata from memory, save it as .csv and read it 
# load(paste0("/media/newhd/joao-souza/projects/GEMINI/PRACT/Backup/registry_data",
#             format(Sys.Date(), format="%d%b%Y"),".Rdata"))
# write.csv(reg_data, "trauma_registry_pract.csv")
reg_data <- read.csv("trauma_registry_pract.csv")

reg_data_recoded <- reg_data %>% 
  mutate(fac_id = "kcmc",
         reg_id = paste(part_id, "registry", sep="_"),
         dob = NA,
         age = age,
         age_cat = NA,
         gender = recode(female, "0"="Male", "1"="Female"),
         inj_loc = NA,
         pt_res = NA,
         pt_oc = recode(employ,"0" = "Student", "1" = "Unemployed", "2" = "Professional", "3" = "Skilled employment", 
                        "4" = "Self-employed", "5" = "Farmer", "89" = "other"),
         fac_number = ifelse(firsthospkcmc != 1, 1, 0),
         fac_level = NA,
         date_arr = as.Date.character(datearrivekcmc, format="%Y-%m-%d"),
         time_arr = timearrivekcmc,
         arr_mode = recode(moa, "0" = "Ambulance", "1" = "Private car", "2" = "Bijaji", "3" = "Police car", 
                           "4" = "Private motorcycle", "5" = "Boda Boda", "6" = "Taxi", "7" = "Walking", 
                           "99" = "Unknown", "89" = "Other"),
         # using gcs?
         life = ifelse(gcs_eye != 1 | gcs_verbal != 1 | gcs_motor != 1, "Yes", "No"),
         # time arrival at ED or at kcmc
         life_time = time_arrvs,
         hr_arred = hr_arred,
         sbp_arred = sbp_arred,
         rr_arred = rr_arred,
         pox_arred = pox_arred,
         oxygen = recode(ox_ed, "1" = "Yes", "0" = "No"),
         gcs_total = rowSums(select(., c(gcs_eye, gcs_verbal, gcs_motor)), na.rm=TRUE),
         gcs_eye = gcs_eye,
         gcs_verbal = gcs_verbal,
         gcs_motor = gcs_motor,
         gcs_qual = NA,
         avpu = NA,
         pupils = case_when(pupils_l %in% c(0, 1) & pupils_r %in% c(0, 1) ~ 2,
                            pupils_l %in% c(0, 1) | pupils_r %in% c(0, 1) ~ 1,
                            TRUE ~ 0),
         prov_time = ifelse(!is.na(o_hosptime), o_hosptime, timearrivekcmc),
         comorb = case_when(dm == 1 & htn == 1  ~ 2,
                            dm == 1 | htn == 1  ~ 1,
                            TRUE ~ 0),
         pregnant = NA,
         mass_event = NA,
         inj_date = as.Date.character(dateinjury, format="%Y-%m-%d"),
         inj_time = timeinjury,
         setting = NA,
         activity = NA,
         moi = recode(moi, "99" = "Unknown", "0" = "Road Traffic Injury", "1" = "Assault", 
                      "2" = "Penetrating Trauma", "3" = "Fall", "4" = "Burn", "5" = "Drowning", 
                      "6" = "Suffocation/Choking/Hanging", "7" = "Poisoning/Toxic Exposure", 
                      "8" = "Animal Envenomation", "89" = "Other"),
         int_drown = case_when(moi == "Drowning" & intent == 1 ~ "Intentional",
                               moi == "Drowning" & intent == 0 ~ "Not intentional",
                               TRUE ~ NA_character_),
         int_pois = case_when(moi == "Poisoning/Toxic Exposure" & intent == 1 ~ "Intentional",
                              moi == "Poisoning/Toxic Exposure" & intent == 0 ~ "Not intentional",
                              TRUE ~ NA_character_),
         rti_transp = recode(moi_vehic, "0" = "Motorcycle", "1" = "Car", "2" = "Bijaji", "3" = "Truck", "4" = "Dala Dala",
                             "5" = "Bus"),
         road_user = recode(moi_rti, "0" = "Driver", "1" = "Passenger", "2" = "Pedestrian", "3" = "Bicycle"),
         crash_counter = NA,
         int = recode(intent, "0" = "Not intentional", "1" = "Intentional", "99" = "Unknown"),
         assault_rel = NA,
         # list
         prot_dev = case_when(moi_motohelm == 1 | moi_bikehelm == 1 ~ "Helmet",
                              moi_sb == 1 & moi_airbag == 1 ~ "Seat belt, Airbag",
                              moi_sb == 1 ~ "Seat belt",
                              moi_airbag == 1 ~ "Airbag",
                              TRUE ~ NA_character_),
         alcohol = recode(etoh, "0" = "Negative", "1" = "Positive"),
         drug = ifelse(subs_other == 1, "Not-specified", NA_character_),
         first_care = NA,
         first_care_proc = NA,
         first_fac_date = as.Date.character(o_hospdate, format="%Y-%m-%d"),
         first_fac_time = o_hosptime,
         first_care_transp = NA,
         other_fac_int = NA,
         condition = NA,
         # combined injury_body__ variables
         # inj_anatomic = NA,
         # Not really possible to estimate number of serious injuries...
         serious_inj = ifelse(inj_req_or == 1 | inj_life == 1, 1, 0),
         ed_int = NA,
            # Exams, diagnostics, procedures, medication, treatment
            # intub_ed == 1, "Intubation"
            # np_air == 1, "Nasopharyngeal airway"
            # ox_ed = 1, "Oxygen applied"
            # fluids == 1, "Fluids started"
             # blood == 1, "Blood started"
         ed_leave_date = as.Date.character(date_leaved, format="%Y-%m-%d"),
         ed_leave_time = time_leaved,
         ed_dispo = recode(dispo_loc, "0" = "OR", "1" = "ICU", "2" = "Surgical", "3" = "Surgical", 
                           "4" = "ICU", "5" = "Other", "6" = "Urology", "7" = "Burn Unit", 
                           "89" = "Other" ),
         ed_death_cause = NA,
         ed_std_diag = NA,
         int_date = NA,
         #        # solve issue with interventions later
         interventions = NA,
         #        # interventions = list(unite(toc, surg_1proc, surg_2proc, surg_3proc, remove=FALSE, na.rm=TRUE, sep=" | ")),
         vent_days = NA,
         or_encounters = case_when(surg_gt3 == 1 ~ "3 or more",
                                   !is.na(surg_1date) & !is.na(surg_2date) ~ "2",
                                   !is.na(surg_1date) ~ "1",
                                   TRUE ~ "0"),
         or_date = as.Date.character(surg_1date, format="%Y-%m-%d"),
         or_time = surg_1time,
         complications = NA,
         in_fac_dc_date = as.Date.character(dc_date, format="%Y-%m-%d"),
         in_fac_dispo = recode(discharge_dest, "0" = "Morgue",  "1" = "Home", "89" = "Other"),
         in_death_cause = death_cause,
         in_std_diag = NA,
         in_func = rowSums(select(., fimcare:fimmemory), na.rm=TRUE),
         injury_body___f01 = ifelse(injury_body___f01 == 1, "Cranium (front-right)", NA_character_),
         injury_body___f02 = ifelse(injury_body___f02 == 1, "Cranium (front-left)", NA_character_),
         injury_body___f03 = ifelse(injury_body___f03 == 1, "Face (front-right)", NA_character_),
         injury_body___f04 = ifelse(injury_body___f04 == 1, "Face (front-left)", NA_character_),
         injury_body___f05 = ifelse(injury_body___f05 == 1, "Neck (front-right)", NA_character_),
         injury_body___f06 = ifelse(injury_body___f06 == 1, "Neck (front-left)", NA_character_),
         injury_body___f07 = ifelse(injury_body___f07 == 1, "Shoulder (front-right)", NA_character_),
         injury_body___f08 = ifelse(injury_body___f08 == 1, "Chest (front-right)", NA_character_),
         injury_body___f09 = ifelse(injury_body___f09 == 1, "Chest (front-left)", NA_character_),
         injury_body___f10 = ifelse(injury_body___f10 == 1, "Shoulder (front-left)", NA_character_),
         injury_body___f11 = ifelse(injury_body___f11 == 1, "Bicep (front-right)", NA_character_),
         injury_body___f12 = ifelse(injury_body___f12 == 1, "Bicep (front-left)", NA_character_),
         injury_body___f13 = ifelse(injury_body___f13 == 1, "Elbow (front-right)", NA_character_),
         injury_body___f14 = ifelse(injury_body___f14 == 1, "Elbow (front-left)", NA_character_),
         injury_body___f15 = ifelse(injury_body___f15 == 1, "Forearm (front-right)", NA_character_),
         injury_body___f16 = ifelse(injury_body___f16 == 1, "Abdomen (front-right)", NA_character_),
         injury_body___f17 = ifelse(injury_body___f17 == 1, "Abdomen (front-left)", NA_character_),
         injury_body___f18 = ifelse(injury_body___f18 == 1, "Forearm (front-left)", NA_character_),
         injury_body___f19 = ifelse(injury_body___f19 == 1, "Wrist (front-right)", NA_character_),
         injury_body___f20 = ifelse(injury_body___f20 == 1, "Hip (front-right)", NA_character_),
         injury_body___f21 = ifelse(injury_body___f21 == 1, "Pubic (front-right)", NA_character_),
         injury_body___f22 = ifelse(injury_body___f22 == 1, "Pubic (front-left)", NA_character_),
         injury_body___f23 = ifelse(injury_body___f23 == 1, "Hip (front-left)", NA_character_),
         injury_body___f24 = ifelse(injury_body___f24 == 1, "Wrist (front-left)", NA_character_),
         injury_body___f25 = ifelse(injury_body___f25 == 1, "Hand (palm-right)", NA_character_),
         injury_body___f26 = ifelse(injury_body___f26 == 1, "Upper Leg (front-right)", NA_character_),
         injury_body___f27 = ifelse(injury_body___f27 == 1, "Upper Leg (front-left)", NA_character_),
         injury_body___f28 = ifelse(injury_body___f28 == 1, "Hand (palm-left)", NA_character_),
         injury_body___f29 = ifelse(injury_body___f29 == 1, "Knee (front-right)", NA_character_),
         injury_body___f30 = ifelse(injury_body___f30 == 1, "Knee (front-left)", NA_character_),
         injury_body___f31 = ifelse(injury_body___f31 == 1, "Lower Leg (front-right)", NA_character_),
         injury_body___f32 = ifelse(injury_body___f32 == 1, "Lower Leg (front-left)", NA_character_),
         injury_body___f33 = ifelse(injury_body___f33 == 1, "Ankle (front-right)", NA_character_),
         injury_body___f34 = ifelse(injury_body___f34 == 1, "Ankle (front-left)", NA_character_),
         injury_body___f35 = ifelse(injury_body___f35 == 1, "Foot (top-right)", NA_character_),
         injury_body___f36 = ifelse(injury_body___f36 == 1, "Foot (top-left)", NA_character_),
         injury_body___b01 = ifelse(injury_body___b01 == 1, "Cranium (back-left)", NA_character_),
         injury_body___b02 = ifelse(injury_body___b02 == 1, "Cranium (back-right)", NA_character_),
         injury_body___b03 = ifelse(injury_body___b03 == 1, "Head (back-left)", NA_character_),
         injury_body___b04 = ifelse(injury_body___b04 == 1, "Head (back-right)", NA_character_),
         injury_body___b05 = ifelse(injury_body___b05 == 1, "Neck (back-left)", NA_character_),
         injury_body___b06 = ifelse(injury_body___b06 == 1, "Neck (back-right)", NA_character_),
         injury_body___b07 = ifelse(injury_body___b07 == 1, "Shoulder (back-left)", NA_character_),
         injury_body___b08 = ifelse(injury_body___b08 == 1, "Upper Back (back-left)", NA_character_),
         injury_body___b09 = ifelse(injury_body___b09 == 1, "Upper Back (back-right)", NA_character_),
         injury_body___b10 = ifelse(injury_body___b10 == 1, "Shoulder (back-right)", NA_character_),
         injury_body___b11 = ifelse(injury_body___b11 == 1, "Bicep (back-left)", NA_character_),
         injury_body___b12 = ifelse(injury_body___b12 == 1, "Middle Back (back-left)", NA_character_),
         injury_body___b13 = ifelse(injury_body___b13 == 1, "Middle Back (back-right)", NA_character_),
         injury_body___b14 = ifelse(injury_body___b14 == 1, "Bicep (back-right)", NA_character_),
         injury_body___b15 = ifelse(injury_body___b15 == 1, "Elbow (back-left)", NA_character_),
         injury_body___b16 = ifelse(injury_body___b16 == 1, "Elbow (back-right)", NA_character_),
         injury_body___b17 = ifelse(injury_body___b17 == 1, "Forearm (back-left)", NA_character_),
         injury_body___b18 = ifelse(injury_body___b18 == 1, "Lower Back (back-left)", NA_character_),
         injury_body___b19 = ifelse(injury_body___b19 == 1, "Lower Back (back-right)", NA_character_),
         injury_body___b20 = ifelse(injury_body___b20 == 1, "Forearm (back-right)", NA_character_),
         injury_body___b21 = ifelse(injury_body___b21 == 1, "Wrist (back-left)", NA_character_),
         injury_body___b22 = ifelse(injury_body___b22 == 1, "Hip (back-left)", NA_character_),
         injury_body___b23 = ifelse(injury_body___b23 == 1, "Buttocks (back-left)", NA_character_),
         injury_body___b24 = ifelse(injury_body___b24 == 1, "Buttocks (back-right)", NA_character_),
         injury_body___b25 = ifelse(injury_body___b25 == 1, "Hip (back-right)", NA_character_),
         injury_body___b26 = ifelse(injury_body___b26 == 1, "Wrist (back-right)", NA_character_),
         injury_body___b27 = ifelse(injury_body___b27 == 1, "Hand (top-left)", NA_character_),
         injury_body___b28 = ifelse(injury_body___b28 == 1, "Upper Leg (back-left)", NA_character_),
         injury_body___b29 = ifelse(injury_body___b29 == 1, "Upper Leg (back-right)", NA_character_),
         injury_body___b30 = ifelse(injury_body___b30 == 1, "Hand (top-right)", NA_character_),
         injury_body___b31 = ifelse(injury_body___b31 == 1, "Knee (back-left)", NA_character_),
         injury_body___b32 = ifelse(injury_body___b32 == 1, "Knee (back-right)", NA_character_),
         injury_body___b33 = ifelse(injury_body___b33 == 1, "Lower Leg (back-left)", NA_character_),
         injury_body___b34 = ifelse(injury_body___b34 == 1, "Lower Leg (back-right)", NA_character_),
         injury_body___b35 = ifelse(injury_body___b35 == 1, "Ankle (back-left)", NA_character_),
         injury_body___b36 = ifelse(injury_body___b36 == 1, "Ankle (back-right)", NA_character_),
         injury_body___b37 = ifelse(injury_body___b37 == 1, "Foot (bottom-left)", NA_character_),
         injury_body___b38 = ifelse(injury_body___b38 == 1, "Foot (bottom-right)", NA_character_)
  ) %>% 
  unite(inj_anatomic, injury_body___f01:injury_body___b38, na.rm=TRUE, sep=" | ") %>% 
  select(fac_id, reg_id, dob, age, age_cat, gender, inj_loc, pt_res, pt_oc, fac_number, 
         fac_level, date_arr, time_arr, arr_mode, life, life_time, hr_arred, sbp_arred, 
         rr_arred, pox_arred, oxygen, gcs_total, gcs_eye, gcs_verbal, gcs_motor, gcs_qual, 
         avpu, pupils, prov_time, comorb, pregnant, mass_event, inj_date, inj_time, setting, 
         activity, moi, int_drown, int_pois, rti_transp, road_user, crash_counter, int, 
         assault_rel, prot_dev, alcohol, drug, first_care, first_care_proc, first_fac_date, 
         first_fac_time, first_care_transp, other_fac_int, condition, inj_anatomic, serious_inj, 
         ed_int, ed_leave_date, ed_leave_time, ed_dispo, ed_death_cause, ed_std_diag, int_date, 
         interventions, vent_days, or_encounters, or_date, or_time, complications, in_fac_dc_date, 
         in_fac_dispo, in_death_cause, in_std_diag, in_func)



# Combining data sets ------------------------------------------------------

data <- bind_rows(toc_recoded, tbi_recoded, reg_data_recoded)

data <- data %>% 
  mutate_if(is.character, as.factor)

sapply(data, function(x){sum(is.na(x))})
sapply(toc_recoded, function(x){sum(is.na(x))})
sapply(tbi_recoded, function(x){sum(is.na(x))})
sapply(reg_data_recoded, function(x){sum(is.na(x))})

dim(tbi_recoded)
dim(toc_recoded)
dim(reg_data_recoded)