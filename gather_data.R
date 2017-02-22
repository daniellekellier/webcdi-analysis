
# Collect data from wordbank.


num_words <- get_item_data() %>%
  filter(type == "word") %>%
  group_by(language, form) %>%
  summarise(n = n())

vocab_data <- get_administration_data() %>%
  select(data_id, language, form, age, sex, 
         mom_ed, birth_order, production, comprehension, source_name, longitudinal) %>% 
  left_join(num_words) %>%
  mutate(no_production = n - production)

if (!allow_longitudinal){
  vocab_data <- vocab_data %>% filter(!longitudinal)
}

min_age <- min(vocab_data$age)
max_age <- max(vocab_data$age)


source("modeling_functions.R")



# Grab English WG comprehension data from Wordbank and plot alone with model

comp_data <- vocab_data %>%
  filter(form == "WG" &  language == "English") %>%
  mutate(mean = comprehension / n)

comp_models <- comp_data %>%
  mutate(mean = jitter(mean, amount=0, factor = 0.3)) %>%
  split(.$language) %>%
  map(fit_gcrq) 

comp_preds <- comp_data %>%
  group_by(language, age) %>%
  summarise(n = n()) %>%
  data.frame() %>%
  split(.$language) %>%
  map_df(function(x) pred_gcrq(x, comp_models)) 


# Grab production data from English WG and WS for Wordbank and plot data alone with model


prod_data <- vocab_data %>%
  filter(language == "English") %>%
  mutate(mean = production / n)

prod_models <- prod_data %>%
  mutate(mean = jitter(mean, amount=0, factor = 0.3)) %>%
  split(.$language) %>%
  map(fit_gcrq) 

prod_preds <- prod_data %>%
  group_by(language, age) %>%
  summarise(n = n()) %>%
  data.frame() %>%
  split(.$language) %>%
  map_df(function(x) pred_gcrq(x, prod_models)) 


# Access Web-CDI database and find data from the studies of interest (labeled 'MTurk').
# Grab the related CDI items. There are one set each for the two instruments.



webcdi <- connect_to_webcdi(mode=mode)
study_info <- get_common_table(webcdi, "researcher_UI_study") %>% as.data.frame() %>% filter(grepl('MTurk', name)) %>% rename(study_id = id, instrument = instrument_id)
instruments <- unique(study_info$instrument)
study_items <- list()

for (i in 1:length(instruments)){
  temp <- get_common_table(webcdi, paste("cdi_forms", tolower(instruments[i]), sep="_")) %>% as.data.frame() %>% rename(item_ID = itemID)
  study_items[[i]] <- temp
  study_items[[i]]$instrument <- instruments[i]
}

cdi_items <- bind_rows(study_items) %>% filter(!grepl('example', item_ID))



# Grab Web-CDI by-child data (administration details, demographic information, and given answers)


admin_info <- get_common_table(webcdi, "researcher_UI_administration") %>% as.data.frame() %>% filter(study_id %in% study_info$study_id ) %>% rename(administration_id = id)

background_info <- get_common_table(webcdi, "cdi_forms_backgroundinfo") %>% as.data.frame() %>% filter(administration_id %in% admin_info$administration_id)  %>% mutate(english = (84-(language_days_per_week * language_hours_per_day))/84)

background_info$english[is.na(background_info$english)] <- 1

cdi_answers <- get_common_table(webcdi, "researcher_UI_administration_data") %>% as.data.frame() %>% filter(administration_id %in% admin_info$administration_id) %>% select(-id)



# Merge Web-CDI tables.
# Calculate proportions of words understood and words produced.


combined_data <- left_join(background_info, admin_info, by = "administration_id") %>% left_join(., study_info, by = "study_id") %>% left_join(., cdi_items, by = c("instrument")) %>% left_join(., cdi_answers, by = c("administration_id", "item_ID"))

combined_words <- combined_data %>% filter(item_type == "word")
combined_words$value[is.na(combined_words$value)] <- "neither"


# Combine wordbank and Web-CDI data into a single dataframe for later facet graphs


slim_wordbank_data <- vocab_data %>% filter(language == "English") %>% rename(id = data_id, study_group = source_name) %>% select(id, form, age, sex, study_group, mom_ed, production, comprehension, n) %>% mutate(source = "wordbank", mom_ed = fct_collapse(mom_ed, 
                               `Below Secondary` = c("None","Primary",
                                                     "Some Secondary"),
                               `Secondary` = c("Secondary", "Some College"),
                               `College and Above` = c("College", 
                                                       "Some Graduate", 
                                                       "Graduate")))

slim_webcdi_data <- combined_words %>% group_by(id, instrument, age, sex, study_group, mother_education, child_ethnicity, language_days_per_week, language_hours_per_day, annual_income, birth_weight,english, analysis, born_on_due_date, completed) %>% summarise(comprehension = sum(value == "understands" | value == "produces"), production = sum(value == "produces"), n = n(), source = "webcdi", form = NA, mom_ed = NA) %>% rename(sex_old = sex) %>% mutate(sex = ifelse(sex_old == "F", "Female", ifelse(sex_old == "M", "Male", NA)))

slim_webcdi_data$form[grepl('WG', slim_webcdi_data$instrument)] <- "WG"
slim_webcdi_data$form[grepl('WS', slim_webcdi_data$instrument)] <- "WS"

slim_webcdi_data$mom_ed[slim_webcdi_data$mother_education %in% 0:9] <- "Below Secondary"
slim_webcdi_data$mom_ed[slim_webcdi_data$mother_education %in% 10:15] <- "Secondary"
slim_webcdi_data$mom_ed[slim_webcdi_data$mother_education %in% 16:25] <- "College and Above"

slim_webcdi_data$instrument <- slim_webcdi_data$mother_education <- NULL

slim_webcdi_data <- slim_webcdi_data %>% ungroup()
slim_wordbank_data <- slim_wordbank_data %>% ungroup()

data_list <- c("slim_webcdi_data", "slim_wordbank_data", "prod_data", "prod_preds", "comp_preds")

for (i in data_list){
  write_feather(get(i), paste0("data/",i,".feather"))
}
