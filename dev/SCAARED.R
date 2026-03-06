# scoring the screen for adult anxiety related disorders (SCAARED)
# last updated: Feb. 2025
# author: helen schmidt

# replace labels with scores
scaared <- scaared |>
  mutate(across(.cols = c(anxiety_1:anxiety_44),
                ~case_when(. == "Not True or Hardly Ever True" ~ 0,
                           . == "Somewhat True or Sometimes True" ~ 1,
                           . == "Very True or Often True" ~ 2))) 

# calculate scores
# total >= 23 may indicate presence of anxiety disorder
# panic_somatic == 5 may indicate panic disorder / significant somatic symptoms
# GAD == 12 may indicate generalized anxiety disorder
# separation == 3 may indicate separation anxiety disorder
# social_phobia == 7 may indicate social phobis disorder
scaared <- scaared |>
  group_by(PID) |>
  summarize(total_anxiety = sum(c_across(anxiety_1:anxiety_44)),
            panic_somatic = sum(c(anxiety_1, anxiety_2, anxiety_6,
                                  anxiety_9, anxiety_11, anxiety_12,
                                  anxiety_15, anxiety_17, anxiety_18,
                                  anxiety_19, anxiety_22, anxiety_25,
                                  anxiety_28, anxiety_32, anxiety_36,
                                  anxiety_38, anxiety_40)),
            GAD = sum(c(anxiety_5, anxiety_7, anxiety_8, anxiety_14,
                        anxiety_21, anxiety_23, anxiety_24, anxiety_29,
                        anxiety_31, anxiety_35, anxiety_37,
                        anxiety_39, anxiety_44)),
            separation = sum(c(anxiety_4, anxiety_13, anxiety_16,
                               anxiety_20, anxiety_26, anxiety_30, anxiety_33)),
            social_phobia = sum(c(anxiety_3, anxiety_10, anxiety_27,
                                  anxiety_34, anxiety_41, anxiety_42,
                                  anxiety_43))) |>
  ungroup()