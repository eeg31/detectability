minprob <- 1e-8
default_sample <- 1000

low_var <- 25
mid_var <- 10
high_var <- 5
NA_var <- 2

load(file.path('data','incidence.rda'))
default_incidence$location <- default_incidence$ISO

to_scale <- c('Ebola virus disease', 'Marburg virus disease', 'Lassa virus disease',
              'leptospirosis', 'Rift Valley fever', 'epidemic typhus')
load(file.path('data','symptoms.rda'))
load(file.path('data','var.rda'))

n <- rownames(default_mode)
default_mode <- default_mode %>%
                mutate_all(~case_when(.=='NI' ~ as.character(minprob),
                                      as.numeric(.)>1-minprob ~ as.character(1-minprob),
                                      as.numeric(.)<minprob ~ as.character(minprob),
                                      TRUE~as.character(.))
                )
rownames(default_mode) <- n

default_var <- default_var %>%
               mutate_if(is.integer, as.double) %>%
               data.matrix
#               mutate_all(~case_when(.!='NA' & !is.na(.) ~ mid_var,
#                                     .=='NA' | is.na(.) ~ NA_var)) %>%
#               mutate(`fatigue/weakness`=high_var,
#                      anorexia=high_var,
#                      `abdominal pain`=high_var,
#                      hiccups=high_var) %>% #continue selecting high and low variation symptoms
#               data.matrix
rownames(default_var) <- rownames(default_mode)