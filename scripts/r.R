binned_file_name <-'/student/15/xf15/GitHub/shinyNDTr/data/binned/ZD_150_samples_binned_every_10_samples_start_-300_end_300.Rda'
variable_to_decode <-'combined_ID_position'
num_cv_splits <- 5
ds <- NDTr::basic_DS$new(binned_file_name, variable_to_decode, num_cv_splits)
ds$num_repeats_per_level_per_cv_split <- 2
cl <- NDTr::max_correlation_CL$new()
fps <- list()
cv <- NDTr::standard_CV$new(ds, cl, fps)
DECODING_RESULTS <- cv$run_decoding()
save('DECODING_RESULTS', file = '/student/15/xf15/GitHub/shinyNDTr/results/r.rda')
