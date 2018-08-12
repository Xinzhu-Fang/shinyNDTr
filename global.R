
library('fields')
library('ggplot2')
library("plotrix")
library('shinydashboard')
library('dplyr')


rm(list=ls())



# working directoy is initialized as the dir of run file
# setwd("..")
setwd("../NDTr")
# setwd("C:/Users/14868/Documents/GitHub/NDTr")
# setwd("/cbcl/cbcl01/xf15/NDTr")

# all_cl <- c("maximum correlation", "support vecotor machine", "poisson naive bayes")
# all_fp <- c("select_pvalue_significant_features","select or exclude top k features", "zscore_normalize")

# df_cl_fp <- data.frame(c(1, 1, 1), c(1, 1, 1), c(1, 1, 0))

all_cl <- c("max_correlation_CL", "svm_CL", "poisson_naive_bayes_CL")
all_fp <- c("select_k_features_FP", "zscore_FP")

df_cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))

colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp

req_dc_para <- c("CL", "CV_bDiag", "CV_repeat", "CV_resample", "CV_split", "DS_chosen_bin", "DS_type")

# req_dc_para_basic <-c("DS_bUse_all_levels")
#
# req_dc_para_basic_leve <- c()

all_input <- list()
input_id <- c("DC_script_mode","DC_displayed_script_name","DS_save_binned_to_disk","DC_save_script_to_disk","bin_save_raster_to_disk","bin_create_raster",
              "bin_bin_data", "bin_bin_width", "bin_bPlot", "bin_chosen_raster",
              "bin_end_ind", "bin_next_neuron", "bin_prefix_of_binned_file_name",
              "bin_new_raster",
              "bin_pre_neuron", "bin_raster_end_ind","bin_raster_start_ind",
              "bin_start_ind", "bin_step_size",
  "bin_uploaded_raster","bin_uploaded_raster_name",
  "CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
  "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample",
  "CV_split","DC_chosen_script", "DC_run_decoding", "DC_save_displayed_script", "DC_scriptize",
  "DC_uploaded_script","DC_uploaded_script_name", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels",
  "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",
  "DS_gen_var_to_use", "DS_type", "DS_uploaded_binned", "DS_uploaded_binned_name","FP", "FP_excluded_k",
  "FP_selected_k", "Plot_basic_result_type_to_plot", "Plot_TCT_result_type_to_plot",
  "script", "sidebarCollapsed", "sidebarItemExpanded")


input_label <- c("File type for generated script","Where you want the displayed script to be saved", "Save to disk","Save to disk","Save to disk",
  "Create raster",
                 "Bin the data", "Bin width", "Plot the data? (only for spike trains in .Rda file)", "Choose raster data directory",
                 "Index of the sample where the last bin ends (optional)", "next file","prefix of binned file name (e.g., data/binned/ZD)",
                 "New raster directory name (e.g., data/raster/Zhang_Desimone_7objects_raster_data_rda; by default, we append '_rda' to the matlab raster directory name)",
                 "previous file", "Index of the sample where the new raster data end","Index of the sample where the new raster data begin",
                 "Index of the sample where the first bin starts (optional)", "Step size",
                 "Upload a zipped file raster data (optional)", "Where you want the file to be unzipped",
                 "Classifier", "Coef0", "Cost", "Degree of polynomial",
                 "Gamma", "Kernel", "Test only at training times?", "Number of repeats of each level in each CV split", "Number of resampling runs",
                 "Number of cross validation split", "Show an existing script (optional)","Run decoding", "Save the script", "Generate script from gui configuration",
                 "Upload new script (optional)","Where you want this file to be saved", "Levels to use", "Variable to decode and to use", "Use all the levels of this variable?",
                 "Choose your binned data", "How many training level groups you will use?",  "Variable to decode",
                 "Variable to use", "Type of data source", "Upload new binned data (optional)", "Where you want this file to be saved", "Feature Preprocessors", "exclude top ? features (this will be applied second)",
                 "select top ? features (this will be applied first)", "Type of result to plot", "Type of result to plot",
                 NA, NA, NA)

lLabels <- as.list(input_label)
names(lLabels) <- input_id

binning_paras <- paste0("input$",c("bin_bin_width","bin_chosen_raster","bin_end_ind","bin_start_ind", "bin_step_size"))


temp_decoding_paras_id <<- c("CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
                             "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample",
                             "CV_split", "CV_repeat", "CV_resample", "CV_split", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels",
                             "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",
                             "DS_gen_var_to_use", "DS_type","FP", "FP_excluded_k",
                             "FP_selected_k")

temp_decoding_paras_input_id <<- paste0("input$", temp_decoding_paras_id)

move_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}





create_script_in_Rmd <- function(my_decoding_paras, rv) {



  print(names(my_decoding_paras))




  my_text = ""

my_text = paste0(my_text, "---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n",
                 "```{r setup, include=FALSE}\n",
                 "knitr::opts_chunk$set(echo = TRUE)\n",
                 "```\n")


my_text = paste0(my_text, "\n```{r}\n")

  my_text = paste0(my_text, "binned_file_name <-", "'",rv$binned_file_name,"'", "\n")



  my_text = paste0(my_text, "```\n")
  my_text = paste0(my_text, "\n```{r}\n")

  if(my_decoding_paras$DS_type == "basic_DS"){
    my_text = paste0(my_text, "variable_to_decode <-", "'",my_decoding_paras$DS_basic_var_to_decode,"'", "\n")
    my_text = paste0(my_text, "num_cv_splits <- ", my_decoding_paras$CV_split, "\n")

    my_text = paste0(my_text, "ds <- NDTr::basic_DS$new(binned_file_name, variable_to_decode, num_cv_splits)\n")
    # this one is bad because level_to_use can be passed from the previous selection
    # if(!is.null(my_decoding_paras$DS_basic_level_to_use)){
    if(!my_decoding_paras$DS_bUse_all_levels){
      my_text = paste0(my_text, "ds$num_repeats_per_level_per_cv_split <- ", my_decoding_paras$CV_repeat, "\n")
    }
  }




  my_text = paste0(my_text, "```\n")
  my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "cl <- NDTr::", my_decoding_paras$CL, "$new()\n")


  my_text = paste0(my_text, "```\n")
  my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "fps <- list(")


  select_k_text = ""
  norm_text = ""

  if(!is.null(my_decoding_paras$fps)){
    if(grepl(my_decoding_paras$FP, all_fp[2]) == TRUE){
      norm_text = paste0(my_text, "NDTr::", my_decoding_paras$FP[2], "$new()")

    }

    if(grepl(my_decoding_paras$FP, all_fp[1]) == TRUE){
      select_k_text = paste0(my_text, "NDTr::", my_decoding_paras$FP[2], "$new(","num_sites_to_use = ", my_decoding_paras$FP_selected_k, "num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k,")" )
    }
  }


  my_text = paste0(my_text,select_k_text, norm_text, ")\n")


  my_text = paste0(my_text, "```\n")
  my_text = paste0(my_text, "\n```{r}\n")

  my_text = paste0(my_text, "cv <- NDTr::standard_CV$new(ds, cl, fps)\n")

  my_text = paste0(my_text, "```\n")
  my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "DECODING_RESULTS <- cv$run_decoding()\n")
  my_text = paste0(my_text, "```\n")







  return(my_text)

}

create_script_in_R <- function(my_decoding_paras, rv) {



  print(names(my_decoding_paras))




  my_text = ""

  # my_text = paste0(my_text, "---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n",
  #                  "```{r setup, include=FALSE}\n",
  #                  "knitr::opts_chunk$set(echo = TRUE)\n",
  #                  "```\n")
  #
  # my_text = paste0(my_text, "\n```{r}\n")

  my_text = paste0(my_text, "binned_file_name <-", "'",rv$binned_file_name,"'", "\n")



  #   my_text = paste0(my_text, "```\n")
  #   my_text = paste0(my_text, "\n```{r}\n")

  if(my_decoding_paras$DS_type == "basic_DS"){
    my_text = paste0(my_text, "variable_to_decode <-", "'",my_decoding_paras$DS_basic_var_to_decode,"'", "\n")
    my_text = paste0(my_text, "num_cv_splits <- ", my_decoding_paras$CV_split, "\n")

    my_text = paste0(my_text, "ds <- NDTr::basic_DS$new(binned_file_name, variable_to_decode, num_cv_splits)\n")
    # this one is bad because level_to_use can be passed from the previous selection
    # if(!is.null(my_decoding_paras$DS_basic_level_to_use)){
    if(!my_decoding_paras$DS_bUse_all_levels){
      my_text = paste0(my_text, "ds$num_repeats_per_level_per_cv_split <- ", my_decoding_paras$CV_repeat, "\n")
    }
  }




  # my_text = paste0(my_text, "```\n")
  # my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "cl <- NDTr::", my_decoding_paras$CL, "$new()\n")


  # my_text = paste0(my_text, "```\n")
  # my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "fps <- list(")


  select_k_text = ""
  norm_text = ""

  if(!is.null(my_decoding_paras$fps)){
    if(grepl(my_decoding_paras$FP, all_fp[2]) == TRUE){
      norm_text = paste0(my_text, "NDTr::", my_decoding_paras$FP[2], "$new()")

    }

    if(grepl(my_decoding_paras$FP, all_fp[1]) == TRUE){
      select_k_text = paste0(my_text, "NDTr::", my_decoding_paras$FP[2], "$new(","num_sites_to_use = ", my_decoding_paras$FP_selected_k, "num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k,")" )
    }
  }


  my_text = paste0(my_text,select_k_text, norm_text, ")\n")


  # my_text = paste0(my_text, "```\n")
  # my_text = paste0(my_text, "\n```{r}\n")

  my_text = paste0(my_text, "cv <- NDTr::standard_CV$new(ds, cl, fps)\n")

  # my_text = paste0(my_text, "```\n")
  # my_text = paste0(my_text, "\n```{r}\n")


  my_text = paste0(my_text, "DECODING_RESULTS <- cv$run_decoding()\n")
  # my_text = paste0(my_text, "```\n")



  return(my_text)

}
