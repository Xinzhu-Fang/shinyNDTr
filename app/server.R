


function(input, output, session) {

  # shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))
  # shinyDirChoose(input, "bin_chosen_raster", roots = c(wd='.'))

  # shinyDirChoose(input, "bin_chosen_raster")


  # !
  raster_base_dir <- file.path(eval(getwd()),'data/raster') #"."
  bin_base_dir <- file.path(eval(getwd()),'data/binned') #"."
  script_base_dir <- file.path(eval(getwd()),'tests') #"."

  rv <- reactiveValues()

  rv$raster_base_dir <- raster_base_dir
  rv$raster_cur_dir_name <- NULL
  rv$raster_cur_neuron <- 1
  rv$raster_num_neuron <- NA
  rv$raster_cur_file_name <- NULL
  rv$raster_cur_data <- NULL
  rv$raster_bRda <- FALSE
  rv$raster_bMat <-FALSE

  # !
  rv$create_bin_function_run <- ""
  rv$create_raster_function_run <- ""

  rv$binned_base_dir <- bin_base_dir
  rv$binned_file_name <- NULL
  rv$binned_data <- NULL
  rv$binned_maximum_num_of_levels_in_all_var <- NULL
  rv$binned_all_var <- NULL

  rv$script_base_dir <- script_base_dir
  rv$script_name <- NULL
  rv$script <- NULL
  # only files meet specified files types will be shown. However, such dir shown as empty can still be choosed
  shinyFiles::shinyDirChoose(input, "bin_chosen_raster", roots = c(wd=raster_base_dir), filetypes = c("mat", "Rda"))
  shinyFiles::shinyFileChoose(input, "DS_chosen_bin", roots = c(wd=bin_base_dir), filetypes = "Rda")
  shinyFiles::shinyFileChoose(input, "DC_chosen_script", root =c(wd=script_base_dir, filetypes = c("R", "Rmd")))

  observe({
    req(input$bin_chosen_raster)

    rv$raster_cur_dir_name <- shinyFiles::parseDirPath(c(wd= rv$raster_base_dir),input$bin_chosen_raster)

    # # we need this second check because as soon as the buttin is clicked, an object instantiated and assigned to input$bin_chosen_raster
    # print(input$bin_chosen_raster)
    # print(rv$raster_cur_dir_name)
    req(rv$raster_cur_dir_name)

    temp_names_of_all_mat_files_in_raster_dir <-
      list.files(rv$raster_cur_dir_name, pattern = "\\.mat$")
    #
    if(length(temp_names_of_all_mat_files_in_raster_dir) > 0){
      rv$raster_bMat <- TRUE

      print(rv$raster_bMat)
      #

      print("mat")
    } else {
      rv$raster_bMat <-FALSE
      temp_names_of_all_rda_files_in_raster_dir <-
        list.files(rv$raster_cur_dir_name, pattern = "\\.Rda$")
      rv$raster_num_neuron <- length(temp_names_of_all_rda_files_in_raster_dir)

      if(rv$raster_num_neuron > 0){
        rv$raster_bRda <- TRUE
        print("rda")
        rv$raster_cur_file_name <- temp_names_of_all_rda_files_in_raster_dir[rv$raster_cur_neuron]
        load(file.path(rv$raster_cur_dir_name, rv$raster_cur_file_name))
        rv$raster_cur_data <- select(raster_data, starts_with("time."))
        # print(head(rv$raster_cur_data))
      } else{
        print("none")
        rv$raster_bRda <- FALSE
        # this doesn't work; observe is for action not calculation
        # validate("Only accept raster data in .mat or .Rda format !")

      }
    }
  })

  observe({
    req(input$DS_chosen_bin)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$binned_base_dir),input$DS_chosen_bin)
    print(temp_df_file)
    req(temp_df_file$datapath)
    rv$binned_file_name <- temp_df_file$datapath

    load(rv$binned_file_name)
    rv$binned_data <- binned_data
    rv$binned_maximum_num_of_levels_in_all_var <- max(apply(select(binned_data, starts_with("labels"))[,],2, function(x) length(levels(as.factor(x)))))
    rv$binned_all_var <- sub("labels.", "", names(select(binned_data, starts_with("labels"))))

  })

  observe({
    req(input$DC_chosen_script)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$script_base_dir),input$DC_chosen_script)
    print(temp_df_file)
    req(temp_df_file$datapath)
    rv$script_name <- temp_df_file$datapath
    rv$script <- readChar(rv$script_name, file.info(rv$script_name)$size)
  })

  # when unzip a file, the new file is unzipped to exdir with origianl name, thus there is no need to update input with chosen file name
  # observe({
  #   req(input$bin_uploaded_raster)
  #   temp_file_name <-input$bin_uploaded_raster$datapath
  #   print(temp_file_name)
  #
  #   updateTextInput(session, "bin_uploaded_raster_name", value = file.path(rv$raster_base_dir, basename(temp_file_name)))
  # })

  observeEvent(input$bin_save_raster_to_disk, {
    req(input$bin_uploaded_raster,input$bin_uploaded_raster_name )
    unzip(input$bin_uploaded_raster$datapath, exdir=input$bin_uploaded_raster_name)


  })
  observe({
    req(input$DS_uploaded_binned)
    temp_file_name <-input$DS_uploaded_binned$datapath
    updateTextInput(session, "DS_uploaded_binned_name", value = file.path(rv$binned_base_dir, basename(temp_file_name)))
  })

  observeEvent(input$DS_save_binned_to_disk, {
    req(input$DS_uploaded_binned,input$DS_uploaded_binned_name )
    move_file(input$DS_uploaded_binned$datapath,input$DS_uploaded_binned_name )


  })

  observe({
    req(input$DC_uploaded_script)
    temp_file_name <-input$DC_uploaded_script$datapath
    updateTextInput(session, "DC_uploaded_script_name", value = file.path(rv$script_base_dir, basename(temp_file_name)))
  })

  observeEvent(input$DC_save_script_to_disk, {
req(input$DC_uploaded_script,input$DC_uploaded_script_name )
    move_file(input$DC_uploaded_script$datapath,input$DC_uploaded_script_name )


  })

  observeEvent(input$bin_bin_data,{
    if(rv$raster_bRda){
      print(input$bin_start_ind)
      temp_call = paste0("NDTr::create_binned_data(rv$raster_cur_dir_name,",
                         "input$bin_prefix_of_binned_file_name,",
                         "input$bin_bin_width, input$bin_step_size")
      if(!is.na(input$bin_start_ind)){
        temp_call = paste0(temp_call, ",input$bin_start_ind")
      }
      if(!is.na(input$bin_end_ind)){
        temp_call = paste0(temp_call, ",input$bin_end_ind")
      }
      temp_call = paste0(temp_call,")")
      rv$create_bin_function_run <- temp_call
      eval(parse(text = temp_call))

    } else if(rv$raster_bMat){
      temp_call = paste0("NDTr::create_binned_data_from_matlab_raster_data(rv$raster_cur_dir_name,",
                         "input$bin_prefix_of_binned_file_name,",
                         "input$bin_bin_width, input$bin_step_size")
      if(!is.na(input$bin_start_ind)){
        temp_call = paste0(temp_call, ",input$bin_start_ind")
      }
      if(!is.na(input$bin_end_ind)){
        temp_call = paste0(temp_call, ",input$bin_end_ind")
      }
      temp_call = paste0(temp_call,")")
      rv$create_bin_function_run <- temp_call
      eval(parse(text = temp_call))

    }

  })

  observeEvent(input$bin_create_raster,{

    temp_call = paste0("NDTr::create_raster_data_from_matlab_raster_data(rv$raster_cur_dir_name,",
                       "input$bin_new_raster")
    if(!is.na(input$bin_start_ind)){
      temp_call = paste0(temp_call, ",input$bin_raster_start_ind")
    }
    if(!is.na(input$bin_end_ind)){
      temp_call = paste0(temp_call, ",input$bin_raster_end_ind")
    }
    temp_call = paste0(temp_call,")")
    rv$create_raster_funciton_run <- temp_call
    eval(parse(text = temp_call))


  })




rv_para <- reactiveValues()
rv_para$id <-  c("DS_chosen_bin", "DS_type","CL", "CV_repeat", "CV_resample","CV_split")




observeEvent(input$DC_scriptize,{
 # oberveEvent is executed before eventReactive
  if(input$DS_type == "basic_DS"){
    rv_para$id <- c(rv_para$id,"DS_basic_var_to_decode")
    if(!input$DS_bUse_all_levels){
      rv_para$id <- c(rv_para$id,  "DS_basic_level_to_use")
    }
  } else{
    rv_para$id <- c(rv_para$id,"DS_gen_var_to_use","DS_gen_var_to_decode", "DS_gen_num_training_level_groups")
  }


  rv_para$inputID <- paste0("input$", rv_para$id)

  eval(parse(text = paste0("req(", rv_para$inputID, ")")))

  rv_para$id_of_useful_paras <- c(rv_para$id, "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
                                  "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "DS_bUse_all_levels","FP", "FP_excluded_k",
                                  "FP_selected_k")

  if(!is.null(input$DS_gen_num_training_level_groups)){
    temp_training_level_groups <- paste0("input$DS_training_level_group_", c(1:input$DS_gen_num_training_level_groups))
    temp_testing_level_groups <- paste0("input$DS_testing_level_group_", c(1:input$DS_gen_num_testing_level_groups))
    rv_para$id_of_useful_paras <- c(rv_para$id_of_useful_paras, trainin_level_groups, testing_level_groups)
  }

  rv_para$values <- lapply(rv_para$inputID, function(i){
    eval(parse(text = i))
  })
  rv$script <- create_script(lDecoding_paras, rv)

  print(rv_para$values)
  lDecoding_paras <<- as.list(rv_para$values)
  lDecoding_paras <<- setNames(lDecoding_paras, rv_para$id)

  print(lDecoding_paras)
  print(lDecoding_paras$CL)
})

er_scriptize_action_error <- eventReactive(input$DC_scriptize,{


  # my_decoding_paras <<- paste0("my_",decoding_paras)

  temp_need = lapply(rv_para$id, function(i){
    eval(parse(text = paste0("need(input$", i, ", '", "You need to set ",eval(parse(text = paste0("lLabels$", i))), "')")))
  })


  do.call(validate, temp_need)


})
output$DC_scriptize_error <- renderText({
  er_scriptize_action_error()

  # do.call(validate, temp_need)
  # eval(parse(text = temp_val))
  rv$script <- create_script(input)
  print(rv$script)
})

  observeEvent(input$DC_run_decoding,{
    eval(parse(text = rv$script))
  })

  observeEvent(input$bin_pre_neuron,{
    if(rv$raster_cur_neuron > 1){
      rv$raster_cur_neuron <- rv$raster_cur_neuron - 1
      # print("pre")
      # print(rv$raster_cur_neuron)

    }

  })

  observeEvent(input$bin_next_neuron,{
    if(rv$raster_cur_neuron < rv$raster_num_neuron){
      rv$raster_cur_neuron <- rv$raster_cur_neuron + 1
      # print(rv$raster_num_neuron)
      # print("next")
      # print(rv$raster_cur_neuron)

    }
  })



  reactive_validate_for_scriptizing <- reactive({

  })

  reactive_bRaster_qualified <- reactive({
    sum(rv$raster_bMat, rv$raster_bRda)
    # validate(
    #   need(!rv$raster_deamon, "Only accept .mat and .Rda format!! Please change your dataset")
    # )
  })

  reactive_bin_num_neuron <- reactive({

    # this error message doesn't show up now since datasource is on the first tab and DS is selected. I keep it here
    # as an example of using validate
    validate(
      need(input$DS_chosen_bin,"Please select data source first to get total number of neurons!")
    )
    binned_data = rv$binned_data
    length(unique(factor(binned_data$siteID)))
  })






  reactive_all_levels_of_basic_var_to_decode <- reactive({
    req(rv$binned_file_name)


    binned_data = rv$binned_data
    print(head(binned_data))
    print(input$DS_var_to_decode)
    levels(factor(binned_data[[paste0("labels.",input$DS_basic_var_to_decode)]]))

    # }
  })

  reactive_all_levels_of_gen_var_to_use <- reactive({
    req(rv$binned_file_name)

    binned_data = rv$binned_data
    print(head(binned_data))
    print(input$DS_var_to_decode)
    levels(factor(binned_data[[paste0("labels.",input$DS_gen_var_to_use)]]))

    # }
  })

  reactive_all_fp_avail <- reactive({
    req(input$CL)
    all_fp[df_cl_fp[,input$CL]>0]
  })

  er_bin_action_error <- eventReactive(input$bin_bin_data,{
    validate(
      need(rv$raster_cur_dir_name, "You haven't chosen the raster data yet!")
    )

    validate(
      need(rv$raster_bRda||rv$raster_bMat, "We only accept .mat and .Rda format !")
    )
  })


  er_bin_save_raster_to_disk_error <- eventReactive(input$bin_save_raster_to_disk,{
    validate(
      need(input$bin_uploaded_raster, paste0("Please ", lLabels$bin_uploaded_raster, "!")),
      need(input$bin_uploaded_raster_name, paste0("Please tell me ", lLabels$bin_uploaded_raster_name))
    )
  })

  er_DS_save_binned_to_disk_error <- eventReactive(input$DS_save_binned_to_disk,{
    validate(
      need(input$DS_uploaded_binned, paste0("Please ", lLabels$DS_uploaded_binned, "!")),
      need(input$DS_uploaded_binned_name, paste0("Please tell me ", lLabels$DS_uploaded_binned_name))
    )
  })
  er_DC_save_script_to_disk_error <- eventReactive(input$DC_save_script_to_disk,{
    validate(
      need(input$DC_uploaded_script, paste0("Please ", lLabels$DC_uploaded_script, "!")),
      need(input$DC_uploaded_script_name, paste0("Please tell me ", lLabels$DC_uploaded_script_name))
    )
  })

output$bin_action_error = renderUI({
  er_bin_action_error()

})

output$bin_save_raster_to_disk_error = renderUI({

  er_bin_save_raster_to_disk_error()

})
output$DS_save_binned_to_disk_error = renderUI({

  er_DS_save_binned_to_disk_error()

})
output$DC_save_script_to_disk_error = renderUI({

  er_DC_save_script_to_disk_error()

})
  output$where = renderDataTable(input$bin_uploaded_raster)

  output$bin_offer_upload_raster = renderUI({
    list(
      fileInput("bin_uploaded_raster", lLabels$bin_uploaded_raster, multiple = TRUE),

      textInput("bin_uploaded_raster_name", lLabels$bin_uploaded_raster_name, rv$raster_base_dir),
      actionButton("bin_save_raster_to_disk", lLabels$bin_save_raster_to_disk),
      uiOutput("bin_save_raster_to_disk_error")
    )


  })

  output$DS_offer_upload_bin = renderUI({
    list(
      fileInput("DS_uploaded_binned", lLabels$DS_uploaded_binned, multiple = TRUE),
      textInput("DS_uploaded_binned_name", lLabels$DS_uploaded_binned_name, rv$binned_base_dir),                                 actionButton("DS_save_binned_to_disk",lLabels$DS_save_binned_to_disk),
      uiOutput("DS_save_binned_to_disk_error")

    )
  })

  output$DC_offer_upload_script = renderUI({
    list(
      fileInput("DC_uploaded_script", lLabels$DC_uploaded_script, multiple = TRUE),
      textInput("DC_uploaded_script_name", lLabels$DC_uploaded_script_name, rv$script_base_dir),
      actionButton("DC_save_script_to_disk", lLabels$DC_save_script_to_disk),
      uiOutput("DC_save_script_to_disk_error")
    )
    })
  output$bin_offer_create_raster = renderUI({
    req(rv$raster_cur_dir_name)


    # req(input$bin_chosen_raster)
    if(rv$raster_bMat){
      # checkboxInput("bin_bCreate_raster_in_rda",lLabels$bin_bCreate_raster_in_rda)
      temp_matlab_raster_dir_name <- rv$raster_cur_dir_name
      # if the directory name ends with _mat, remove _mat
      temp_non_desired_pattern = '.*_mat$'
      if (grepl(temp_non_desired_pattern, temp_matlab_raster_dir_name) == TRUE){
        temp_r_raster_dir_name <- substr(temp_matlab_raster_dir_name, 1, nchar(temp_matlab_raster_dir_name) - 4)
      }

      # append Rda
      temp_r_raster_dir_name <- paste0(temp_r_raster_dir_name, "_rda/")

      list(
        helpText(paste0("We can bin raster data in .mat format, but do you want to create raster data in .Rda format? ",
                        "Benefits include the option to plot raster data ")),

        textInput("bin_new_raster", lLabels$bin_new_raster, temp_r_raster_dir_name),
        numericInput("bin_raster_start_ind", lLabels$bin_raster_start_ind, value = NULL),
        numericInput("bin_raster_end_ind", lLabels$bin_raster_end_ind, value = NULL),

        actionButton("bin_create_raster", lLabels$bin_create_raster))
    }
  })

  output$bin_evil_raster = renderUI({
    #
    req(rv$raster_cur_dir_name)
    validate(


      need(reactive_bRaster_qualified() > 0, "Only accept .mat and .Rda format!! Please change your dataset"))
  })


  output$bin_show_create_bin_function_run = renderText({
    rv$create_bin_function_run
  })

  output$bin_show_create_raster_function_run = renderText(({
    rv$create_raster_funciton_run
  }))


  output$bin_show_chosen_raster = renderText({
    # temp_text = "Chose raster"
    # rv$raster_cur_dir_name <- parseDirPath(c(wd=eval(getwd())),input$bin_chosen_raster)
    rv$raster_cur_dir_name
  })

  output$bin_show_raster_cur_file_name = renderText({
    paste0("current data shown:", "\n", rv$raster_cur_file_name)

  })

  output$bin_raster_plot = renderPlot({
    # print(head(rv$raster_cur_data))
    req(rv$raster_cur_data)
    temp_raster <-rv$raster_cur_data

    color2D.matplot(1 - temp_raster, border = NA, xlab = "Time (ms)",
                    ylab = "Trial")
  })

  output$bin_PSTH = renderPlot({
    req(rv$raster_cur_data)

    temp_raster <- rv$raster_cur_data
    plot(colSums(temp_raster, na.rm = FALSE, dims = 1)/nrow(temp_raster),
         xlab = "Time(ms)", ylab = "average firing rate")
  })


  output$DS_show_chosen_bin = renderText({

    rv$binned_file_name
  })

  output$DS_basic_list_of_var_to_decode = renderUI({
    req(rv$binned_file_name)

    selectInput("DS_basic_var_to_decode",
                lLabels$DS_basic_var_to_decode,
                rv$binned_all_var
                # c("")

    )

  })

  output$DS_gen_list_of_var_to_decode = renderUI({
    req(rv$binned_file_name)

    selectInput("DS_gen_var_to_decode",
                lLabels$DS_gen_var_to_decode,
                rv$binned_all_var
                # c("")

    )

  })

  output$DS_basic_list_of_levels_to_use = renderUI({

    selectInput("DS_basic_level_to_use",
                lLabels$DS_basic_level_to_use,
                reactive_all_levels_of_basic_var_to_decode(),
                multiple = TRUE)

  })
  #
  output$DS_gen_list_of_var_to_use = renderUI({
    req(rv$binned_file_name)

    selectInput("DS_gen_var_to_use",
                lLabels$DS_gen_var_to_use,
                rv$binned_all_var)
  })

  output$DS_gen_select_num_of_groups = renderUI({
    req(rv$binned_file_name)

    temp_max <- rv$binned_maximum_num_of_levels_in_all_var
    numericInput("DS_gen_num_training_level_groups",
                 lLabels$DS_gen_num_training_level_groups,
                 1,
                 min = 1,
                 max  = temp_max)
    # print(temp_max)
  })

  output$DS_gen_list_of_training_level_groups = renderUI({
    req(input$DS_gen_num_training_level_groups)
    temp_num <- input$DS_gen_num_training_level_groups
    # print(temp_num)
    # if(!is.null(temp_num)){
    temp_output <- lapply(1:temp_num, function(i){
      list(selectInput(paste0("DS_training_level_group_", i),
                       paste("Training level group", i),
                       reactive_all_levels_of_gen_var_to_use(),
                       multiple = TRUE
      ),
      selectInput(paste0("DS_testing_level_group_", i),
                  paste("Testing level group", i),
                  reactive_all_levels_of_gen_var_to_use(),
                  multiple = TRUE
      ))


    })
    # print(temp_output)
    temp_output <- unlist(temp_output, recursive = FALSE)
    # output <- do.call(c, unlist(temp_output, recursive=FALSE))
    # print(output)
    temp_output
    # }


  })







  output$FP_check_fp = renderUI({
    checkboxGroupInput("FP",
                       lLabels$FP,
                       reactive_all_fp_avail()
    )
  }
  )

  output$FP_select_k_features = renderUI({
    print(input$FP)
    if(sum(grepl(all_fp[1], input$FP))){
      print("FP")
      numericInput("FP_selected_k",
                   lLabels$FP_selected_k,
                   reactive_bin_num_neuron(),
                   min = 1,
                   max = reactive_bin_num_neuron())
    }




  })

  # we don't put exclude together with select because the max of exclude is contigent on select. Therefore, we also need the req()
  output$FP_exclude_k_features = renderUI({

    req(input$FP_selected_k)
    numericInput("FP_excluded_k",
                 lLabels$FP_excluded_k,
                 0,
                 min = 1,
                 max = reactive_bin_num_neuron() - input$FP_selected_k)
  })

  output$DC_show_chosen_script = renderText({
    rv$script_name
  })

  output$DC_ace = renderUI({
    shinyAce::aceEditor("script",
                        rv$script,
                        mode = "r")
    # mode = "markdown")

    # check all inputs and poentially send error message !

  })
}



