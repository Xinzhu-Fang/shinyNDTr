

ui <- dashboardPage(
  dashboardHeader(title = "NDTr"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("binning the raster data", tabName = "bin"),
      menuItem("Population Decoding", tabName = "decode")#,
      # menuItem("Single Neuron Analysis", tabName = "single")

    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bin",
              navbarPage(title = "",
                         tabPanel(
                           title = "Upload new raster data",
                           fluidRow(
                             box(width = NULL,
                                 helpText("We only accept .mat and .Rda format !"),
                                 uiOutput("bin_offer_upload_raster"),
                                 uiOutput("bin_offer_create_raster"),
                                 uiOutput("bin_evil_raster"),
                                 textOutput("bin_show_create_raster_function_run")
                             )
                           )
                         ),

                         # tabPanel(
                         #   title = "Choose raster data",
                         #   fluidRow(
                         #     box(width = NULL,
                         #
                         #
                         #
                         #
                         #
                         #     )
                         #
                         #
                         #   )
                         # ),
                         tabPanel(
                           title = "Specifing binning parameters",
                           fluidPage(

                             fluidRow(
                               column(width = 8,
                                      box(width = NULL,
                                          shinyFiles::shinyDirButton("bin_chosen_raster", lLabels$bin_chosen_raster, ""),
                                          helpText("Full path of your chosen directory of raster data: "),

                                          textOutput("bin_show_chosen_raster"),
                                          numericInput("bin_bin_width", lLabels$bin_bin_width, value = 10, min = 1),
                                          numericInput("bin_step_size", lLabels$bin_step_size, value = 1, min = 1),
                                          numericInput("bin_start_ind", lLabels$bin_start_ind, value = NULL),
                                          numericInput("bin_end_ind", lLabels$bin_end_ind, value = NULL),
                                          textInput("bin_prefix_of_binned_file_name", lLabels$bin_prefix_of_binned_file_name),
                                          actionButton("bin_bin_data", lLabels$bin_bin_data),
                                          uiOutput("bin_action_error"),
                                          textOutput("bin_show_create_bin_function_run")                                      )



                               )

                             )
                           )
                         ),

                         tabPanel(

                           title = "Plot raster data",
                           fluidPage(
                             fluidRow(
                               column(width = 4,
                                      box(width = NULL,
                                          # shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
                                          # checkboxInput("bin_bPlot", lLabels$bin_bPlot),
                                          # conditionalPanel(condition = "input.bin_bPlot",
                                          actionButton("bin_pre_neuron", lLabels$bin_pre_neuron),
                                          actionButton("bin_next_neuron", lLabels$bin_next_neuron),
                                          textOutput("bin_show_raster_cur_file_name"),
                                          dataTableOutput('where')
                                      )
                               ) ,
                               column(width = 8,


                                      box(width = NULL,
                                          title = "Raster plot",
                                          color = "green", ribbon = TRUE, title_side = "top right",
                                          # conditionalPanel(condition = "input.bin_bPlot",

                                          plotOutput("bin_raster_plot")

                                      ),
                                      box(width = NULL,
                                          title = "PSTH (Peristimulus time histogram)",
                                          color = "red", ribbon = TRUE, title_side = "top right",
                                          # conditionalPanel(condition = "input.bin_bPlot",

                                          plotOutput("bin_PSTH")

                                      )
                               )
                             )
                           )
                         )




              )


      ),
      tabItem(tabName = "decode",
              navbarPage(title = "",
                         tabPanel(
                           title = "Specifing decoding papameters",
                           fluidPage(



                             fluidRow(
                               column(width = 6,
                                      tabBox(width = 12,
                                             height = 1000,

                                             tabPanel(
                                               title = "Upload new binned data",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               uiOutput("DS_offer_upload_bin")

                                             ),
                                             tabPanel(
                                               title = "Data source",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",

                                               shinyFiles::shinyFilesButton("DS_chosen_bin", lLabels$DS_chosen_bin, "", multiple = FALSE),
                                               helpText("Full path of your chosen binned data file: "),

                                               textOutput("DS_show_chosen_bin"),
                                               selectInput("DS_type", lLabels$DS_type, c("basic_DS","generalization_DS")),



                                               conditionalPanel(condition = "input.DS_type == 'basic_DS'",
                                                                uiOutput("DS_basic_list_of_var_to_decode"),

                                                                checkboxInput("DS_bUse_all_levels", "Use all the levels of this variable?", TRUE)
                                               ),
                                               conditionalPanel(condition = "!input.DS_bUse_all_levels && input.DS_type == 'basic_DS'",
                                                                uiOutput("DS_basic_list_of_levels_to_use")),


                                               conditionalPanel(condition = "input.DS_type == 'generalization_DS'",
                                                                uiOutput("DS_gen_list_of_var_to_decode"),
                                                                uiOutput("DS_gen_list_of_var_to_use"),
                                                                uiOutput("DS_gen_select_num_of_groups"),
                                                                uiOutput("DS_gen_list_of_training_level_groups")#,
                                                                # uiOutput("DS_gen_list_of_testing_level_groups")
                                               )
                                             ),

                                             tabPanel(
                                               title = "Classifier",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               selectInput("CL", "Classifier", all_cl),
                                               box(
                                                 width = NULL,
                                                 title = "Additional parameters (if applicable)",
                                                 conditionalPanel(condition  = "input.CL == 'svm_CL'",
                                                                  selectInput("CL_SVM_kernel",
                                                                              lLabels$CL_SVM_kernel,
                                                                              c("linear", "polynomial", "radial", "sigmoid"),
                                                                              selected = "linear"),
                                                                  numericInput("CL_SVM_cost",
                                                                               lLabels$CL_SVM_cost, # of constraints violation / inverse of regularization constant",
                                                                               1,
                                                                               min = 0
                                                                  ),
                                                                  conditionalPanel(condition ="input.CL_SVM_kernel == 'polynomial'",
                                                                                   numericInput("CL_SVM_degree",
                                                                                                lLabels$CL_SVM_degree,
                                                                                                3,
                                                                                                min = 2,
                                                                                                max  = 10)),


                                                                  conditionalPanel(condition = "input.CL_SVM_kernel == 'radial'|input.CL_SVM_kernel == 'polynomial'",
                                                                                   numericInput("CL_SVM_coef0",
                                                                                                lLabels$CL_SVM_coef0, # Constant in the kernel function",
                                                                                                0)),
                                                                  conditionalPanel(condition = "input.CL_SVM_kernel == 'radial'|input.CL_SVM_kernel == 'polynomial'|input.CL_SVM_kernel == 'sigmoid'",
                                                                                   numericInput("CL_SVM_gamma",
                                                                                                lLabels$CL_SVM_gamma,
                                                                                                NULL)
                                                                  )
                                                 ))

                                             ),
                                             tabPanel(
                                               title = "Feature preprocessors",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",

                                               uiOutput("FP_check_fp"),
                                               uiOutput("FP_select_k_features"),
                                               uiOutput("FP_exclude_k_features")


                                             ),
                                             tabPanel(
                                               title = "Cross validator",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               numericInput("CV_repeat", lLabels$CV_repeat, value = 2, min = 1),
                                               numericInput("CV_split", lLabels$CV_split, value = 5, min = 2),
                                               numericInput("CV_resample", lLabels$CV_resample, value = 20, min = 1),
                                               checkboxInput("CV_bDiag", lLabels$CV_bDiag,TRUE)
                                             ),
                                             tabPanel(
                                               title = "Generate Script",
                                               width = NULL,

                                               # box(

                                               helpText("If you choose to generate the script in R Markdown, to run the script first save it"),
                                               radioButtons("DC_script_mode", lLabels$DC_script_mode, c("r", "markdown"), selected = "markdown"),
                                               actionButton("DC_scriptize", "generate script from gui configuration"),
                                               uiOutput("DC_scriptize_error")
                                               # )
                                             ),
                                             tabPanel(
                                               title = "Other scripts",
                                               width = NULL,
                                               box(
                                                 title = "",
                                                 width = NULL,
                                                 uiOutput("DC_offer_upload_script")
                                               ) ,
                                               box(
                                                 title = "",
                                                 width = NULL,
                                                 # script will show upon chosen
                                                 shinyFiles::shinyFilesButton("DC_chosen_script", lLabels$DC_chosen_script, "", multiple = FALSE),
                                                 helpText("Full path of your chosen script: "),

                                                 textOutput("DC_show_chosen_script")
                                               )
                                             )

                                             # )
                                             # )
                                             #


                                             # )



                                      )),








                               column(width = 6,
                                      box(width = NULL,
                                          height = NULL,

                                          uiOutput("DC_ace"),
                                          uiOutput("DC_offer_run_decoding"),

                                          # textinput of filename to be saved if not existing and to be saved as if existing;
                                          uiOutput("DC_offer_save_displayed_script")
                                      )


                               )


                             )
                           )

                         ),
                         tabPanel(
                           title = "Plot decoding results",

                           column(width = 12,
                                  #issue cannot make use of the large blank on the right
                                  tabBox(width = 12,
                                         # title = "Result plot",
                                         tabPanel("timeplot",
                                                  selectInput("Plot_TCT_result_type_to_plot", lLabels$Plot_TCT_result_type_to_plot,
                                                              c("Zero-one loss", "Rank results", "Decision Values")),
                                                  plotOutput("timeplot")
                                         ),
                                         tabPanel("TCT heatmap",
                                                  selectInput("Plot_basic_result_type_to_plot", lLabels$Plot_basic_result_type_to_plot,
                                                              c("Zero-one loss", "Rank results", "Decision Values")),
                                                  plotOutput("tct")
                                         )

                                  )
                           )
                         )
              )
      )

    )


  )

)
