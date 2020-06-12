
# shinyNDTr
## curretn status
I pulled ehtan's changes then found bugs in my code. The pull was reverted and the bug need to be fixed yet.

## Getting started
* `install.packages(c("yaml","digest", "devtools", "curl"))`, which provide you with the basic tools for package development"
* For developlers:
   * Assume you working directory is a GitHub directory of all git repos
   1. `git clone https://github.com/Xinzhu-Fang/shinyNDTr.git`
   2. `git clone https://github.com/Xinzhu-Fang/NDTr.git`
   3. `cd shinyNDTr` 
   4. `devtools::install_local("../NDTr")`
* For users:
  1. do `devtools::install_github("Xinzhu-Fang/shinyNDTr")` to install shinyNDTr from GitHub.
  2. Because NDTr is not in CRAN yet, to install NDTr you could either do `devtools::install_github("Xinzhu-Fang/NDTr")`, which install NDTr from GitHub, or do `devtools::install_github(“r-lib/devtools”, ref = “7211ff8bff6d109fd1d9079080205306f4225ec8”)` so that the `Remotes` tag in `DESCRIPTION` is processed and the previous operation will be automatically done for you.
## Use the app
* `shiny::runApp('shinyNDTr')`


Welcome to the NDTr_Shiny_app wiki!

## to do
* sample -> data for the three DS functions
* decide if we want to use tidymodel and connect the app to svm
* decoding result doesn't contain information about DS and FP


## essential components
* chosen_raster, chosen_binned, chosen_script, and chosen_result are chosen from the server using shinyFiles
* to_be_saved_script and to_be_saved_result

## future enhancement
* use observe to call funcitons, so function is spilled before execution?
* sink console to output process?
* ggplot binary and continuous raster data; mitigate the part to single neuron analysis
* user uploaded data visibility
* show something like my tDecodingObject and tDecoding Protocol in a dataframe
* in tutorials, names of variables passed as arguments should be the name of the argument prefixed with something like "my_" 
* have the option of saving state https://github.com/aoles/shinyURL
* parallizing converting .mat raster to .Rda raster
* the font, "label for TextOutput"
* online database, hosting data on github is slowing down pushing and pulling
## to be implemented
* naming in dataframe
    * site_info.session_ID
    * labels.combined_ID_position
    * time.1_150
* public data organization in the project
    * data
        * raster
        * bin
        * result
* naming in code
    * implemented
        * specific_binned_label_names -> var_to_decode (should also applies to gen too)
        * label_names_to_use -> levels_to_use
        * specific_labels_names_to_use(in matlab tutorial)/specific_binned_label_name(in readout.info) -> var_to_use
        * the_training_label_numbers(in readout.info)/the_training_label_names(in matlab tutorial) -> training_level_groups
        *  the_testing_label_numbers(in readout.info)/the_testing_label_names(in matlab tutorial) -> testing_level_group
        * num_times_to_repeat_labels_per_cv_block -> num_repeats_per_level_per_cv_split

* bin_data_one_site vs.  bin_data_one_site2 
## questions
* shoudln't we copy import from namespace to description?
* dplyr depends on rlang, when I load dplyr, rlang is not installed. why?
* I put dpyr in library 
## what I testified
* `search()` list pakcages attached. 
## to be settled
* boxcar for binning
* time cannot be computed since site_info is not used. have to usa sample not time.
* more names
## Ethan to do
## shiny issues
* https://stackoverflow.com/questions/41706201/shiny-numericinput-does-not-respect-min-and-max-values

