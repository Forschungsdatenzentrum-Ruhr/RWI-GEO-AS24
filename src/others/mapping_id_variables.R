mapping_id_variables <- function(
    auto_data = NA,
    mapping_tables = NA
    ) {
    #' @title Map variables with mapping tables
    #' 
    #' @description This function maps all variables that need mapping with the
    #' corresponding mapping tables.
    #' 
    #' @param auto_data Dataframe with cleaned AS data
    #' @param mapping_tables List with mapping tables
    #' 
    #' @return Dataframe with mapped variables
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # map all categorical variables with mapping tables

    for (var in config_globals()[["needs_mapping_vars"]]) {
        # NOTE: not all listed variables that have a mapping table are still in
        # the data. For example: offertypeid (See also cleaning function)
        if (var %in% names(auto_data)) {
            #--------------------------------------------------
            # check that variable actually has a mapping table

            targets::tar_assert_true(
                var %in% names(mapping_tables),
                msg = glue::glue(
                    "Variable {var} does not have a mapping table. ",
                    "Please add the mapping table to the mapping_tables list. ",
                    " (Error code: miv#1)"
                )
            )

            #--------------------------------------------------
            # subset mapping table for variable

            mapping_table <- mapping_tables[[var]]

            #--------------------------------------------------
            # replace with missings if not in mapping tables
            # NOTE: this is only a partial fix. we need clarification from AS24
            # why these are missings in the mapping tables
            # TODO: remove once the mapping tables are updated

            if (var == "makeid") {
                auto_data <- auto_data |>
                    dplyr::mutate(
                        makeid = dplyr::case_when(
                            makeid %in% c(
                                51768,
                                16412,
                                51808,
                                51792,
                                16405,
                                51771,
                                16425,
                                16437,
                                16391,
                                51904,
                                16406, 
                                16417, 
                                20137, 
                                16392,
                                50981
                            ) ~ helpers_missing_values()[["other"]],
                            TRUE ~ makeid
                        )
                    )
            }

            if (var == "modelid") {
                auto_data <- auto_data |>
                    dplyr::mutate(
                        modelid = dplyr::case_when(
                            modelid %in% c(
                                # 76880,
                                # 76874,
                                # 76873,
                                15165,
                                19220,
                                15944,
                                19219,
                                15720,
                                19233,
                                19753,
                                20914,
                                15820,
                                19509,
                                75257,
                                74963,
                                20768,
                                74443,
                                21164,
                                74263,
                                19666,
                                76141,
                                76140, 
                                20769, 
                                75256, 
                                21104, 
                                21068, 
                                20771, 
                                21069, 
                                20767, 
                                74961, 
                                74940, 
                                16075, 
                                20407, 
                                19507, 
                                19506, 
                                19504
                            ) ~ helpers_missing_values()[["other"]],
                            TRUE ~ modelid
                        )
                    )
            }

            if (var == "emissionstickerid") {
                auto_data <- auto_data |>
                    dplyr::mutate(
                        emissionstickerid = dplyr::case_when(
                            emissionstickerid == 5 ~ helpers_missing_values()[["other"]],
                            TRUE ~ emissionstickerid
                        )
                    )
            }

            #--------------------------------------------------
            # check that unique values in the data can be found in the mapping tables

            unique_present_values <- unique(auto_data[[var]])

            not_included_in_mapping <- unique_present_values[
                !unique_present_values %in% unique(mapping_table$mapping_id)
            ]

            # make sure that the not-included values are not missing values
            truely_not_included_in_mapping <- not_included_in_mapping[
                !not_included_in_mapping %in% helpers_missing_values()[["all_missings"]]
            ]

            # actual test
            targets::tar_assert_true(
                length(truely_not_included_in_mapping) == 0,
                msg = glue::glue(
                    "The following values are not included in the mapping table for {var}:",
                    paste(truely_not_included_in_mapping, collapse = ", "),
                    " (Error code: miv#2)"
                )
            )
            
            #--------------------------------------------------
            # merge actual data and mapping table

            auto_data <- merge(
                auto_data,
                mapping_table,
                by.x = var,
                by.y = "mapping_id",
                all.x = TRUE
            )

            #--------------------------------------------------
            # check that missing values in mapping value are actually missings

            missing_values <- auto_data |>
                dplyr::filter(
                    is.na(mapping_value)
                )

            targets::tar_assert_true(
                all(
                    unique(missing_values[[var]]) %in%
                        helpers_missing_values()[["all_missings"]]
                ),
                msg = glue::glue(
                    "The following values are missing in the mapping table for {var}:",
                    paste(unique(missing_values[[var]]), collapse = ", "),
                    " (Error code: miv#3)"
                )
            )

            #--------------------------------------------------
            # replace missings in mapping variable with missing values in
            # original variable

            # get data type of mapping variable
            type_new_var <- typeof(auto_data$mapping_value)

            # replace missings with missing encoding
            auto_data <- auto_data |>
                dplyr::mutate(
                    mapping_value = dplyr::case_when(
                        is.na(mapping_value) ~ as(.data[[var]], type_new_var),
                        TRUE ~ mapping_value
                    )
                )

            #--------------------------------------------------
            # check that the same missing encodings have been assigned

            rows_with_missings <- auto_data |>
                dplyr::filter(
                    .data[[var]] %in% helpers_missing_values()[["all_missings"]]
                ) |>
                dplyr::mutate(
                    cols_equal = as(.data[[var]], type_new_var) == mapping_value
                )

            targets::tar_assert_true(
                length(which(rows_with_missings$cols_equal == FALSE)) == 0,
                msg = glue::glue(
                    "Some rows have different missing values in {var}.",
                    " (Error code: miv#4)"
                )
            )

            #--------------------------------------------------
            # drop original variable and rename mapping variable

            auto_data <- auto_data |>
                dplyr::mutate(
                    !!var := NULL
                ) |>
                dplyr::rename(
                    !!var := mapping_value
                )

            #--------------------------------------------------
            # check that there not "real missings", i.e. observations that do
            # not have been recode or assigned

            targets::tar_assert_true(
                length(which(is.na(auto_data[[var]]))) == 0,
                msg = glue::glue(
                    "There are {real_missings} real missings in {var}.",
                    " (Error code: miv#5)"
                )
            )
        }
    }

    #--------------------------------------------------
    # return

    return(auto_data)
}