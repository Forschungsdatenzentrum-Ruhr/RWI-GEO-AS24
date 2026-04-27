reading_auto_data <- function(
    data_file_path = NA
) {
    #' @title Reading AutoScout24 data
    #' 
    #' @description This function reads all data from the delivery folder and
    #' combines it into one dataset.
    #' 
    #' @param data_file_path Character, path to the delivery folder.
    #' 
    #' @note Delivery Jan_2025 gets treated specially because it contains
    #' multiple years and some of them have fewer columns.
    #' 
    #' @return Dataframe, combined data.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # list all files in delivery folder

    # NOTE: potentially swap if and else statement (if "else" becomes the exception)
    if (config_globals()[["current_delivery"]] %in% c("Jan_2025", "Feb_2025")) {
        folders <- list.dirs(
            data_file_path,
            recursive = FALSE,
            full.names = FALSE
        )

        files <- c()
        for (folder in folders) {
            files <- c(
                files,
                list.files(
                    file.path(data_file_path, folder),
                    pattern = "*.csv",
                    full.names = TRUE
                )
            )
        }
    } else {
        files <- list.files(
            data_file_path,
            pattern = "*.csv",
            full.names = TRUE
        )
    }

    #--------------------------------------------------
    # read all data

    data_storage <- list()
    for (file in files) {
        dta <- data.table::fread(
            file,
            encoding = "UTF-8"
        )

        # add file to identify the data point if needed for debugging
        dta$origin <- file

        data_storage[[file]] <- dta
    }

    #--------------------------------------------------
    # check that all columns are in all partitions
    # NOTE: before 2022: there are fewer variables available (hence the checks
    # cannot be applied uniformly)

    if (config_globals()[["current_delivery"]] == "Jan_2025") {
        # split the data into before and after 2022
        data_before_2022 <- names(data_storage)[
            grepl("2019|2020|2021", names(data_storage))
        ]

        data_after_2022 <- names(data_storage)[
            !names(data_storage) %in% data_before_2022
        ]
    }

    # check that all columns are in all partitions
    checking_dimensions <- function(data_names = NA, data_label = NA) {
        #' @param data_names Vector with all data partitions.
        #' @param data_label Character, label for the data partitions. Needed for
        #' check
        
        num_cols <- list()
        names_cols <- list()

        for (dta in data_names) {
            num_cols[[dta]] <- ncol(data_storage[[dta]])
            names_cols[[dta]] <- colnames(data_storage[[dta]])
        }

        targets::tar_assert_true(
            length(unique(num_cols)) == 1,
            msg = glue::glue(
                "!!! WARNING:
                Number of columns in all partitions is not the same: {unique(num_cols)}.",
                " Check {data_label}.",
                " (Error code: rad#1)"
            )
        )

        targets::tar_assert_true(
            length(unique(names_cols)) == 1,
            msg = glue::glue(
                "!!! WARNING:
                Column names in all partitions are not the same: {unique(names_cols)}.",
                " Check {data_label}.",
                " (Error code: rad#2)"
            )
        )
    }

    if (config_globals()[["current_delivery"]] == "Jan_2025") {
        checking_dimensions(data_before_2022, data_label = "data before 2022")
        checking_dimensions(data_after_2022, data_label = "data after 2022")
    } else {
        checking_dimensions(names(data_storage), data_label = "all data")
    }
    
    #--------------------------------------------------
    # check variable types
    
    checking_variables_types <- function(data_names = NA, data_label = NA) {
        #' @param data_names Vector with all data partitions.
        #' @param data_label Character, label for the data partitions. Needed for
        #' check
    
        col_types_list <- list()
        for (dta in data_names) {
            # extract element name
            split_list <- stringr::str_split(dta, "/")
            for (element in split_list) {
                split <- element[length(element)]
                split <- stringr::str_replace_all(split, ".csv", "")
            }
            
            # determine column types
            col_types <- sapply(data_storage[[dta]], class) |>
                unlist() |>
                as.data.frame() |>
                dplyr::rename("col_type" = 1)
                
            # get column names
            col_types$col_name <- rownames(col_types)
            rownames(col_types) <- NULL

            # move column names to the front
            col_types <- col_types |>
                dplyr::relocate(col_name)

            # store
            col_types_list[[split]] <- col_types
        }

        # combine all column types dataframes
        col_types_all <- purrr::reduce(
            col_types_list,
            dplyr::full_join,
            by = "col_name"
        )

        # assign proper column names (so you know which partition the column type
        # belongs to)
        colnames(col_types_all) <- c("col_name", names(col_types_list))

        # check that all types are identical across partitions
        check_col_types <- col_types_all |>
            dplyr::rowwise() |>
            dplyr::mutate(
                all_equal = all(
                    dplyr::c_across(-col_name) == dplyr::first(dplyr::c_across(-col_name))
                )
            ) |>
            dplyr::pull(all_equal)

        # assign logical to types dataframe (to see where differences occur)
        col_types_all <- col_types_all |>
            dplyr::mutate(
                all_equal = check_col_types
            )

        # check if all types are the same
        for (var in unique(col_types_all$col_name)) {
            if(!var %in% config_fixed_variables_reading()) {
                targets::tar_assert_true(
                    col_types_all[col_types_all$col_name == var, ]["all_equal"] |>
                        dplyr::pull() == TRUE,
                    msg = glue::glue(
                        "!!! WARNING:
                        Column types are not the same across all partitions for {var}.",
                        " Check {data_label}.",
                        " (Error code: rad#3)"
                    )
                )
            }
        }
    }

    # apply check
    if (config_globals()[["current_delivery"]] == "Jan_2025") {
        checking_variables_types(data_before_2022, data_label = "data before 2022")
        checking_variables_types(data_after_2022, data_label = "data after 2022")

        #--------------------------------------------------
        # fix non-uniform type issues

        # variables with respect to data before 2022
        data_storage <- helpers_fixing_reading_types(
            data_names = data_before_2022,
            data_label = "data before 2022",
            data_storage_list = data_storage,
            target_var = "zip",
            desired_type = "character"
        )

        # variables with respect to data after 2022: numerical variables
        for (var in c(
            "modelid", "mileage", "bodytypeid", "fuelconsumptioncity",
            "fuelconsumptioncountry"
        )) {
            data_storage <- helpers_fixing_reading_types(
                data_names = data_after_2022,
                data_label = "data after 2022",
                data_storage_list = data_storage,
                target_var = var,
                desired_type = "double"
            )
        }

        # variables with respect to data after 2022: character variables
        for (var in c("currencyid")) {
            data_storage <- helpers_fixing_reading_types(
                data_names = data_after_2022,
                data_label = "data after 2022",
                data_storage_list = data_storage,
                target_var = var,
                desired_type = "character"
            )
        }

    } else {
        checking_variables_types(names(data_storage), data_label = "all data")
    }

    #--------------------------------------------------
    # combine all

    dta_prep <- data.table::rbindlist(
        data_storage,
        fill = TRUE
    )

    #--------------------------------------------------
    # check that dataframe is not empty

    targets::tar_assert_nonempty(
        dta_prep,
        msg = glue::glue(
            "!!! WARNING:",
            " Combined dataset is empty.",
            " (Error code: rad#4)"
        )
    )

    #--------------------------------------------------
    # return

    return(dta_prep)
}