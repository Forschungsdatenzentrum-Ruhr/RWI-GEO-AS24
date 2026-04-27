# TODO: Adjust error codes cad#1 to cad#xx (consistent numbering)
cleaning_auto_data <- function (
    auto_data = NA,
    municipalities = NA
) {
    #' @title Clean AutoScout24 data
    #' 
    #' @description This function cleans the raw AutoScout24 dataset.
    #' 
    #' @param auto_data Dataframe with raw AutoScout24 data
    #' @param municipalities Spatial dataframe with municipality shapes
    #' 
    #' @return Dataframe with cleaned AutoScout24 data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # convert dates to characters

    auto_data_prep <- auto_data |>
        dplyr::mutate(
            dplyr::across(
                .cols = c("firstregistration", "createddate"),
                ~ as.character(.x)
            )
        )

    #--------------------------------------------------
    # adjust type of zip code

    if (typeof(auto_data_prep$zip) != "character") {
        auto_data_prep$zip <- as.character(auto_data_prep$zip)
    }

    #--------------------------------------------------
    # handle logical variables

    logical_cols <- auto_data_prep |>
        dplyr::select(
            dplyr::where(is.logical)
        ) |>
        names()

    for (col in logical_cols) {
        # check that column exists
        targets::tar_assert_true(
            col %in% names(auto_data_prep),
            msg = glue::glue(
                "!!! WARNING: ",
                "The column {col} does not exist in the data set.",
                " (Error code: cad#1)"
            )
        )

        # check that columns only contains TRUE, FALSE, or NA
        targets::tar_assert_true(
            all(unique(auto_data_prep[[col]]) %in% c(NA, TRUE, FALSE)),
            msg = glue::glue(
                "!!! WARNING: ",
                "The column {col} contains values other than TRUE, FALSE, or NA.",
                " (Error code: cad#2)"
            )
        )

        # redefine logical values
        auto_data_prep <- auto_data_prep |>
            dplyr::mutate(
                !!col := dplyr::case_when(
                    .data[[col]] == TRUE ~ 1,
                    .data[[col]] == FALSE ~ 0,
                    TRUE ~ .data[[col]]
                ) |> as.numeric()
            )
    }

    # TODO: move line 250 (type setting)

    #--------------------------------------------------
    # set missing values according to variable type
    # NOTE: other missing types will be specified throughout the cleaning

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::where(is.numeric),
                ~ dplyr::case_when(
                    is.na(.x) ~ helpers_missing_values()[["not_specified"]],
                    TRUE ~ .x
                )
            ),
            dplyr::across(
                .cols = dplyr::where(is.character),
                ~ dplyr::case_when(
                    is.na(.x) ~ as.character(
                        helpers_missing_values()[["not_specified"]]
                    ),
                    .x == "" ~ as.character(
                        helpers_missing_values()[["not_specified"]]
                    ),
                    TRUE ~ .x
                )
            ),
            dplyr::across(
                .cols = dplyr::where(is.logical),
                ~ dplyr::case_when(
                    is.na(.x) ~ helpers_missing_values()[["not_specified"]],
                    TRUE ~ .x
                )
            )
        )

    #--------------------------------------------------
    # exclude variables
    
    for (col in helpers_deleted_variables()) {
        if (col %in% names(auto_data_prep)) {
            # extract unique values
            unique_values <- unique(auto_data_prep[[col]])

            # for fornewmarket check that always 0 because removing
            if ("fornewmarket" %in% names(auto_data_prep)) {
                unique_values <- unique(auto_data_prep[["fornewmarket"]])
        
                unique_values <- unique_values[
                    !unique_values %in% helpers_missing_values()[["all_missings"]]
                ]
                targets::tar_assert_true(
                    unique_values == 0,
                    msg = glue::glue(
                        "!!! WARNING: ",
                        "The unique value for {col} is not as expected.",
                        " (Error code: cad#4)"
                    )
                )
            }

            # for currencyid: it should be all missing as all prices are in Euro
            if ("currencyid" %in% names(auto_data_prep)) {
                unique_values <- unique(auto_data_prep[["currencyid"]])

                targets::tar_assert_true(
                    all(
                        unique_values %in% c(
                            helpers_missing_values()[["not_specified"]],
                            "EUR"
                        )
                    ),
                    msg = glue::glue(
                        "!!! WARNING: ",
                        "The unique value for {col} is not as expected.",
                        " (Error code: cad#5)"
                    )
                )
            }

            # for stateid: check if always 'A'
            if ("stateid" %in% names(auto_data_prep)) {
                unique_values <- unique(auto_data_prep[["stateid"]])

                unique_values <- unique_values[
                    !unique_values %in% helpers_missing_values()[["all_missings"]]
                ]
                targets::tar_assert_true(
                    unique_values == "A",
                    msg = glue::glue(
                        "!!! WARNING: ",
                        "The unique value for {col} is not as expected.",
                        " (Error code: cad#6)"
                    )
                )
            }

            # remove column
            auto_data_prep[[col]] <- NULL
        }
    }

    #--------------------------------------------------
    # merge countrycode and country_id and fix differences
    # NOTE: coalesce cannot be used as NA are redefined already

    if (!"countrycode" %in% names(auto_data_prep)) {
        auto_data_prep$countrycode <- NA_character_
    }

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            countrycode_aux = NA_character_,
            countrycode_aux = dplyr::case_when(
                country_id == "D" ~ "DE",
                country_id == "F" ~ "FR", 
                country_id == "I" ~ "IT", 
                country_id == "NL" ~ "NL", 
                country_id == "A" ~ "AT", 
                countrycode == "DE" ~ "DE",
                TRUE ~ countrycode_aux
            )
        )  |>
        dplyr::select(-c("country_id", "countrycode")) |>
        dplyr::rename(countrycode = countrycode_aux)

    # check that countrycode only includes expected values
    unique_countrycode <- unique(auto_data_prep$countrycode)[
        !unique(auto_data_prep$countrycode) %in% as.character(helpers_missing_values()[["all_missings"]])
    ]

    targets::tar_assert_true(
        all(
            unique(auto_data_prep$countrycode) %in%
                config_globals()[["possible_country_codes"]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "The country codes do not match the expected values.",
            " (Error code: cad#7)"
        )
    )

    # check that all cases have been considered
    unique_country_id_raw <- unique(auto_data$country_id)[
        !is.na(unique(auto_data$country_id))
    ]

    unique_countrycode_raw <- unique(auto_data$countrycode)[
        !is.na(unique(auto_data$countrycode))
    ]

    targets::tar_assert_true(
        all(unique_country_id_raw %in% c("D", "A", "F", "NL", "I")),
        msg = glue::glue(
            "!!! WARNING: ",
            "The country ID does not match the expected values.",
            " (Error code: cad#8)"
        )
    )

    targets::tar_assert_true(
        all(unique_countrycode_raw %in% c("DE", "AT", "FR", "NL", "IT")),
        msg = glue::glue(
            "!!! WARNING: ",
            "The country codes do not match the expected values.",
            " (Error code: cad#9)"
        )
    )


    #--------------------------------------------------
    # fix variable types for February 2025 delivery
    # TODO: Generalize: if variables do not have type -> set type
    # move to the top
    #
    if (config_globals()[["current_delivery"]] == "Feb_2025") {
        auto_data_prep <- auto_data_prep |>
            dplyr::mutate(
                dplyr::across(c("modelid", "mileage", "bodytypeid"), as.integer),
                dplyr::across(c("price"), as.numeric),
                dplyr::across(c("zip"), as.character)
            )
    }

    #--------------------------------------------------
    # set missings because some variables are not available for all years

    if (config_globals()[["current_delivery"]] == "Jan_2025") {
        older_data <- auto_data_prep |>
            dplyr::filter(grepl("2019|2020|2021", origin))

        newer_data <- auto_data_prep |>
            dplyr::filter(!grepl("2019|2020|2021", origin))

        for (var in helpers_newer_variables()) {
            # make sure that the variable is always missing
            if (typeof(older_data[[var]]) == "character") {
                statement <- unique(older_data[[var]]) == as.character(
                    helpers_missing_values()[["not_specified"]]
                )
            } else {
                statement <- unique(older_data[[var]]) == helpers_missing_values()[["not_specified"]]
            }
            
            targets::tar_assert_true(
                statement,
                msg = glue::glue(
                    "!!! WARNING: ",
                    "The variable {var} is not missing in the older data.",
                    " (Error code: cad#10)"
                )
            )

            # swap missing type
            if (typeof(older_data[[var]]) == "character") {
                older_data[[var]] <- as.character(
                    helpers_missing_values()[["not_available"]]
                )
            } else {
                older_data[[var]] <- helpers_missing_values()[["not_available"]]
            }
        }

        # combine the two datasets
        auto_data_prep <- rbind(older_data, newer_data)
    } else {
        # NOTE: add newer variables if needed
        for (var in helpers_newer_variables()) {
            if (!var %in% names(auto_data_prep)) {
                auto_data_prep[[var]] <- helpers_missing_values()[["not_available"]]
            }
        }

        # adjust types
        auto_data_prep <- auto_data_prep |>
            dplyr::mutate(
                dplyr::across(
                    .cols = c("transmissionid", "city", "country_zip_code"),
                    ~ as.character(.x)
                )
            )
    }

    # TODO: Check if really missing for other countries than D
    #--------------------------------------------------
    # calculate thresholds for censoring

    # define upper threshold
    # refers to 99 percentile
    # upper_threshold_value <- 0.999

    numeric_cols <- auto_data_prep |>
        dplyr::select(dplyr::where(is.numeric)) |>
        names()

    # exclude categorical variables
    numeric_cols <- numeric_cols[
        !numeric_cols %in% c(
            # variables with mapping table (therefore categorical)
            "makeid", "modelid", "customertypeid", "bodycolorid", "bodytypeid",
            "efficiencyclassid", "emissionpollutionclassid", "emissionstickerid",
            "equipmentfirst", "fuelid", "interiorcolorid",
            # logical variables
            "vatdeductible", "askingprice",
            # regional information
            "zip"
        )
    ]

    # calculate thresholds
    thresholds_list <- list()
    thresholds_dataframes_list <- list()
    for (col in numeric_cols) {
        thresholds_var <- helpers_calculate_censoring_threshold(
            auto_data = auto_data_prep,
            variable_of_interest = col,
            threshold = c(0.001, 0.999),
            threshold_type = "percentile"
        )

        thresholds_list[[col]] <- thresholds_var
        thresholds_dataframes_list[[col]] <- thresholds_list[[col]][["dataframe"]]
    }

    # combine all thresholds for export
    all_thresholds <- data.table::rbindlist(thresholds_dataframes_list)

    # export
    openxlsx::write.xlsx(
        all_thresholds,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["next_version"]],
            "info",
            "thresholds_censoring.xlsx"
        ),
        rowNames = FALSE
    )

    # export as latex
    all_thresholds |>
        kableExtra::kbl(
            escape = FALSE,
            format = "latex",
            longtable = TRUE,
            align = "l",
            linesep = "",
            caption = "Censoring Thresholds for Implausbible Values",
            col.names = c(
                "Variable",
                "Threshold",
                "\\makecell[l]{Threshold\\\\type}",
                "Value"
            ),
            label = "thresholds_censoring"
        ) |>
        kableExtra::kable_styling(
            latex_options = c(
                "striped",
                "hold_position"
            ),
            # NOTE: color is defined in report latex file
            # see coding: \definecolor{user_gray}{rgb}{0.851,0.851,0.851}
            stripe_color = "user_gray"
        ) |>
        kableExtra::save_kable(
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["next_version"]],
                "info",
                "thresholds_censoring.tex"
            ),
            label = "tab:thresholds_censoring"
        )

    #--------------------------------------------------
    # convert implausible values to missings for numerical columns

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::all_of(numeric_cols),
                ~ dplyr::case_when(
                    (
                        .x <= thresholds_list[[dplyr::cur_column()]][["value"]][1] &
                        .x != helpers_missing_values()[["not_available"]] &
                        .x != helpers_missing_values()[["not_specified"]] &
                        .x != helpers_missing_values()[["other"]]
                    ) ~ helpers_missing_values()[["implausible"]],
                    (
                        .x >= thresholds_list[[dplyr::cur_column()]][["value"]][2] &
                        .x != helpers_missing_values()[["not_available"]] &
                        .x != helpers_missing_values()[["not_specified"]] &
                        .x != helpers_missing_values()[["other"]]
                    ) ~ helpers_missing_values()[["implausible"]],
                    TRUE ~ .x
                )
            )
        )

    #--------------------------------------------------
    # convert implausible values to missings for other variables

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            #--------------------------------------------------
            # first registration date
            # make sure date cannot be in the future
            firstregistration_year = substring(firstregistration, 1, 4) |>
                as.numeric(),
            firstregistration = dplyr::case_when(
                firstregistration_year > config_globals()[["max_year"]] ~ as.character(
                    helpers_missing_values()[["implausible"]]),
                TRUE ~ firstregistration
            ),
            #--------------------------------------------------
            # transmission ID
            transmissionid = dplyr::case_when(
                transmissionid == "A" ~ "Automatic",
                transmissionid == "M" ~ "Manual",
                transmissionid == "S" ~ "Semi-automatic",
                TRUE ~ as.character(helpers_missing_values()[["not_specified"]])
            ),
            #--------------------------------------------------
            # customer type ID
            # Encode customer type ID = C because this is not part of our
            # data delivery (see AS response in AS log entry 03022025-01 on GitHub)
            customertypeid = dplyr::case_when(
                customertypeid == "C" ~ as.character(helpers_missing_values()[["other"]]),
                TRUE ~ customertypeid
            ),
            #--------------------------------------------------
            # zip-code
            # remove white space around zip code "00  "
            # NOTE: this is a special case since somehow these are not regular
            # white-spaces
            # TODO: fix sufficient for different countries?
            zip = stringi::stri_replace_all_fixed(zip, "\uFFFC", ""),
            # fix that some zip codes include ".0" (especially earlier waves)
            zip = dplyr::case_when(
                stringr::str_detect(zip, "\\.0$") ~ stringr::str_remove(zip, "\\.0$"),
                TRUE ~ zip
            ),
            # fix other special cases
            zip = dplyr::case_when(
                stringr::str_detect(zip, "‘") ~ stringr::str_remove(zip, "‘"),
                stringr::str_detect(zip, "\\[") ~ stringr::str_remove(zip, "\\["),
                TRUE ~ zip
            ),
            # fix length of zip code strings
            zip = dplyr::case_when(
                # censor implausible values (values too short)
                nchar(zip) < 4 ~ as.character(
                    helpers_missing_values()[["implausible"]]
                ),
                # add leading zeros for 4-digit zip codes
                nchar(zip) == 4 & countrycode == "DE" ~ stringr::str_pad(
                    zip,
                    width = 5,
                    side = "left",
                    pad = "0"
                ),
                nchar(zip) > 6 ~ as.character(
                    helpers_missing_values()[["implausible"]]
                ),
                TRUE ~ zip
            ),
            #--------------------------------------------------
            # creation date
            # make sure date cannot be in the future
            createddate_year = substring(createddate, 1, 4) |>
                as.numeric(),
            createddate = dplyr::case_when(
                createddate_year > config_globals()[["max_year"]] ~ as.character(
                    helpers_missing_values()[["implausible"]]),
                TRUE ~ createddate
            )
        ) |>
        # drop temporary variable
        dplyr::select(-c("firstregistration_year", "createddate_year"))

    #--------------------------------------------------
    # testing for zip codes and transmission ID
    # TODO: adapt for other countries

    # test that zip codes are 5 digits long
    unique_values_zip <- unique(auto_data_prep$zip)
    unique_values_zip <- unique_values_zip[
        !unique_values_zip %in% helpers_missing_values()[["all_missings"]]
    ]

    targets::tar_assert_true(
        all(nchar(unique_values_zip) %in% c(4,5,6)),
        msg = glue::glue(
            "!!! WARNING: ",
            "The zip codes do not have the expected length of 4, 5 or 6 digits.",
            " (Error code: cad#12)"
        )
    )

    # test that transmissionID only contains three types
    # NOTE: test on original data before recoding into numbers
    targets::tar_assert_true(
        all(unique(auto_data$transmissionid) %in% 
            c("A", "M", "S", "", NA) # NOTE: NA is not yet recoded
        ) == TRUE,
        msg = glue::glue(
            "!!! WARNING: ",
            "The transmissionid contains values other than 'A', 'M', or 'S'. ",
            "Adjust recoding.",
            " (Error code: cad#13)"
        )
    )
    
    #--------------------------------------------------
    # calculate the number of implausible values for reporting

    calculating_implausible_values <- function(col) {
        #' @param col Column name

        if (typeof(auto_data_prep[[col]]) == "character") {
            count_implausible <- length(which(
                auto_data_prep[[col]] == helpers_missing_values()[["implausible"]]
            ))
        } else {
            count_implausible <- length(which(
                auto_data_prep[[col]] == helpers_missing_values()[["implausible"]]
            ))
        }

        # create dataframe
        dta <- data.frame(
            variable = col,
            implausible_values = count_implausible,
            implausible_values_perc = round(
                (count_implausible / nrow(auto_data_prep)) * 100,
                3
            )
        )

        return(dta)
    }

    implausible_values_list <- list()
    for (col in names(auto_data_prep)) {
        implausible_values_list[[col]] <- calculating_implausible_values(col)
    }

    implausible_values_df <- data.table::rbindlist(implausible_values_list) |>
        dplyr::arrange(dplyr::desc(implausible_values_perc))

    # check that all columns have been considered
    targets::tar_assert_true(
        nrow(implausible_values_df) == ncol(auto_data_prep),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all columns have been considered for the implausible values.",
            " (Error code: cad#14)"
        )
    )

    # export
    openxlsx::write.xlsx(
        implausible_values_df,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["next_version"]],
            "info",
            "implausible_values_shares.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # check for dates

    unique_dates <- list(
        "firstregistration" = unique(auto_data_prep$firstregistration)[
            !unique(auto_data_prep$firstregistration) %in% helpers_missing_values()[["all_missings"]]
        ],
        "createddate" = unique(auto_data_prep$createddate)[
            !unique(auto_data_prep$createddate) %in% helpers_missing_values()[["all_missings"]]
        ]
    )

    for (var in list()) {
        # check for length of character. If 10 it means "YYYY-MM-DD"
        targets::tar_assert_true(
            unique(nchar(unique_dates[[var]])) == 10,
            msg = glue::glue(
                "!!! WARNING: ",
                "The {var} dates do not have the expected length of 10 characters.",
                " (Error code: cad#15)"
            )
        )

        # check for no letters (in case of wrong format, e.g. "Jan 01, 2021")
        targets::tar_assert_true(
            all(grepl("[a-zA-Z]", unique_dates[[var]]) == FALSE),
            msg = glue::glue(
                "!!! WARNING: ",
                "The {var} dates do include letters",
                " (Error code: cad#16)"
            )
        )
    }

    #--------------------------------------------------
    # subset the unique municipality names for the next step
    # TODO: how to deal with this once you have more countries than Germany?
    # HR: We don't have the variable city for other countries

    munics <- unique(municipalities$name)

    #--------------------------------------------------
    # (partial) clean city name

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            # handle special characters
            city = stringi::stri_trans_general(
                city,
                "de-ASCII; Latin-ASCII"
            ),
            # some city names are just numbers (zip codes). use this information
            # to remove the number rows from city column
            city_zip = as.numeric(city),
            city = dplyr::case_when(
                !is.na(city_zip) ~ as.character(
                    helpers_missing_values()[["other"]]
                ),
                # remove clearly wrong values
                city %in% helpers_implausible_values()[["implausible_city"]] ~ as.character(
                    helpers_missing_values()[["implausible"]]
                ),
                # remove single quotes
                stringr::str_detect(city, "^'") ~ stringr::str_remove(city, "^'"),
                # remove digits that are not captures by city_zip strategy
                stringr::str_detect(city, "[0-9]") ~ as.character(
                    helpers_missing_values()[["implausible"]]
                ),
                # remove special characters
                stringr::str_detect(city, "---") ~ stringr::str_remove(city, "---"),
                stringr::str_detect(city, "--") ~ stringr::str_remove(city, "--"),
                stringr::str_detect(city, "=")  ~ stringr::str_remove(city, "="),
                stringr::str_detect(city, ",") ~ stringr::str_remove(city, ","),
                # fix short city names (3 letters and shorter)
                (
                    nchar(city) < 4 &
                    !city %in% munics |
                    city %in% as.character(helpers_missing_values()[["all_missings"]])
                ) ~ as.character(helpers_missing_values()[["implausible"]]),
                TRUE ~ city
            ),
            # capitalize first letter
            city = stringr::str_to_title(city)
        ) |>
        dplyr::select(-city_zip)

    # Test that city names do not include numbers
    # Important because sometimes people even list their cellphone number as city
    targets::tar_assert_true(
        all(
            unique(auto_data_prep$city)[
                !unique(auto_data_prep$city) %in% as.character(helpers_missing_values()[["all_missings"]])
            ] |>
                grepl(pattern = "[0-9]") == FALSE
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "The city names include numbers.",
            " (Error code: cad#17)"
        )
    )

    #--------------------------------------------------
    # test for country zipcodes

    # test that country_zip_code is only four letters long
    unique_country_zipcode <- unique(auto_data_prep$country_zip_code)[
        !unique(auto_data_prep$country_zip_code) %in% as.character(helpers_missing_values()[["all_missings"]])
    ]

    if (length(unique_country_zipcode) > 0) {
    targets::tar_assert_true(
        unique(nchar(unique_country_zipcode)) == 4,
        msg = glue::glue(
            "!!! WARNING: ",
            "The country zip codes do not have the expected length of 4 digits.",
            " (Error code: cad#18)"
        )
    )
    }

    # test that the country identifier in zipcode matches country code
    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            countrycode_aux = substring(country_zip_code, 1, 2)
        )

    targets::tar_assert_true(
        length(which(
            unique(auto_data_prep$countrycode_aux[
                !auto_data_prep$countrycode_aux %in% as.character(helpers_missing_values()[["all_missings"]])
            ]) !=
            auto_data_prep$countrycode
        )) == 0,
        msg = glue::glue(
            "!!! WARNING: ",
            "The country zip codes do not match the country code.",
            " (Error code: cad#19)"
        )
    )

    auto_data_prep$countrycode_aux <- NULL
    
    
    #--------------------------------------------------
    # add version and delivery of the data

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            carmkt_delivery = stringr::str_replace_all(
                config_globals()[["current_delivery"]],
                "_",
                "-"
            )
        )

    #--------------------------------------------------
    # remove origin (after all checks have been done)

    auto_data_prep$origin <- NULL

    #--------------------------------------------------
    # return

    return(auto_data_prep)
}