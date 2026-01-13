#--------------------------------------------------
# description

# This file is solely responsible for generating some output for a news
# article regarding the publication the first version of the data.

#--------------------------------------------------
# libraries

library(ggplot2)

#--------------------------------------------------
# read data

auto_data <- targets::tar_read(auto_data_cleaned_appended)

# geo data zips
plz_geo <- sf::st_read(
    file.path(
        config_paths()[["gebiete_path"]],
        "Postleitzahl",
        "2019",
        "PLZ.shp"
    ),
    quiet = TRUE
)

# geographical data for municipalities
district_geo <- sf::st_read(
    file.path(
        config_paths()[["gebiete_path"]],
        "Kreis",
        "2019",
        "VG250_KRS.shp"
    ),
    quiet = TRUE
)

# geo data for states
states  <- sf::st_read(
    file.path(
        config_paths()[["gebiete_path"]],
        "Bundesland",
        "2019",
        "VG250_LAN.shp"
    ),
    quiet = TRUE
)

#--------------------------------------------------
# create clean states data

district_union <- sf::st_union(district_geo)
states_clean <- sf::st_intersection(states, district_union)

#--------------------------------------------------
# Connect zipcodes and districts

plz_district <- sf::st_join(
    plz_geo,
    district_geo |> dplyr::select(AGS, GEN, geometry),
    left = TRUE,
    largest = TRUE
)

#--------------------------------------------------
# add created year

auto_data <- auto_data |>
    dplyr::mutate(
        created_year = as.numeric(substring(created_date, 1, 4))
    )

#--------------------------------------------------
# subset to recent years
# for faster data processing + most information is available for recent years

auto_data_recent <- auto_data |>
    dplyr::filter(created_year == 2024)

#--------------------------------------------------
# add zip-code to munic data

auto_data_merged <- merge(
    auto_data_recent,
    plz_district |> sf::st_drop_geometry(),
    by.x = "zipcode",
    by.y = "PLZ",
    all.x = TRUE
)

#--------------------------------------------------
# calculate the share of offered electric cars

electric_share <- auto_data_merged |>
    dplyr::mutate(
        electric_share = dplyr::case_when(
            fuel_type == "Electric" ~ 1,
            TRUE ~ 0
        )
    ) |>
    dplyr::group_by(AGS) |>
    dplyr::summarise(
        district_name = dplyr::first(GEN),
        electric_share_perc = round(
            (sum(electric_share) / dplyr::n()) * 100,
            2
        )
    )

data.table::fwrite(
    electric_share,
    file.path(
        config_paths()[["output_path"]],
        "miscellaneous",
        "electric_share.csv"
    )
)

# calculate change in electric share
# electric_share <- electric_share |>
#     dplyr::group_by(AGS_kreis) |>
#     dplyr::mutate(
#         electric_share_change = electric_share - dplyr::lag(electric_share, 1)
#     )

#--------------------------------------------------
# merge geo data with electric share

electric_share_geo <- merge(
    electric_share |> as.data.frame(),
    district_geo |> dplyr::select(AGS, geometry),
    by = "AGS",
    left = TRUE
)

electric_share_geo <- sf::st_as_sf(electric_share_geo)

sf::st_write(
    electric_share_geo,
    file.path(
        config_paths()[["output_path"]],
        "miscellaneous",
        "electric_share.gpkg"
    ),
    quiet = TRUE,
    append = FALSE
)

#--------------------------------------------------
# create map

# change top end to 10+
quantile(electric_share$electric_share_perc, probs = seq(0, 1, 0.01))
electric_share_geo <- electric_share_geo |>
    dplyr::mutate(
        electric_share_perc = dplyr::case_when(
            electric_share_perc >= 10 ~ 10,
            TRUE ~ electric_share_perc
        )
    )

# define breaks
brk <- seq(
    round(min(electric_share_geo$electric_share_perc, na.rm = TRUE), digits = -1),
    round(max(electric_share_geo$electric_share_perc, na.rm = TRUE), digits = -1),
    by = 2
)

lbl <- c(as.character(brk[1:(length(brk)-1)]), "10+")

# create map
electric_share_map <- ggplot()+
    geom_sf(
        data = electric_share_geo,
        mapping = aes(fill = electric_share_perc),
        color = NA
    )+
    scale_fill_viridis_c(
        option = "magma",
        direction = -1,
        breaks = brk,
        labels = lbl,
        name = "Anteil Elektroautos (%)"
    )+
    geom_sf(
        data = states_clean,
        fill = NA,
        color = "black",
        linewidth = 0.8
    )+
    ggtitle(
        "Anteil angebotener Elektroautos in Deutschland\n(Kreisebene, 2024)"
    )+
    theme_void()+
    theme(
        legend.title = element_text(size = 18, vjust = 0.8),
        legend.text = element_text(size = 16),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold")
    )

ggsave(
    plot = electric_share_map,
    filename = file.path(
        config_paths()[["output_path"]],
        "miscellaneous",
        "electric_share_map.png"
    ),
    dpi = 400,
    width = 10,
    height = 11
)

#--------------------------------------------------
# generate bar plot
# NOTE: adjust data to more years if you want to redo the plot

# electric_share_plot <- ggplot2::ggplot(
#     electric_share |> dplyr::filter(created_year >= 2019),
#     ggplot2::aes(
#         x = created_year,
#         y = electric_share * 100
#     )
# ) +
#     scale_y_continuous(limits = c(0, 100))+
#     geom_text(
#         aes(label = scales::percent(electric_share, accuracy = 0.1)),
#         vjust = -0.5,
#         size = 7
#     ) +
#     ggplot2::geom_col(fill = "#0f5e86") +
#     ggplot2::labs(
#         title = "Share of Electric Cars Offered",
#         x = "Year",
#         y = "Share of Electric Cars"
#     ) +
#     ggplot2::theme_minimal()+
#     ggplot2::theme(
#         plot.title = ggplot2::element_text(size = 18, face = "bold"),
#         axis.text = ggplot2::element_text(size = 14),
#         axis.title = ggplot2::element_text(size = 16)
#     )

# ggsave(
#     plot = electric_share_plot,
#     file.path(
#         config_paths()[["output_path"]],
#         "miscellaneous",
#         "electric_share_plot.png"
#     ),
#     dpi = 400
# )
