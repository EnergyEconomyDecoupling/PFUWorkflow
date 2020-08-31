#' Title
#'
#' @param .df
#' @param year
#' @param .values
#' @param machine
#' @param eu_product
#' @param machine_eu_product
#'
#' @return
#' @export
#'
#' @examples
alloc_graph <- function(.df,
                        year = IEATools::iea_cols$year,
                        .values = IEATools::template_cols$.values,
                        machine = IEATools::template_cols$machine,
                        eu_product = IEATools::template_cols$eu_product,
                        machine_eu_product = paste0(machine, "_", eu_product)) {
  .df %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[year]],
                                           y = .data[[.values]],
                                           fill = .data[[machine_eu_product]])) +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::ylab("Allocation [-]") +
    MKHthemes::xy_theme() +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank())
}


#' Title
#'
#' @param completed_allocation_tables_target
#' @param path
#' @param country
#' @param ef_product
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
alloc_plots_df <- function(completed_allocation_tables_target = SEAPSUTWorkflow::target_names$CompletedAllocationTables,
                           path = ".drake",
                           country = IEATools::iea_cols$country,
                           ef_product = IEATools::template_cols$ef_product,
                           destination = IEATools::template_cols$destination) {
  drake::readd(completed_allocation_tables_target, path = cache_path, character_only = TRUE) %>%
    dplyr::group_by(.data[[country]],
                    .data[[ef_product]],
                    .data[[destination]]) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      plots = purrr::map(data, alloc_graph)
    )
}
