

#' @export
#' @title Graphique des cas cumules jusqu'a date.
#' @description Evolution du comptage des cas.
#'
#' @import ggplot2
#' @param df_aggregated Data frame agrege (granulatite globale).
#' @param plot_date Date en caracteres ("2020-01-25").
#' @param y_var_name Nom de la variable a ploter.
#' @param ylabel Label axe des y.
#' @param col Code couleur ("#cc4c02")
#' @return ggplot2 object.
#' @family plot tools

cumulative_plot = function(df_aggregated, plot_date, y_var_name, ylabel, col) {
  plot_df = subset(df_aggregated, date<=plot_date)

  g1 = ggplot(plot_df, aes(x = date, y = !!y_var_name, color = "Global")) +
    geom_line() +
    geom_point(size = 1, alpha = 0.8) +
    ylab(ylabel) +
    theme_bw() +
    scale_colour_manual(values=col) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  g1
}



#' @export
#' @title Graphique des nouveaux cas jusqu'a date.
#' @description Evolution du comptage des nouveaux cas.
#'
#' @import ggplot2
#' @param df_aggregated Data frame agrege des donnees globales (cas/morts/nouveaux cas).
#' @param plot_date Date en caracteres ("2020-01-25").
#' @param y_var_name Nom de la variable a ploter.
#' @param ylabel Label axe des y.
#' @param col Code couleur ("#cc4c02")
#' @return ggplot2 object.
#' @family plot tools

new_cases_plot = function(df_aggregated, plot_date, y_var_name, ylabel, col) {
  plot_df_new = subset(df_aggregated, date<=plot_date)

  g1 = ggplot(plot_df_new, aes(x = date, y = !!y_var_name, fill = "Global")) +
    geom_bar(position="stack", stat="identity") +
    ylab(ylabel) + theme_bw() +
    scale_fill_manual(values=col) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  g1
}













