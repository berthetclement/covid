globalVariables(c("region"))

#' @export
#' @title Graphique cumul avec plotly.
#' @description Evolution des cas/mors \code{...}.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @param df Data frame pour cuml global/pays/continent.
#' @param var_count Variable du data frame correspondant a cas/mors.
#' @param col Codes couleurs pays ("#cc4c02", "#d84cff", \code{...})
#' @seealso [ggplotly()] pour plus de détails.
#' @return plotly object.
#' @family plotly tools

cumul_plotly <- function(df, var_count, col){
  df$date <- as.Date(df$date)
  g = ggplot(df, aes(x = date, y = !!var_count, color = region, group = 1,
                     text=paste0(format(date, "%d %B %Y"),
                                 "\n", region, ": ",
                                 !!var_count))) +
    geom_line() +
    geom_point(size = 1, alpha = 0.8) +
    ylab("cumul") +
    theme_bw() +
    scale_colour_manual(values=col) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "",
          plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))


  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))

}


#' @export
#' @title Graphique cumul (nouveaux cas) avec plotly.
#' @description Evolution des nouveaux cas/mors \code{...}.
#'
#' Affichage sous forme de barres geom_bar().
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @param df Data frame pour cuml global/pays/continent.
#' @param var_count Variable du data frame correspondant a cas/mors.
#' @param col Codes couleurs pays ("#cc4c02", "#d84cff", \code{...})
#' @seealso [ggplotly()] pour plus de détails.
#' @return plotly object.
#' @family plotly tools

cumul_new_plotly <- function(df, var_count, col){
  df$date <- as.Date(df$date)
  g = ggplot(df, aes(x = date, y = !!var_count, fill = region,
                     text=paste0(format(date, "%d %B %Y"),
                                 "\n", region, ": ",
                                 !!var_count))) +
    geom_bar(position="stack", stat="identity") +
    ylab("nouveaux") + theme_bw() +
    scale_fill_manual(values=col) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
          # ,
          # plot.margin = margin(5, 12, 5, 5))


  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))

}

#' @export
#' @title Graphique cumul log10 avec plotly.
#' @description Evolution des nouveaux cas/mors \code{...}.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @param df Data frame pour cuml global/pays/continent.
#' @param var_count Variable du data frame correspondant a cas/mors.
#' @param col Codes couleurs pays ("#cc4c02", "#d84cff", \code{...})
#' @seealso [ggplotly()] pour plus de détails.
#' @return plotly object.
#' @family plotly tools

cumul_plotly_log <- function(df, var_count, col){
  df$date <- as.Date(df$date)
  g = ggplot(df, aes(x = date, y = !!var_count, color = region, group = 1,
                     text=paste0(format(date, "%d %B %Y"),
                                 "\n", region, ": ",
                                 !!var_count))) +
    geom_line() +
    geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") +
    theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=col) +
    theme(legend.title = element_blank(),
          legend.position = "",
          plot.title = element_text(size=10))


  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))

}
