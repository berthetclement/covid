

#' @export
#' @title Ajouts d'indicateurs a date.
#' @description Les nouveaux cas, deces, guerisons du jour.
#'
#' @importFrom dplyr mutate lag
#' @importFrom magrittr "%>%"
#' @param global_data Data frame tidy global (cas/morts/gueris).
#' @return Data.frame au format tidy.
#' @family covid stats


add_stats <- function(global_data){
  # date au format 2020/12/20 en caractere pour jointures futures
  global_data$date <- format(as.Date(global_data$date,"%m/%d/%y"))

  # tri par date pour comptage new cases/new deaths/new recover
  global_data <- global_data[order(global_data$Country, global_data$date),]

  tt <- global_data %>% mutate(new_cases = lag(global_data$cases,1),
                               new_deaths = lag(global_data$deaths,1),
                               new_recovered = lag(global_data$recovered,1)
  )

  # init (lag NA)
  tt$new_cases[1] <- tt$cases[1]
  tt$new_deaths[1] <- tt$deaths[1]
  tt$new_recovered[1] <- tt$recovered[1]

  # calcul
  tt$new_cases <- ifelse(tt$cases - tt$new_cases>=0, tt$cases - tt$new_cases, 0)
  tt$new_deaths <- ifelse(tt$deaths - tt$new_deaths>=0, tt$deaths - tt$new_deaths, 0)
  tt$new_recovered <- ifelse(tt$recovered - tt$new_recovered>=0, tt$recovered - tt$new_recovered, 0)

  tt
}




#' @export
#' @title cas/morts/gueris pour 100k de la population.
#' @description Ces stats serviront aussi au font de carte.
#'
#' @importFrom dplyr mutate lag
#' @param spatial_df Data frame enrichi des valeurs de population par pays.
#' @return Data.frame.
#' @family covid stats

add_map_stats <- function(spatial_df){
  spatial_df$per100k = as.numeric(format(round(spatial_df$cases/(spatial_df$population/100000),1), nsmall = 1))
  spatial_df$newper100k = round(spatial_df$new_cases/(spatial_df$population/100000),1)

  spatial_df$deathsper100k <- as.numeric(format(round(spatial_df$deaths/(spatial_df$population/100000),1), nsmall = 1))
  spatial_df$newdeathsper100k <- round(spatial_df$new_deaths/(spatial_df$population/100000),1)

  spatial_df
}

globalVariables(c("cases", "deaths", "new_cases", "date", "recovered", "deathsper100k", "per100k"))

#' @export
#' @title Stats monde.
#' @description Aggregation des donnees.
#'
#' @importFrom dplyr select group_by summarise_each funs
#' @param global_df Data frame des donnees globales (cas/morts/nouveaux cas).
#' @return Data.frame aggrege par date.
#' @family covid stats

global_stats <- function(global_df){
  global_df %>% select(date, cases, deaths, new_cases) %>%
    group_by(date) %>% summarise_each(funs(sum))
}



# comptage des nouveaux cas depuis une date de départ

#' @export
#' @title Stats recalculées.
#' @description Re comptage des cas comme nouveaux cas.
#'
#' @importFrom dplyr select group_by mutate slice
#' @param global_data Data frame des donnees globales (cas/morts/nouveaux cas).
#' @return Data.frame.
#' @family covid stats

add_stats2 <- function(global_data){

  # tri par date pour comptage new cases/new deaths/new recover
  global_data <- global_data[order(global_data$Country, global_data$date),]

  tt <- global_data %>% mutate(new_cases = lag(global_data$cases,1),
                               new_deaths = lag(global_data$deaths,1),
                               new_recovered = lag(global_data$recovered,1)
  )

  # init (lag NA)
  tt$new_cases[1] <- tt$cases[1]
  tt$new_deaths[1] <- tt$deaths[1]
  tt$new_recovered[1] <- tt$recovered[1]

  # calcul
  tt$new_cases <- tt$cases - tt$new_cases
  tt$new_deaths <- tt$deaths - tt$new_deaths
  tt$new_recovered <- tt$recovered - tt$new_recovered

  # selection des valeurs non negatives
  group_no_neg <- tt %>% group_by(Country) %>% slice(-1)

  # maj des nouveaux cas
  group_neg <- tt %>% group_by(Country) %>% slice(1) %>%
    mutate(
      new_cases = cases,
      new_deaths = deaths,
      new_recovered = recovered
    )
  #
  ok = rbind(group_no_neg, group_neg)
  ok
}


#' @export
#' @title Tableau dynamique
#' @description Tableau avec presentation [gt()]
#'
#' @importFrom gt gt fmt_number tab_header md tab_source_note
#' @param df Data frame pour onglet "pays".
#' @param niveau Ex : "pays", "continent"
#' @return Data.frame.
#' @family covid stats

do_gt_table <- function(df, niveau){
  df %>%
    gt() %>%
    fmt_number(columns = vars(cases, deaths), decimals = 0) %>%
    fmt_number(columns = vars(per100k, deathsper100k), decimals = 1) %>%
    tab_header(
      title = md("Cumul a partir de la *\"date de depart\"*"),
      subtitle = md("Selection par ***Niveau / Continent/Pays***")
    ) %>%
    tab_source_note(md(paste0("*Tableau dynamique des donnees* ", "***",niveau, "***")))
}

##
# onglet regions/departements
##

# recalcul pour somme date de depart global france

#' @export
#' @title Stats recalculées pour l'onglet regions/departements.
#' @description Re comptage des cas (cumul) comme nouveaux cas (à date) pour la France au global.
#'
#' @importFrom dplyr select group_by mutate slice
#' @param df_france_global Data frame des donnees globales (cas/morts/gueris).
#' @return Data.frame.
#' @family covid stats

add_stats_global <- function(df_france_global){

  # tri par date pour comptage new cases/new deaths/new recover
  df_france_global <- df_france_global[order(df_france_global$date),]

  tt <- df_france_global %>% mutate(new_cases = lag(df_france_global$cases,1),
                                    new_deaths = lag(df_france_global$deaths,1),
                                    new_recovered = lag(df_france_global$recovered,1)
  )

  # init (lag NA)
  tt$new_cases[1] <- tt$cases[1]
  tt$new_deaths[1] <- tt$deaths[1]
  tt$new_recovered[1] <- tt$recovered[1]

  # calcul
  tt$new_cases <- tt$cases - tt$new_cases
  tt$new_deaths <- tt$deaths - tt$new_deaths
  tt$new_recovered <- tt$recovered - tt$new_recovered

  # recomptage de la premiere ligne
  tt$new_cases[1] <- tt$cases[1]
  tt$new_deaths[1] <- tt$deaths[1]
  tt$new_recovered[1] <- tt$recovered[1]

  tt
}



# recalcul pour somme date de depart region

#' @export
#' @title Stats recalculées pour l'onglet regions/departements.
#' @description Re comptage des cas (cumul) comme nouveaux cas (à date) pour les regions.
#'
#' @importFrom dplyr select group_by mutate slice
#' @param global_data Data frame des donnees globales (cas/morts/gueris).
#' @return Data.frame.
#' @family covid stats


add_stats_reg <- function(global_data){

  # tri par date pour comptage new cases/new deaths/new recover
  global_data <- global_data[order(global_data$region, global_data$date),]

  tt <- global_data %>% mutate(new_cases = lag(global_data$cases,1),
                               new_deaths = lag(global_data$deaths,1),
                               new_recovered = lag(global_data$recovered,1)
  )

  # init (lag NA)
  tt$new_cases[1] <- tt$cases[1]
  tt$new_deaths[1] <- tt$deaths[1]
  tt$new_recovered[1] <- tt$recovered[1]

  # calcul
  tt$new_cases <- tt$cases - tt$new_cases
  tt$new_deaths <- tt$deaths - tt$new_deaths
  tt$new_recovered <- tt$recovered - tt$new_recovered

  # selection des valeurs non negatives
  group_no_neg <- tt %>% group_by(region) %>% slice(-1)


  # maj des nouveaux cas
  group_neg <- tt %>% group_by(region) %>% slice(1) %>%
    mutate(
      new_cases = cases,
      new_deaths = deaths,
      new_recovered = recovered
    )
  #
  ok = rbind(group_no_neg, group_neg)
  ok
}

