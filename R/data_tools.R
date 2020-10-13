# imports depuis repos github : https://github.com/CSSEGISandData/COVID-19




#' @export
#' @title Import depuis repos Github.
#' @description Donnés COVID au format ".csv".
#'
#' @importFrom data.table fread
#' @param data_github Lien html vers fichier .csv (repos Github).
#' @param ... Parametres [data.table::fread()]
#' @seealso [sources](https://github.com/CSSEGISandData/COVID-19) les sources.
#' @return Objet de type data.table data.frame.
#' @family data tools

data_covid <- function(data_github, ...){
  data.table::fread(data_github, ...)
}




#' @export
#' @title Recodage noms pays.
#' @description La fonction utilise un fichier d'entree .csv (une colonne "old" et "new").
#'
#'Identifie a partir de la colonne "old" et applique la modification de la colonne "new".
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @param input_ref Fichier au format .csv avec les colonnes "old" et "new".
#' @param data_to_reco Data frame des donnees a modifier (Country).
#' @return Data.frame mis a jour ainsi que la suppression des "espaces" dans les noms de pays.
#' @family data tools

data_recod <- function(input_ref, data_to_reco){
  names(data_to_reco)[1:2] = c("Province", "Country")

  reco <- fread(input_ref) # local (data prealable)

  old_names <- reco$old
  has_match <- data_to_reco$Country %in% old_names
  existing_match <- match(data_to_reco$Country[has_match], old_names)
  data_to_reco$Country[has_match] <- reco$new[existing_match]

  data_to_reco$Country = data_to_reco$Country %>% str_replace_all(" ", "")
  data_to_reco

}



#' @export
#' @title Recodage noms pays.
#' @description La fonction utilise un fichier d'entree .csv (une colonne "old" et "new").
#'
#'Cette fonction est adaptee pour les donnees (opencovid-19 fr)
#'Identifie a partir de la colonne "old" et applique la modification de la colonne "new".
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @param input_ref Fichier au format .csv avec les colonnes "old" et "new".
#' @param data_to_reco Data frame des donnees a modifier (Country).
#' @return Data.frame mis a jour pour la France et des DOM TOM.
#' @family data tools

data_recod_dom <- function(input_ref, data_to_reco){
  reco <- fread(input_ref) # local (data prealable)

  old_names <- reco$old
  has_match <- data_to_reco$region %in% old_names
  existing_match <- match(data_to_reco$region[has_match], old_names)
  data_to_reco$region[has_match] <- reco$new[existing_match]

  data_to_reco
}


# declaration variables globales
globalVariables(c("Province", "Lat", "Long", "Country"))

#' @export
#' @title Structuration des donnees.
#' @description Mise au format "tidy".
#'
#' L'objectif est de faciliter les jointures.
#' @importFrom dplyr select group_by summarise_each funs
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @param data Objet de type data frame (time series cases/deaths/recovered).
#' @return Data.frame au format tidy.
#' @family data tools


make_tidy_df <- function(data){
  data %>%
    select(-c(Province, Lat, Long)) %>%
    group_by(Country) %>%
    summarise_each(funs(sum)) %>%
    pivot_longer(-c(Country), names_to = "date", values_to = "count")
}















