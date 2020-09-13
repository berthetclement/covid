


#' @export
#' @title Map covid-19 monde vierge.
#' @description Une base de map reutilisable.
#'
#' Un fond de carte est applique sur l'indicateur morts/population/100000.
#' @import leaflet
#' @importFrom htmltools HTML
#' @param data_day Data frame des donnees a date.
#' @param map_param Objet de type "SpatialPolygonsDataFrame".
#' @param pal_param Parametre contenant une fonction de palette de couleurs.
#' @return Objet de type leaflet.
#' @seealso [leaflet()].
#' @family covid map

base_map_global <- function(data_day, map_param, pal_param){

  leaflet(map_param) %>%
    addTiles() %>%
    addLayersControl(
      position = "bottomright",
      overlayGroups = c("2020-COVID (cas cumules)", "2020-COVID (nouveaux cas)"), # case a cocher sur map
      options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("2020-COVID (nouveaux cas)"))  %>%
    addProviderTiles(providers$CartoDB.Positron) %>% # un type de carte (affichage)
    fitBounds(~-100,-50,~80,80) %>%
    addLegend("bottomright", pal = pal_param, values = data_day$deathsper100k, # les intervalles (0-50-100)
              title = "<small>Morts pour 100,000 (cumul)</small>")
}















