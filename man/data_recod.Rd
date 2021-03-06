% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_tools.R
\name{data_recod}
\alias{data_recod}
\title{Recodage noms pays.}
\usage{
data_recod(input_ref, data_to_reco)
}
\arguments{
\item{input_ref}{Fichier au format .csv avec les colonnes "old" et "new".}

\item{data_to_reco}{Data frame des donnees a modifier (Country).}
}
\value{
Data.frame mis a jour ainsi que la suppression des "espaces" dans les noms de pays.
}
\description{
La fonction utilise un fichier d'entree .csv (une colonne "old" et "new").

Identifie a partir de la colonne "old" et applique la modification de la colonne "new".
}
\seealso{
Other data tools: 
\code{\link{data_covid}()},
\code{\link{data_recod_dom}()},
\code{\link{make_tidy_df}()}
}
\concept{data tools}
