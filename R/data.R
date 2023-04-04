#' Alcohol consumption by country
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{Land}{country}
#'   \item{Typ}{type of alcohol}
#'   \item{Portionen}{quantity of portions}
#' }
"alkohol"

#' Alcohol consumption by country
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{Marke}{brand}
#'   \item{PS}{horse power}
#'   \item{Preis}{price}
#'   \item{Alter}{age}
#'   \item{Kategorie}{category}
#'   \item{Kilometer}{mileage in km}
#' }
"kfz"

#' List of words with sentiment value
#'
#' A list of words in a text with their sentiment value based on the Leipzig dictionary \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 36 rows and 4 variables:
#' \describe{
#'   \item{token}{word}
#'   \item{anzahl}{count of word in text}
#'   \item{wert}{sentiment value of word}
#'   \item{kumwert}{cumulated sentiment value of words}
#' }
"leipzig"


#' Used cars in the USA crawled 2016
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 34864 rows and 23 variables:
#' \describe{
#'   \item{DatumCrawled}{date of crawling}
#'   \item{Name}{name}
#'   \item{Verkäufer}{seller}
#'   \item{AngebotsTyp}{type of offer}
#'   \item{PreisChar}{price as character}
#'   \item{Test}{test}
#'   \item{Typ}{type of car}
#'   \item{Jahr}{year of production}
#'   \item{Getriebe}{transmission}
#'   \item{PS}{horsepower}
#'   \item{Modell}{model}
#'   \item{Kilometer}{odometer in km}
#'   \item{Monat}{month of production}
#'   \item{Motor}{motor}
#'   \item{Marke}{brand}
#'   \item{Unfall}{collision}
#'   \item{Datum}{date}
#'   \item{AnzahlBilder}{quantity of pictures}
#'   \item{ZipCode}{postal code}
#'   \item{DatumSichtung}{date}
#'   \item{Alter}{age}
#'   \item{Preis}{price}
#'   \item{Kategorie}{category}
#' }
"autos"

#' German soccer league standing 2021
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 18 rows and 12 variables:
#' \describe{
#'   \item{R.}{rank}
#'   \item{V.}{rank previous week}
#'   \item{Var.3}{not used}
#'   \item{Verein}{name}
#'   \item{Sp.}{number of games played}
#'   \item{S.}{games win}
#'   \item{U.}{games neiter lost nor won}
#'   \item{N.}{games lost}
#'   \item{Tore.}{goals}
#'   \item{TD.}{difference of goals made versus received}
#'   \item{P..}{points}
#'   \item{Quali.}{qualification for champions league}
#' }
"bundesliga"


#' Group of friends with attributes
#'
#' The dataset was created fictitiously.
#'
#' @format A data frame with 5 rows and 5 variables:
#' \describe{
#'   \item{freund}{name of friend}
#'   \item{fb}{facebook friends}
#'   \item{alter}{age}
#'   \item{farbe}{colour favourite}
#'   \item{altersgruppe}{age group}
#' }
"df"

#' Flights from New York airports in 2013
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 336788 rows and 30 variables:
#' \describe{
#'   \item{dest}{destination}
#'   \item{year}{year}
#'   \item{dest_month}{arrival month}
#'   \item{dest_day}{arrival day}
#'   \item{dest_hour}{arrival hour}
#'   \item{origin}{origin}
#'   \item{month}{origin month}
#'   \item{day}{origin day}
#'   \item{hour}{origin hour}
#'   \item{tailnum}{plane tailnumber ID}
#'   \item{carrier}{carrier}
#'   \item{flight}{flight number}
#'   \item{air_time}{air_time}
#'   \item{dep_delay}{departure delay}
#'   \item{arr_delay}{arrival delay}
#'   \item{sched_arr_time}{scheduled arrival time}
#'   \item{origin_alt}{origin altitude}
#'   \item{origin_tzone}{origin time zone}
#'   \item{dest_alt}{destination altitude}
#'   \item{dest_tzone}{destination time zone}
#'   \item{name}{carrier name}
#'   \item{age}{age of plane}
#'   \item{seats}{seats on plane}
#'   \item{origin_precip}{origin precipitation}
#'   \item{origin_visib}{origin visibility}
#'   \item{dest_precip}{destination precipitation}
#'   \item{dest_visib}{destination visibility}
#'   \item{quarter}{quarter of year}
#'   \item{agegroup}{age group of plane}
#'   \item{avg_delay}{average delay}
#' }
"flug"

#' Racing results from formula1 in 2019
#'
#' The dataset was created from data available at \url{https://www.prescient.pro/}.
#'
#' @format A data frame with 420 rows and 9 variables:
#' \describe{
#'   \item{Rennen}{race number}
#'   \item{Ort}{place}
#'   \item{Startposition}{starting order}
#'   \item{Rennwagen}{race car number}
#'   \item{Fahrer}{driver}
#'   \item{Team}{team}
#'   \item{Platzierung}{result}
#'   \item{Punkte}{points}
#'   \item{WRPunkte}{points towards world championship}
#' }
"formel1"

#' Students data from 2020
#'
#' The dataset was created from student data.
#'
#' @format A data frame with 357 rows and 22 variables
#' \describe{
#'   \item{Gruppe}{group}
#'   \item{Geschlecht}{gender}
#'   \item{Geburtsjahr}{year of birth}
#'   \item{Geburtsmonat}{month of birth}
#'   \item{Größe}{height}
#'   \item{Einwohner}{citicens of city of origin}
#'   \item{Distanz}{distance of residence to university}
#'   \item{DistanzEltern}{distance of residence of parents to university}
#'   \item{Buchstaben}{length of first name}
#'   \item{Fußball}{like or dislike of soccer}
#'   \item{Schuhe}{pairs of shoes in posession}
#'   \item{Intelligenz}{intelligence}
#'   \item{Attraktivität}{attractiveness}
#'   \item{Lieblingsfarbe}{preferred colour}
#'   \item{Hobby}{hobby}
#'   \item{IntLeadership}{interest in leadership}
#'   \item{IntMgmt}{interest in management}
#'   \item{HSAnsbach1}{Ansbach first choice}
#'   \item{AlternativeHS}{name of alternative university}
#'   \item{AlternativerStg}{name of alternative program}
#'   \item{DistanzElternhaus}{distance of residence to residence of parents}
#'   \item{Note}{expected grade}
#' }
"studierende"

#' Visualization theme
#'
#' The theme defines the standards for your own organization.
#'
#' @format List with 93 elements
#' \describe{
#'   \item{line}{line}
#'   \item{rect}{rect}
#'   \item{...}{...}
#' }
"theme.own"

#' Visualization theme
#'
#' The theme defines the standards when r is started.
#'
#' @format List with 93 elements
#' \describe{
#'   \item{line}{line}
#'   \item{rect}{rect}
#'   \item{...}{...}
#' }
"theme.standard"

#' Colours for traffic lights
#'
#' The theme defines the colours green, yellow and red.
#'
#' @format Vector with 3 elements with the colour code
#' \describe{
#' }
"colour.own.ampel"

#' Colours for nominal variables - Option 1
#'
#' The theme defines colours for nominal variables.
#'
#' @format Vector with 11 elements with the colour code
#' \describe{
#' }
"colour.own.nomin.1"

#' Colours for nominal variables - Option 2
#'
#' The theme defines colours for nominal variables.
#'
#' @format Vector with 8 elements with the colour code
#' \describe{
#' }
"colour.own.nomin.2"

#' Colours for sequential variables - Option 1
#'
#' The theme defines colours for sequential variables.
#'
#' @format Vector with 9 elements with the colour code
#' \describe{
#' }
"colour.own.seque.1"

#' Colours for sequential variables - Option 2
#'
#' The theme defines colours for sequential variables.
#'
#' @format Vector with 11 elements with the colour code
#' \describe{
#' }
"colour.own.seque.2"

#' Colours for spectral variables - Option 1
#'
#' The theme defines colours for spectral variables.
#'
#' @format Vector with 11 elements with the colour code
#' \describe{
#' }
"colour.own.spect.1"

#' Colours for spectral variables - Option 2
#'
#' The theme defines colours for spectral variables.
#'
#' @format Vector with 11 elements with the colour code
#' \describe{
#' }
"colour.own.spect.2"

#' Average student grades by gender and age
#'
#' The dataset was created was created fictitiously.
#'
#' @format A data frame with 49999 rows and 5 variables
#' \describe{
#'   \item{Note}{grade}
#'   \item{Geschlecht}{gender}
#'   \item{Alter}{age}
#' }
"durchschnittsnoten"
