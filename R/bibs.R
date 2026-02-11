

#' @title rpart bibs
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name rpart_bib
#' @export
.breiman84 <- \() {
  bibentry(
    bibtype = 'Book', key = 'Breiman84',
    author = c(
      person(given = 'Leo', family = 'Breiman'), 
      person(given = 'Jerome', family = 'Friedman'), 
      person(given = c('Richard', 'A.'), family = 'Olshen'), 
      person(given = c('Charles', 'J.'), family = 'Stone')
    ),
    title = 'Classification and Regression Trees',
    year = '1984',
    edition = '1',
    publisher = 'Chapman and Hall/CRC', address = 'New York',
    doi = '10.1201/9781315139470'
  )
}