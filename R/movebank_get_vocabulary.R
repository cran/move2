movebank_simplify_labels <- function(x) {
  gsub("(-|_|\\.|:)", " ", tolower(x))
}

#' Retrieve information from the movebank vocabulary describing the columns
#'
#' @description
#' Retrieve information describing the columns from the 'Movebank Attribute Dictionary'
#'
#'
#' @param labels Either a character vector with the column names to look up or a `move2` object from which the column
#' names will be extracted. Matches are made both based on the preferred label and the alternative label in the vocabulary.
#' If no argument is provided all movebank terms are returned
#' @param xml Either a connection to the movebank vocabulary xml, a path to the vocabulary file or an url where it can
#' be downloaded. The later is the default. By downloading the xml yourself the function will speed up and become
#'  independent of an internet connection being available.
#' @param omit_deprecated If concepts are marked deprecated they are omitted from the set of possible labels to match.
#' @param return_type A character scalar identifying the desired return type, see details for more information on the
#' specific types.
#'
#' @details
#' This function can return data in several formats (see `return_type` argument):
#' * `definition` A named text vector with the description of the term.
#' * `list` A list with all information for each term.
#' * `xml` A xml_node with the definition.
#' * `uri` A link to the full definitions page.
#'
#' In case `labels` matches both a preferred and an alternative label the term with the preferred label is returned.
#'
#' @return A named list of the selected `return_type`, note that if deprecated are not omitted duplicated names can
#'  occur.
#' @export
#'
#' @examples
#' \donttest{
#' ## the names of all terms used in movebank
#' movebank_get_vocabulary() |>
#'   names()
#' ## retrieve one variable
#' movebank_get_vocabulary("gps hdop")
#' ## Count the units used in movebank
#' movebank_get_vocabulary() |>
#'   unlist() |>
#'   grep(pattern = "Units:", value = TRUE) |>
#'   sub(replacement = "", pattern = ".*Units: ") |>
#'   sub(replacement = "", pattern = "; .*") |>
#'   table() |>
#'   sort()
#' ## different return types:
#' movebank_get_vocabulary("light-level", return_type = "definition")
#' movebank_get_vocabulary("light-level", return_type = "xml")
#' movebank_get_vocabulary("light-level", return_type = "uri")
#' movebank_get_vocabulary("light-level", return_type = "list")
#' ## get definitions of all column names of a move2 object, the conversion
#' ## to a list is for better printing
#' data <- mt_read(mt_example())
#' movebank_get_vocabulary(data) |>
#'   as.list()
#' }
movebank_get_vocabulary <-
  function(labels,
           xml = "http://vocab.nerc.ac.uk/collection/MVB/current/",
           omit_deprecated = TRUE,
           return_type = c("definition", "list", "xml", "uri")) {
    check_installed(c("xml2", "curl"), "To parse information from the movebank vocabulary")
    return_type <- rlang::arg_match(return_type)
    if (inherits(xml, "connection")) {
      xml <- xml2::read_xml(xml)
    }
    if (is_scalar_character(xml)) {
      if (file.exists(xml)) {
        xml <- xml2::read_xml(xml)
      } else {
        h <- curl::new_handle()
        curl::handle_setheaders(h, "Accept" = "application/rdf+xml")
        xml <- xml2::read_xml(curl::curl(xml, handle = h))
      }
    }
    if (!missing(labels) && inherits(labels, "move2")) {
      labels <- unique(c(names(labels), names(mt_track_data(labels))))
    }
    assert_that(inherits(xml, "xml_document"))
    select_statement <- sprintf(
      '//rdf:Description[rdf:type[@rdf:resource="http://www.w3.org/2004/02/skos/core#Concept"]%s]',
      ifelse(omit_deprecated, ' and owl:deprecated="false"',
        ""
      )
    )
    all_descriptions <- xml2::xml_find_all(xml, select_statement)
    label_list <- mapply(c,
      prefLabel = lapply(
        all_descriptions,
        xml2::xml_find_all, ".//skos:prefLabel"
      ) |>
        lapply(xml2::xml_text),
      altLabel = lapply(
        all_descriptions,
        xml2::xml_find_all, ".//skos:altLabel"
      ) |>
        lapply(xml2::xml_text),
      SIMPLIFY = FALSE
    ) |>
      lapply(function(x) x[nchar(x) != 0]) |>
      lapply(movebank_simplify_labels)
    if (!missing(labels)) {
      labels_simplified <- movebank_simplify_labels(labels)
      relevant_descriptions <- lapply(
        labels_simplified,
        function(x) {
          matches <- lapply(label_list, "==", x)
          ids <- which(unlist(lapply(matches, any)))
          if (length(ids) == 2L) {
            label_types <- names(unlist(lapply(matches[ids], which)))
            if (sum(label_types == "prefLabel") == 1L) {
              ids <- ids[label_types == "prefLabel"]
            }
          }
          ids
        }
      )
      names(relevant_descriptions) <- labels
    } else {
      relevant_descriptions <- seq_len(length(label_list))
      names(relevant_descriptions) <- unlist(lapply(label_list, head, 1L))
    }
    xml_descriptions <- lapply(relevant_descriptions,
      function(x, i) x[i],
      x = all_descriptions
    )
    l <- unlist(lapply(xml_descriptions, length))

    if (any(l >= 2L)) {
      cli_abort( # nocov start
        "For some terms duplicate vocabulary terms are retrieved. This is an error that needs to be
                investegated, please submit an issue.",
        class = "move2_error_duplicate_vocabulary"
      ) # nocov end
    }
    xml_descriptions <- xml_descriptions[l != 0L]
    xml_descriptions <- lapply(xml_descriptions, "[[", 1L)
    if (anyDuplicated(names(xml_descriptions)) && omit_deprecated) {
      cli_abort( # nocov start
        class = "move2_error_duplicated_vocabulary_entries",
        "There are duplicated entries in the returned vocabulary list, they should not occur if since deprecated values
        are omitted. Please submit an issue to investigate"
      ) # nocov end
    }
    return(switch(return_type,
      list = lapply(xml_descriptions, xml2::as_list),
      uri = unlist(lapply(
        lapply(
          xml_descriptions,
          xml2::xml_find_first, ".//pav:hasCurrentVersion[@rdf:resource]"
        ), xml2::xml_attr, "resource"
      )),
      definition = unlist(lapply(
        xml_descriptions,
        xml2::xml_find_chr, "string(.//skos:definition/child::text())"
      )),
      xml = xml_descriptions
    ))
  }
