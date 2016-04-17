
#' @title Praise Users
#' @name praise
#' @description Build friendly R packages that
#' praise their users if they have done something
#' good, or they just need it to feel better.
#'
#' @docType package
#' @aliases praise praise-package

NULL

`%||%` <- function(l, r) if (is.null(l)) r else l

#' Languages spoken by praise
#'
#' Currently: \itemize{
#'   \item \sQuote{eng} English.
#'   \item \sQuote{hun} Hungarian.
#' }
#'
#' If the language is not detected correctly, set the
#' the \code{LANGUAGE} environment variable to the three
#' letter language code.
#'
#' @export

praise_languages <- c("eng", "hun")

get_language <- function() {
  parse_language(Sys.getenv("LANGUAGE")) %||%
  parse_language(Sys.getenv("LANG")) %||%
  parse_language(Sys.getlocale()) %||%
  "eng"
}

parse_language <- function(x) {
  if (x %in% praise_languages) {
    x

  } else if (substr(x, 1, 3) %in% praise_languages) {
    substr(x, 1, 3)

  } else if (substr(x, 1, 2) %in% substr(praise_languages, 1, 2)) {
    praise_languages[match(substr(x, 1, 2),
                           substr(praise_languages, 1, 2))]

  } else {
    NULL
  }
}

#' Parts of speech for praising
#'
#' @format
#' Named list of character vertors. List entries: \describe{
#'   \item{adjective}{Words and phrases to be used as positive adjectives.
#'     Most of them are from \url{https://github.com/sindresorhus/superb}.}
#'   \item{adverb}{Adverbs.}
#'   \item{adverb_manner}{Adverbs of manner, with positive meanings.}
#'   \item{created}{Synonyms of \sQuote{create} in paste tense.}
#'   \item{creating}{Synonyms of \sQuote{create}, in present participle
#'     form.}
#'   \item{exclamation}{Positive exclamations.}
#'   \item{rpackage}{Synonyms for the term \sQuote{R package}.}
#' }
#'
#' @include english-adjective.R english-adverb.R english-exclamation.R
#' @include english-rpackage.R english-verb.R smiley.R
#' @export

praise_parts <- list(
  adjective = adjective,
  adverb = adverb,
  adverb_manner = adverb_manner,
  created = created,
  creating = creating,
  exclamation = exclamation,
  rpackage = rpackage,
  smiley = smiley
)


#' Randomized praise based on a template
#'
#' @details
#' Replace parts of the template with random words from the praise
#' word lists. See examples below.
#'
#' @param template Character scalar, the template string.
#' @export
#' @examples
#' praise()
#'
#' ## Capitalization
#' praise("${Exclamation}! This ${rpackage} is ${adjective}!")
#'
#' ## All upper case
#' praise("${EXCLAMATION}! You have done this ${adverb_manner}!")

praise <- function(template = "You are ${adjective}!") {
  while (is_template(template)) {
    template <- replace_one_template(template)
  }
  template
}


template_pattern <- "\\$\\{([^\\}]+)\\}"


is_template <- function(x) grepl(template_pattern, x)


replace_one_template <- function(template) {
  match <- regexpr(template_pattern, template, perl = TRUE)

  template1 <- substring(
    template,
    match,
    match + attr(match, "match.length") - 1L
  )

  part <- substring(
    template,
    attr(match, "capture.start"),
    attr(match, "capture.start") + attr(match, "capture.length") - 1L
  )

  match_case_sub(
    template1,
    part,
    sample(praise_parts[[tolower(part)]], 1),
    template
  )
}


match_case_sub <- function(pattern, part, replacement, text) {
  if (toupper(part) == part) {
    replacement <- toupper(replacement)
  } else if (capitalize(part) == part) {
    replacement <- capitalize(replacement)
  }

  sub(pattern, replacement, text, fixed = TRUE)
}

capitalize <- function(x) {
  paste0(
    toupper(substring(x, 1, 1)),
    substring(x, 2)
  )
}
