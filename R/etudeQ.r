#' Create a learnr question
#'
#' This is an interface to learnr::question() to allow pre- and post-processing
#' of the question object
#'
#'
#' @importFrom glue glue
#' @importFrom magrittr `%>%`
#'
#' @export
etudeQ <- function(prompt="The question prompt", ..., id=NULL,
                   right_one=NULL,
                   inline=FALSE, random_answer_order=FALSE,
                   allow_retry=TRUE,
                   correct = "Right!",
                   incorrect = "Sorry.",
                   message = NULL, # message after any response
                   post_message = NULL, # message after correct response
                   submit_button = "Check answer",
                   try_again_button = "Try again",
                   allow_multiple_correct = FALSE,
                   is_learnr = TRUE) {
  # if (is.null(id)) stop("etudeQ must be given <id> argument")
  # id <- paste0(master_id(),"-", id)
  id <- master_id()

  submit_button <- glue("{submit_button} (ID: {id})")
  try_again_button <- glue("{try_again_button} (ID: {id})")
  answer_table <- etude2:::dots_to_answers(..., right_one = right_one,
                  allow_multiple_correct = allow_multiple_correct)


  # for learnr::question
  learnr_answers <- list()

  for (k in 1:nrow(answer_table)) {
    learnr_answers[[k]] <-
      learnr::answer(answer_table$item[k],
                     answer_table$correct[k],
                     answer_table$feedback[k])
  }

  arguments <- c(text=prompt, learnr_answers,
                    correct = correct, incorrect=incorrect,
                    allow_retry = allow_retry,
                    submit_button = submit_button,
                    try_again_button = try_again_button,
                    message = message,
                    post_message = post_message,
                    random_answer_order = random_answer_order)
  result <- do.call(learnr::question, arguments)
  result$options <- list(id = id)
  result$label <- paste0(clean_id(id), "-", result$label)
  etude2::master_id_list(result$label)

  # for text output
  if (!is_learnr)
    result <- paste(capture.output(result), collapse="\n")

  result
}

# return a data frame with one row for each element of ...
dots_to_answers <- function(..., right_one = "",
                            allow_multiple_correct = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    if (is.list(dots[[1]])) choices <- dots[[1]]
    else if (is.vector(dots[[1]])) {
    # it's a character or numerical vector
      choices <- as.list(rep("", length(dots[[1]])))
      names(choices) <- dots[[1]]
    }
  } else {
    choices <- dots
  }

  display <- names(choices)
  no_feedback <- if (is.null(display)) {
    # no names whatsoever
    display <- unlist(choices)
    choices <- as.list(rep("", length(display)))
    names(choices) <- display
    NULL
  } else which(display == "")
  # if it's not named, use the value as the name
  if (length(no_feedback) > 0) {
    display[no_feedback] <- choices[no_feedback]
    choices[no_feedback] <- "" # blank feedback
  }
  names(choices) <- display # update the names
  feedback <- unlist(choices)
  names(feedback) <- NULL
  # store as a data frame
  answers <- tibble(item=names(choices), feedback=feedback)
  if (!is.null(right_one)) answers$correct <- answers$item %in% right_one
  else answers$correct <- grepl("^\\+.*\\+$", answers$item)
  answers$item[answers$correct] <-
    gsub("^\\+(.*)\\+$", "\\1", answers$item[answers$correct])
  if (sum(answers$correct) == 0)
    stop("Must provide one correct answer.")
  if (sum(answers$correct) > 1 && !allow_multiple_correct)
    stop("Must give only one correct answer.")

  answers
}


#' @export
etudeEssay <- function(prompt = "Give a prompt, please.", id=NULL,
                       nrows = 5, placeholder="Your essay here ...") {
  # if (is.null(id)) stop("etudeEssay must be given <id> argument")
  # id <- paste0(master_id(),"-", id)
  id <- master_id()
  result <- question_text(prompt, answer("nice", correct=TRUE),
                          allow_retry=TRUE, incorrect=NULL,
                          submit_button = glue("Store your response (ID:{id})"),
                          try_again_button = glue("Edit response (ID:{id})"),
                          placeholder = placeholder,
                          options = list(id = id, nrows = nrows, essay=TRUE))

  result$label <- paste0(clean_id(id), "-", result$label)
  etude2::master_id_list(result$label)

  result
  }

# Get rid of characters that aren't valid in a chunk label
clean_id <- function(id) {
  paste0(gsub("[^0-9a-zA-Z_-]", "", id))
}

etude_store <- new.env()
etude_store$id_count <- 0
etude_store$master_id_list <- character(0)
etude_store$document_name <- ""

#' Set a master ID for the whole document
#' when questions are created with etude2 they will
#' get assigned a unique ID within the document
#' @export
master_id <- function(id) {
  if (missing(id)) {
    etude_store$id_count <- etude_store$id_count + 1
    res <-
      if ("master_id" %in% names(etude_store)) {
        paste0(etude_store$master_id, "-", etude_store$id_count)
      } else paste0("tmp", "-", etude_store$id_count)

    return(res)
  } else etude_store$master_id <- id
}
#' Get the list of question items in the current document
#' @export
master_id_list <- function(str) {
  if (missing(str)) {
       etude_store$master_id_list
  } else {
    etude_store$master_id_list <-
      c(etude_store$master_id_list, str)
  }
}
#' @export
etude_document_name <- function(str) {
  if (missing(str)) etude_store$document_name
  else etude_store$document_name <- str

}

