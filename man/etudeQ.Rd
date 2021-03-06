% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/etudeQ.r
\name{etudeQ}
\alias{etudeQ}
\title{Create a learnr question}
\usage{
etudeQ(
  prompt = "The question prompt",
  ...,
  id = NULL,
  right_one = NULL,
  inline = FALSE,
  random_answer_order = FALSE,
  allow_retry = TRUE,
  correct = "Right!",
  incorrect = "Sorry.",
  message = NULL,
  post_message = NULL,
  submit_button = "Check answer",
  try_again_button = "Try again",
  allow_multiple_correct = FALSE,
  is_learnr = TRUE
)
}
\description{
This is an interface to learnr::question() to allow pre- and post-processing
of the question object
}
