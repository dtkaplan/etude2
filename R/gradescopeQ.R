#' Render multiple choice questions for GradeScope online assignment
#'
#'
#' @examples
#' foo <- gradescopeQ("What is $$\\int x^2 dx \\mbox{?}$$",
#'            "+an indefinite integral+" = "That's all I wanted.",
#'            "Something I don't understand" = "Keep studying",
#'            "a definite integral" = "No, there are no limits of integration as in $\\int_a^b$."
#'            )

#' @export
gradescopeQ <- function(prompt="The question prompt", ...,
                        id=NULL, # not used
                        right_one=NULL,
                        inline=FALSE, random_answer_order=FALSE, # not used
                        allow_retry=TRUE, # not used
                        correct = "Right!",
                        incorrect = "Sorry.",
                        message = NULL, # message after any response #not used
                        post_message = NULL, # message after correct response
                        submit_button = "Check answer", # not used
                        try_again_button = "Try again", # not used
                        allow_multiple_correct = FALSE) {

  id <- master_id()
  answer_table <- etude2:::dots_to_answers(..., right_one = right_one,
                                           allow_multiple_correct = allow_multiple_correct)

  out <- fix_dollar_signs(prompt)

  answers <- paste0("(", ifelse(answer_table$correct, "x", " "), ")  ",
                    fix_dollar_signs(answer_table$item), collapse="\n")

  feedback <- paste0("[[",
                    paste(answer_table$feedback[answer_table$correct],
                          collapse = "\n"),
                    "]]\n")
  if (feedback == "[[]]") feedback <- NULL

  total <- paste(out, answers, feedback, sep="\n\n")

  knitr::asis_output(paste0("<pre>", "[Question ", id, "]  ", total, "\n</pre>\n"))
  }

fix_dollar_signs <- function(str) {
  str <- gsub("\\${2}", "☹☹☹︎", str)
  str <- gsub("\\${1}", "☹☹", str)
  gsub("☹", "$", str)
}

